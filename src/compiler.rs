//! Module for Lua compilers

use std::ffi::OsStr;
use std::io::{Cursor, Read};
use std::process::{Command, Stdio};
use std::io;
use crate::error::CompileError;
use crate::types::value::function::Prototype;
use crate::bytecode;
use std::fs::File;
use std::path::PathBuf;
use std::rc::Rc;
use crate::bytecode::loader::Loader;
use crate::types::value::string::LuaString;

/// Type alias for default compiler
pub type DefaultCompiler = PipedLuaC;

/// Trait for Lua compilers
pub trait LuaCompiler: Sized {
    /// Result type for compiler, compiled bytecode can be retrieved by reading from this value
    type Output: Read;

    /// Compiles script and returns bytecode
    ///
    /// # Arguments
    ///
    /// * `reader`: Input source for script
    ///
    /// returns: Result<Self::Output, CompileError>
    fn compile<R: Read>(reader: &mut R) -> Result<Self::Output, CompileError>;

    /// Compiles script, loads bytecode, and returns loaded prototype
    ///
    /// # Arguments
    ///
    /// * `reader`: Input source for script
    /// * `name_override`: Name override for script, shown in errors and debug info
    /// * `memory_limit`: Limit on amount of bytes loader may read, see [`crate::bytecode::ReadLimit`]
    ///
    /// returns: Result<Rc<Prototype>, CompileError>
    fn compile_and_load<R: Read>(reader: &mut R, name_override: Option<LuaString>, memory_limit: usize) -> Result<Rc<Prototype>, CompileError> {
        let mut reader = Self::compile(reader)?;
        bytecode::loader::LE64Loader::load_chunk(&mut reader, name_override, memory_limit).map_err(CompileError::from)   // TODO: Selectable loader
    }
}

/// Compiler that calls LuaC over CLI, writing to temporary "luac.in" and "luac.out" files, returns a file handle to read compiled bytecode from
pub struct LuaC {}
impl LuaCompiler for LuaC {
    type Output = File;

    fn compile<R: Read>(reader: &mut R) -> Result<Self::Output, CompileError> {
        let temp_dir = std::env::temp_dir();
        let temp_in = [temp_dir.as_os_str(), OsStr::new("luac.in")].iter().collect::<PathBuf>();
        let temp_out = [temp_dir.as_os_str(), OsStr::new("luac.out")].iter().collect::<PathBuf>();

        let mut in_file = File::create(&temp_in).map_err(|err|CompileError::CompileFailed(format!("{}", err)))?;
        io::copy(reader, &mut in_file).map_err(|err|CompileError::CompileFailed(format!("{}", err)))?;

        let process = Command::new("luac")
            .arg("-o").arg(&temp_out).arg(temp_in)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let output = process.wait_with_output()?;
        if !output.status.success() {
            let code = output.status.code();
            let message = String::from_utf8_lossy(&output.stderr);
            return if let Some(code) = code {
                Err(CompileError::CompileFailed(format!("LuaC compilation failed with error code {} and message: {}", code, message.trim())))
            } else {
                Err(CompileError::CompileFailed(format!("LuaC compilation failed with error message: {}", message.trim())))
            };
        }

        let out_file = File::open(&temp_out).map_err(|err|CompileError::CompileFailed(format!("{}", err)))?;
        Ok(out_file)
    }
}


/// Compiler that calls LuaC over CLI, using STDIO to pipe script in and bytecode out, returns Vec cursor to bytecode
pub struct PipedLuaC {}
impl LuaCompiler for PipedLuaC {
    type Output = Cursor<Vec<u8>>;

    fn compile<R: Read>(reader: &mut R) -> Result<Self::Output, CompileError> {
        let mut process = Command::new("luac")
            .arg("-o").arg("-").arg("-")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        if let Some(stdin) = &mut process.stdin {
            match io::copy(reader, stdin) {
                Ok(_) => {}
                Err(_err) => {} // Maybe log this somewhere? In practice the process output should provide more detail
            };
        }

        let output = process.wait_with_output()?;
        if !output.status.success() {
            let code = output.status.code();
            let message = String::from_utf8_lossy(&output.stderr);
            return if let Some(code) = code {
                Err(CompileError::CompileFailed(format!("LuaC compilation failed with error code {} and message:\n{}", code, message)))
            } else {
                Err(CompileError::CompileFailed(format!("LuaC compilation failed with error message:\n{}", message)))
            };
        }

        let mut script_buffer: Vec<u8>;
        if cfg!(target_os = "windows") {    // Stupid hack to remove the LF -> CRLF upgrading windows does for SOME REASON (一︿一)
            let mut iter = output.stdout.into_iter();
            let mut prev = iter.next().unwrap();
            script_buffer = iter.filter_map(|b| {
                if b == 0xA && prev == 0xD {
                    prev = b;
                    None
                } else {
                    let ret = Some(prev);
                    prev = b;
                    ret
                }
            }).collect();
            script_buffer.push(prev);
        } else {
            script_buffer = output.stdout;
        }
        Ok(Cursor::new(script_buffer))
    }
}