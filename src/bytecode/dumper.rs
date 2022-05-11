//! Module for bytecode dumping
//!
//! Provided implementations support only Lua 5.4 bytecode

use std::io::Write;
use std::{io, mem};
use crate::constants;
use crate::constants::types::{HOST_INT, LUA_INT, LUA_FLOAT, LUA_INSTRUCTION};
use crate::types::value::string::LuaString;
use crate::types::value::LuaValue;
use crate::types::value::number::LuaNumber;
use crate::types::upvalue::UpvalueDesc;
use crate::types::value::function::{LocalVariableInfo, Prototype};
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

/// Error type for bytecode dumping
#[derive(Debug)]
pub enum DumpError {
    /// Values in this chunk cannot be represented by this dumper. A dumper for a larger (32bit -> 64bit) architecture may succeed.
    CannotRepresent,

    /// The dumped function contained undumpable constants (Userdata, Function, Thread or Table)
    CannotDumpConstant,

    /// An IO error occurred during dumping
    IO(io::Error),
}

impl DumpError {
    /// Utility function that provides a lua value representation of this error
    pub fn lua_message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl std::error::Error for DumpError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        if let DumpError::IO(err) = self {
            Some(err)
        } else {
            None
        }
    }
}

impl From<io::Error> for DumpError {
    fn from(error: io::Error) -> Self {
        DumpError::IO(error)
    }
}

impl Display for DumpError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DumpError::CannotRepresent => write!(f, "dump error: Cannot represent chunk in selected dump format"),
            DumpError::CannotDumpConstant => write!(f, "dump error: Chunk contains undumpable constant"),
            DumpError::IO(error) => <io::Error as Display>::fmt(error, f)
        }
    }
}

/// Trait for bytecode dumpers
///
/// Primary use is through [`Dumper::dump_chunk`], which produces a full bytecode file
///
/// `write_*` functions are used internally by dump_chunk, their (non-default) behaviour in any implementation is subject to change and must not be relied on.
/// Accordingly, implementation of this trait only requires `dump_chunk` to be functioning.
pub trait Dumper {
    /// Binary signature for Lua bytecode; First few bytes of a dumped chunk. Defaults to [`constants::LUA_SIGNATURE`] ("&lt;esc&gt;Lua")
    const LUA_SIGNATURE: &'static [u8] = constants::LUA_SIGNATURE;
    /// Lua version set in header. Defaults to [`constants::LUA_VERSION`] (0x54)
    const LUA_VERSION: &'static [u8] = &[constants::LUA_VERSION];
    /// Lua format set in header. Defaults to [`constants::LUA_FORMAT`] (0x00)
    const LUA_FORMAT: &'static [u8] = &[constants::LUA_FORMAT];
    /// Lua conversion test data. Defaults to [`constants::LUA_CONV_DATA`] ("\x19\x93\r\n\x1a\n")
    const LUA_CONV_DATA: &'static [u8] = constants::LUA_CONV_DATA;
    /// Lua system parameters. 3 bytes containing the size of LUA_INSTRUCTION, LUA_INT and LUA_FLOAT
    const LUA_SYSTEM_PARAMETER: &'static [u8];
    /// Lua conversion test integer. Defaults to [`constants::LUA_CHECK_INTEGER`] (0x5678)
    const LUA_CHECK_INTEGER: LUA_INT = constants::LUA_CHECK_INTEGER;
    /// Lua conversion test float. Defaults to [`constants::LUA_CHECK_FLOAT`] (370.5)
    const LUA_CHECK_FLOAT: LUA_FLOAT = constants::LUA_CHECK_FLOAT;


    /// Writes a single usize
    ///
    /// A default implementation is provided as size values have a dynamic length in bytecode
    ///
    /// # Arguments
    ///
    /// * `size`: Size to write
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_host_usize<W: Write>(size: usize, dest: &mut W) -> Result<(), DumpError> {
        let bits = (std::mem::size_of::<usize>() * 8) - size.leading_zeros() as usize; // The math in this function is safe as neither function will result in numbers > usize::MAX

        let mut num_bytes = (bits + 6) / 7;
        if num_bytes == 0 { num_bytes += 1; }
        for i in (0..num_bytes).rev() {
            if i != 0 {
                dest.write_all(&[((size >> (i * 7)) & 0x7F) as u8])?;
            } else {
                dest.write_all(&[((size >> (i * 7)) & 0x7F) as u8 | 0x80u8])?;
            }
        }
        Ok(())
    }

    /// Writes a single 'host int'; Usually 32-bit. (C's `int` type)
    ///
    /// As implementation varies per architecture, no default implementation is provided
    ///
    /// # Arguments
    ///
    /// * `int`: Integer to write
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_host_int<W: Write>(int: HOST_INT, dest: &mut W) -> Result<(), DumpError>;

    /// Writes a single Lua integer ([`LUA_INT`])
    ///
    /// As implementation varies per architecture, no default implementation is provided
    ///
    /// # Arguments
    ///
    /// * `int`: Integer to write
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_lua_int<W: Write>(int: LUA_INT, dest: &mut W) -> Result<(), DumpError>;

    /// Writes a single Lua float ([`LUA_FLOAT`])
    ///
    /// As implementation varies per architecture, no default implementation is provided
    ///
    /// # Arguments
    ///
    /// * `float`: Float to write
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_lua_float<W: Write>(int: LUA_FLOAT, dest: &mut W) -> Result<(), DumpError>;

    /// Writes a single Lua instruction ([`LUA_INSTRUCTION`])
    ///
    /// As implementation varies per architecture, no default implementation is provided
    ///
    /// # Arguments
    ///
    /// * `instruction`: Instruction to write
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_lua_instruction<W: Write>(instruction: LUA_INSTRUCTION, dest: &mut W) -> Result<(), DumpError>;

    /// Writes a single byte, convenience wrapper around Writer
    ///
    /// # Arguments
    ///
    /// * `byte`: Byte to write
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_byte<W: Write>(byte: u8, dest: &mut W) -> Result<(), DumpError> {
        Ok(dest.write_all(&[byte])?)
    }

    /// Writes a single boolean
    ///
    /// # Arguments
    ///
    /// * `boolean`: bool to write
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_bool<W: Write>(boolean: bool, dest: &mut W) -> Result<(), DumpError> {
        if boolean {
            Self::write_byte(1, dest)
        } else {
            Self::write_byte(0, dest)
        }
    }

    /// Writes a single 'nullable' [`LuaString`]
    ///
    /// # Arguments
    ///
    /// * `string`: string to write `None` represents NULL
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_string<W: Write>(string: &Option<LuaString>, dest: &mut W) -> Result<(), DumpError> {
        if let Some(string) = string {
            Self::write_host_usize(string.len() + 1, dest)?;
            Ok(dest.write_all(string.as_bytes())?)
        } else {
            Self::write_host_usize(0, dest)
        }
    }


    /// Writes a single constant [`LuaValue`]
    ///
    /// Note: Not all LuaValues can be dumped as constants (In default implementation: Userdata, Function, Thread or, Table cannot be dumped)
    ///
    /// # Arguments
    ///
    /// * `string`: string to write `None` represents NULL
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_constant<W: Write>(constant: &LuaValue, dest: &mut W) -> Result<(), DumpError> {
        Self::write_byte(
            match constant {
                LuaValue::NIL => constants::typetag::TNIL,
                LuaValue::BOOLEAN(true) => constants::typetag::VTRUE,
                LuaValue::BOOLEAN(false) => constants::typetag::VFALSE,
                LuaValue::NUMBER(LuaNumber::INT(_)) => constants::typetag::VINTEGER,
                LuaValue::NUMBER(LuaNumber::FLOAT(_)) => constants::typetag::VFLOAT,
                LuaValue::STRING(s) if s.len() <= constants::SHORT_STRING_LENGTH => constants::typetag::VSHORTSTRING,
                LuaValue::STRING(_) => constants::typetag::VLONGSTRING,
                LuaValue::USERDATA(_) => constants::typetag::TUSERDATA,
                LuaValue::FUNCTION(_) => constants::typetag::TFUNCTION,
                LuaValue::THREAD(_) => constants::typetag::TTHREAD,
                LuaValue::TABLE(_) => constants::typetag::TTABLE,
            },
            dest,
        )?;
        match constant {
            LuaValue::NIL | LuaValue::BOOLEAN(_) => Ok(()),
            LuaValue::NUMBER(LuaNumber::INT(i)) => Self::write_lua_int(*i, dest),
            LuaValue::NUMBER(LuaNumber::FLOAT(f)) => Self::write_lua_float(*f, dest),
            LuaValue::STRING(s) => Self::write_string(&Some(s.clone()), dest),
            LuaValue::USERDATA(_) => Err(DumpError::CannotDumpConstant),
            LuaValue::FUNCTION(_) => Err(DumpError::CannotDumpConstant),
            LuaValue::THREAD(_) => Err(DumpError::CannotDumpConstant),
            LuaValue::TABLE(_) => Err(DumpError::CannotDumpConstant),
        }
    }

    /// Writes a single upvalue description (See: [`UpvalueDesc`])
    ///
    /// # Arguments
    ///
    /// * `upvalue`: upvalue description to write
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_upvalue_description<W: Write>(upvalue: &UpvalueDesc, dest: &mut W) -> Result<(), DumpError> {
        Ok(dest.write_all(&upvalue.bytes())?)
    }

    /// Writes a single local variable description (See: [`LocalVariableInfo`])
    ///
    /// # Arguments
    ///
    /// * `locvar`: local variable description to write
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_local_variable<W: Write>(locvar: &LocalVariableInfo, dest: &mut W) -> Result<(), DumpError> {
        Self::write_string(&locvar.name, dest)?;
        Self::write_host_int(locvar.startpc, dest)?;
        Self::write_host_int(locvar.endpc, dest)?;
        Ok(())
    }

    /// Writes a single function NOTE: Unless you are implementing [`Dumper`], you likely want to use [`Dumper::dump_chunk`] instead
    ///
    /// # Arguments
    ///
    /// * `prototype`: function prototype to write
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_prototype<W: Write>(prototype: &Prototype, dest: &mut W) -> Result<(), DumpError> {
        let Prototype {
            upvalue_size: _upvalue_size,
            origin_string: source_string,
            first_line_defined,
            last_line_defined,
            param_count,
            is_vararg,
            max_stack_size,
            code,
            constants,
            upvalue_descriptors: upvalues,
            functions,
            lineinfo,
            abslineinfo,
            localvariableinfo: locvars,
            upvaluenames,
            parent: _parent // We ignore the parent field as all children of a prototype are dumped with the parent.
        } = prototype;
        Self::write_string(source_string, dest)?;
        Self::write_host_int(*first_line_defined, dest)?;
        Self::write_host_int(*last_line_defined, dest)?;
        Self::write_byte(*param_count, dest)?;
        Self::write_byte(*is_vararg, dest)?;
        Self::write_byte(*max_stack_size, dest)?;
        Self::write_host_usize(code.len(), dest)?;
        for instruction in code {
            Self::write_lua_instruction(*instruction, dest)?;
        }
        Self::write_host_int(i32::try_from(constants.len()).map_err(|_| DumpError::CannotRepresent)?, dest)?;
        for constant in constants {
            Self::write_constant(constant, dest)?;
        }
        Self::write_host_int(i32::try_from(upvalues.len()).map_err(|_| DumpError::CannotRepresent)?, dest)?;
        for upvalue in upvalues {
            Self::write_upvalue_description(upvalue, dest)?;
        }
        Self::write_host_int(i32::try_from(functions.len()).map_err(|_| DumpError::CannotRepresent)?, dest)?;
        for function in functions {
            Self::write_prototype(&*function, dest)?;
        }
        Self::write_host_int(i32::try_from(lineinfo.len()).map_err(|_| DumpError::CannotRepresent)?, dest)?;
        for lineinfo in lineinfo {
            Self::write_byte(*lineinfo as u8, dest)?;
        }
        Self::write_host_int(i32::try_from(abslineinfo.len()).map_err(|_| DumpError::CannotRepresent)?, dest)?;
        for (start_pc, end_pc) in abslineinfo {
            Self::write_host_int(*start_pc, dest)?;
            Self::write_host_int(*end_pc, dest)?;
        }
        Self::write_host_int(i32::try_from(locvars.len()).map_err(|_| DumpError::CannotRepresent)?, dest)?;
        for local_variable in locvars {
            Self::write_local_variable(local_variable, dest)?;
        }
        Self::write_host_int(i32::try_from(upvaluenames.len()).map_err(|_| DumpError::CannotRepresent)?, dest)?;
        for upvaluename in upvaluenames {
            Self::write_string(upvaluename, dest)?;
        }
        Ok(())
    }

    /// Writes the Lua bytecode header
    ///
    /// # Arguments
    ///
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn write_header<W: Write>(dest: &mut W) -> Result<(), DumpError> {
        dest.write_all(Self::LUA_SIGNATURE)?;
        dest.write_all(Self::LUA_VERSION)?;
        dest.write_all(Self::LUA_FORMAT)?;
        dest.write_all(Self::LUA_CONV_DATA)?;
        dest.write_all(Self::LUA_SYSTEM_PARAMETER)?;
        Self::write_lua_int(Self::LUA_CHECK_INTEGER, dest)?;
        Self::write_lua_float(Self::LUA_CHECK_FLOAT, dest)?;
        Ok(())
    }

    /// Dumps a Lua function as a bytecode chunk, resulting in a full single bytecode 'file'
    ///
    /// # Arguments
    ///
    /// * `prototype`: function to dump
    /// * `dest`: Writer to use
    ///
    /// returns: Result<(), DumpError>
    ///
    fn dump_chunk<W: Write>(proto: &Prototype, dest: &mut W) -> Result<(), DumpError> {
        Self::write_header(dest)?;
        Self::write_byte(proto.upvalue_size, dest)?; // Amount of upvalues; This property is only written for the outer function of the chunk
        Self::write_prototype(proto, dest)?;
        Ok(())
    }
}

/// Struct providing default implementation for [`Dumper`] which is compatible with Lua 5.4 on little-endian 64-bit architectures. (x86/ARM64)
///
/// Uses i64 for LUA_INT, f64 for LUA_FLOAT, and u32 for LUA_INSTRUCTION
///
pub struct LE64Dumper {}
impl Dumper for LE64Dumper {
    const LUA_SYSTEM_PARAMETER: &'static [u8] = &[
        mem::size_of::<u32>() as u8,    // Instruction size in bytes
        mem::size_of::<i64>() as u8,    // Integer size in bytes
        mem::size_of::<f64>() as u8     // Float size in bytes
    ];

    /// Wrapper around write_host_usize
    fn write_host_int<W: Write>(int: HOST_INT, dest: &mut W) -> Result<(), DumpError> {
        Self::write_host_usize(isize::try_from(int).map_err(|_| DumpError::CannotRepresent)? as usize, dest)
    }

    fn write_lua_int<W: Write>(int: LUA_INT, dest: &mut W) -> Result<(), DumpError> {
        Ok(dest.write_all(&i64::try_from(int).map(i64::to_le_bytes).map_err(|_| DumpError::CannotRepresent)?)?)
    }

    fn write_lua_float<W: Write>(float: LUA_FLOAT, dest: &mut W) -> Result<(), DumpError> {
        Ok(dest.write_all(&f64::try_from(float).map(f64::to_le_bytes).map_err(|_| DumpError::CannotRepresent)?)?)
    }

    fn write_lua_instruction<W: Write>(instruction: LUA_INSTRUCTION, dest: &mut W) -> Result<(), DumpError> {
        Ok(dest.write_all(&u32::try_from(instruction.inner).map(u32::to_le_bytes).map_err(|_| DumpError::CannotRepresent)?)?)
    }
}
