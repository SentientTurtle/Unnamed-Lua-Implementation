//! Test module
//!
//! Uses [Lua test suite](https://www.lua.org/tests/) in the ./test folder
use std::process::Command;
use crate::bytecode;
use crate::vm;
use crate::vm::LuaVM;
use crate::types::value::function::{InstructionDisplay, LuaClosure};
use crate::types::value::LuaValue;
use std::io::Write;
use std::fs::File;
use crate::bytecode::loader::Loader;
use crate::bytecode::dumper::Dumper;
use crate::types::value::string::LuaString;
use crate::constants::types::LUA_INSTRUCTION;
use crate::types::upvalue::Upvalue;

/// Dumps call trace to ./trace/[script-name].csv
fn dump_trace(file_name: &str, lua_vm: &mut LuaVM) -> std::io::Result<()> {
    let trace = lua_vm.get_trace().as_ref().expect("Tracing needs to be enabled!");

    std::fs::create_dir("./trace")?;
    let mut file = File::create(format!("./trace/{}.csv", file_name))?;

    for (prototype, instruction_indices) in trace {
        if prototype.first_line_defined == 0 && prototype.last_line_defined == 0 {
            writeln!(file, "{},",
                     prototype.origin_string.as_ref().unwrap_or(&LuaString::from("[UNKNOWN]")),
            )?;
        } else {
            writeln!(file, "{} ({} to {}),",
                     prototype.origin_string.as_ref().unwrap_or(&LuaString::from("[UNKNOWN]")),
                     prototype.first_line_defined,
                     prototype.last_line_defined
            )?;
        }
        let lines = {
            use std::fs;
            fs::read_to_string("./lua-tests/".to_owned() + file_name + ".lua")
                .expect("script file should be readable")
                .split("\n")
                .map(str::to_string)
                .collect::<Vec<String>>()
        };
        let mut prev_line = usize::MAX;
        for index in instruction_indices {
            let new_line_index = prototype.get_line(*index).expect("Debug info not available!");
            if prev_line != new_line_index {
                for i in prev_line..(new_line_index - 1) {
                    let line = lines.get(i)
                        .map(String::as_str)
                        .unwrap_or("[unknown line]")
                        .trim_end_matches("\n")
                        .trim_end_matches("\r")
                        .replace('"', "\"\"");
                    writeln!(file, "{},\"{}\",,", i + 1, line)?;
                }
                let line = lines.get(new_line_index - 1)
                    .map(String::as_str)
                    .unwrap_or("[unknown line]")
                    .trim_end_matches("\n")
                    .trim_end_matches("\r")
                    .replace('"', "\"\"");
                write!(file, "{},\"{}\",", new_line_index, line)?;
            } else {
                write!(file, ",,")?;
            }
            writeln!(file, "{},\"{}\"",
                   index,
                   format!("{}",
                             InstructionDisplay {
                                 proto: &*prototype,
                                 index: *index,
                                 instruction: *prototype.code.get(*index).unwrap_or(&LUA_INSTRUCTION::invalid()),
                             }
                     )
                         .replace('\t', "    ")
                         .replace('"', "\"\"")
            )?;
            prev_line = new_line_index;
        }
    }
    file.flush()
}

/// Runs Lua test file, expects file to be located in ./test directory
fn do_test(file_name: &str) {
    if !std::env::current_dir().expect("IO required for tests!").to_string_lossy().contains("test") {
        std::env::set_current_dir("./test").expect("Must change current directory for tests!")
    }
    eprintln!("Loading script: {:?}", file_name);
    let command = Command::new("luac").arg("-o").arg("-").arg(String::from("./") + file_name + ".lua").output().expect("Luac error!");
    if command.stderr.len() > 0 {
        panic!("Script compile error: {}", String::from_utf8(command.stderr).unwrap());
    }


    let mut script_buffer: Vec<u8>;
    if cfg!(target_os = "windows") {    // Stupid hack to remove the LF -> CRLF upgrading windows does for SOME REASON (一︿一)
        let mut iter = command.stdout.into_iter();
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
        script_buffer = command.stdout;
    }
    let mut lua_vm = LuaVM::new();
    crate::stdlib::insert_stdlib(&mut lua_vm);
    crate::stdlib::debug::insert_debug_lib(&mut lua_vm);
    lua_vm.global_env.raw_set("_soft", true);

    eprintln!("Decoding script: {:?}", file_name);
    let proto = bytecode::loader::LE64Loader::load_chunk(&mut &script_buffer[..], None,script_buffer.len()).unwrap();
    println!("{}", proto);
    let closure = LuaClosure::new(proto, vec![Upvalue::new_closed(LuaValue::from(lua_vm.global_env.clone()))]);
    eprintln!("Running script: {:?}", file_name);
    match vm::execute_closure(closure, &mut lua_vm, &[]) {
        Ok(_) => eprintln!("Completed script: {:?}", file_name),
        Err(err) => {
            if lua_vm.get_trace().is_some() {
                dump_trace(file_name, &mut lua_vm).expect("File IO Error");
            }
            eprintln!("Failed script: {:?}", file_name);
            eprintln!("Remaining stack:");
            for frame in lua_vm.into_stack().drain(..) {
                println!("\t{:?}", frame.closure.prototype());
            }

            panic!("{:#?}", err);
        }
    };
}

/// Performs round-trip bytecode parsing; Loading script and dumping it to validate bytecode dumping
///
/// NOTE: This test does not validate bytecode is loaded correctly, that is validated as part of the functionality tests in [`do_test`]
fn do_bytecode_check<const N: usize>(names: [&'static str; N]) {
    if !std::env::current_dir().expect("IO required for tests!").to_string_lossy().contains("test") {
        std::env::set_current_dir("../test").expect("Must change current directory for tests!")
    }
    for file_name in names {
        eprintln!("Loading script: {:?}", file_name);
        let command = Command::new("luac").arg("-o").arg("-").arg(String::from("./") + file_name + ".lua").output().expect("Luac error!");
        if command.stderr.len() > 0 {
            panic!("Script compile error: {}", String::from_utf8(command.stderr).unwrap());
        }

        let mut load_buffer: Vec<u8>;
        if cfg!(target_os = "windows") {    // Stupid hack to remove the LF -> CRLF upgrading windows does for SOME REASON (一︿一)
            let mut iter = command.stdout.into_iter();
            let mut prev = iter.next().unwrap();
            load_buffer = iter.filter_map(|b| {
                if b == 0xA && prev == 0xD {
                    prev = b;
                    None
                } else {
                    let ret = Some(prev);
                    prev = b;
                    ret
                }
            }).collect();
            load_buffer.push(prev);
        } else {
            load_buffer = command.stdout;
        }

        eprintln!("Decoding script: {:?}", file_name);
        let proto = match bytecode::loader::LE64Loader::load_chunk(&mut &load_buffer[..], None,load_buffer.len()) {
            Ok(proto) => proto,
            Err(err) => {
                eprintln!("Error loading script: {:?}, {}", file_name, err);
                let mut loadbuf_out_file = File::create("../loadbuffer.txt").unwrap();
                for (index, name, bytes, value) in bytecode::loader::LE64Loader::explain(&*load_buffer).unwrap() {
                    writeln!(&mut loadbuf_out_file, "{:016X}\t{} {:X?} {}", index, name, bytes, value).unwrap();
                }
                loadbuf_out_file.flush().unwrap();
                panic!("{}", err);
            }
        };

        eprintln!("Dumping script: {:?}", file_name);
        let mut dump_buffer = Vec::with_capacity(load_buffer.len());
        bytecode::dumper::LE64Dumper::dump_chunk(&proto, &mut dump_buffer).unwrap();
        eprintln!("Comparing script: {:?}", file_name);
        if load_buffer != dump_buffer {
            eprintln!("Not-equal");
            let mut loadbuf_out_file = File::create("../loadbuffer.txt").unwrap();
            for (index, name, bytes, value) in bytecode::loader::LE64Loader::explain(&*load_buffer).unwrap() {
                writeln!(&mut loadbuf_out_file, "{:016X}\t{} {:X?} {}", index, name, bytes, value).unwrap();
            }
            loadbuf_out_file.flush().unwrap();

            let mut dumpbuf_out_file = File::create("../dumpbuffer.txt").unwrap();
            for (index, name, bytes, value) in bytecode::loader::LE64Loader::explain(&*dump_buffer).unwrap() {
                writeln!(&mut dumpbuf_out_file, "{:016X}\t{} {:X?} {}", index, name, bytes, value).unwrap();
            }
            dumpbuf_out_file.flush().unwrap();

            let mut i = 0;
            while i < load_buffer.len() && i < dump_buffer.len() && load_buffer[i] == dump_buffer[i] {
                i += 1;
            }
            eprintln!("Skipping {} bytes", i);
            assert_eq!(load_buffer[i..], dump_buffer[i..], "Left:  {:X?}\nRight: {:X?}\n", &load_buffer[i..(i+20).min(load_buffer.len())], &dump_buffer[i..(i+20).min(dump_buffer.len())]);
        } else {
            eprintln!("Script dump success: {:?}", file_name);
        }
    }
}

macro_rules! test {
    ($($name:ident),+) => {
        $(
            #[test]
            fn $name() {
                do_test(stringify!($name));
            }
        )+

        #[test]
        fn _test_bytecode_load_dump() {
            do_bytecode_check([$(stringify!($name),)+])
        }
    };
}

/// Separate "_reproduce" text for ease of first time running; IDEs can recognize this test but not the macro-generated tests below
///
/// Used as a "scratch file" test for reproducing smaller parts of the full test suite to aid in debugging
#[test]
fn _reproduce() {
    do_test(stringify!( _reproduce ));
}

test!(
    api,
    attrib,
    big,
    bitwise,
    bwcoercion,
    calls,
    closure,
    code,
    constructs,
    coroutine,
    // cstack,  // C-stack not implemented and c libraries not supported by this Lua implementation
    db,
    errors,
    events,
    files,
    gc,
    gengc,
    goto,
    heavy,
    literals,
    locals,
    main,
    math,
    nextvar,
    pm,
    sort,
    strings,
    tpack,
    utf8,
    vararg,
    verybig
);