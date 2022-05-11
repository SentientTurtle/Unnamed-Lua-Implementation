//! Module containing the Lua basic functions library

use std::io;
use crate::error::{LuaError, InvalidValueError, IntoArgumentError, GenericError, CompileError};
use crate::vm::LuaVM;
use crate::constants;
use crate::constants::types::{LUA_INT};
use crate::types::value::LuaValue;
use crate::types::varargs::Varargs;
use crate::types::value::string::LuaString;
use crate::types::value::number::LuaNumber;
use crate::types::value::function::{LuaFunction, RustClosure};
use crate::types::parameters::LuaParameters;
use crate::types::value::table::LuaTable;
use crate::types::{LuaType, CoerceFrom};
use crate::util::Union2;
use crate::bytecode;
use crate::bytecode::loader::Loader;
use crate::bytecode::loader::LoadError;
use crate::compiler::{DefaultCompiler, LuaCompiler};
use crate::lua_func;
use crate::types::upvalue::Upvalue;

/// Asserts the first argument is truthy, or throws an error (with second optional argument as message)
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-assert>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn assert(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    match params.first() {
        Some(val) if bool::coerce_from(val).expect("Coerce to bool never fails!") => Ok(Varargs::from(params)),
        _ => error(lua_vm, &[
            params.get(1)
                .map(LuaValue::clone)
                .unwrap_or(LuaValue::from("assertion failed!"))
        ])
    }
}

/// Interface to Lua garbage collection.
///
/// Provided for compatibility with existing scripts, but functions as a NO-OP. This behaviour is subject to change.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-collectgarbage>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn collectgarbage(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let option = params.coerce::<LuaString>(0).unwrap_or(LuaString::from("collect"));
    match &option {
        s if s == "collect" => Ok(Varargs::nil()),
        s if s == "stop" => Ok(Varargs::nil()),
        s if s == "restart" => Ok(Varargs::nil()),
        s if s == "count" => unimplemented!(),
        s if s == "step" => Ok(Varargs::nil()),
        s if s == "setpause" => Ok(Varargs::from(0 as LUA_INT)),
        s if s == "setstepmul" => Ok(Varargs::from(0 as LUA_INT)),
        s if s == "isrunning" => Ok(Varargs::from(true)),
        s => Err(LuaError::with_message(format!("invalid option: {}", s)))?
    }
}

/// Runs the specified file as a lua script, or parses standard input if no file is specified.
///
/// **NOTE: This function is not available unless the cargo feature 'file-io' is enabled**
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-dofile>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[cfg(feature = "file-io")]
pub fn dofile(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    if let Some(_filename) = params.coerce_opt::<LuaString>(0)? {}
    todo!()
}

/// Throws an error (with optional argument as message)
///
/// Currently does not support error levels, this is subject to change.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-error>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn error(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    Err(LuaError::with_message(params.first_or_nil().clone()))?    // TODO: Error levels
}

/// Retrieves the metatable of the passed value.
///
/// Note: Metatables may guard against this through use of the '__metatable' field.
/// If the value's metatable contains this field, the fields value will be returned instead.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-getmetatable>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn getmetatable(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let val = params.first_value()?;
    if let Some(guard) = val.index_metatable("__metatable", &lua_vm.metatables).not_nil() {
        Ok(Varargs::from(guard))
    } else {
        Ok(Varargs::from(val.clone_metatable(&lua_vm.metatables).map(LuaValue::from).unwrap_or(LuaValue::NIL)))
    }
}

/// Integer iteration over passed table. Iterating from key 1 to key N, where table\[N\] is nil
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-ipairs>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn ipairs(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let table = params.coerce::<LuaTable>(0)?;
    let closure_table = table.clone();
    let mut index: LUA_INT = 1;
    let closure: RustClosure = RustClosure::new("ipairs-closure", move |_: &mut LuaVM, _: &[LuaValue]| {
        let result = closure_table.raw_get(index);

        if let LuaValue::NIL = result {
            Ok(Varargs::from(()))
        } else {
            index += 1;
            Ok(Varargs::from((index, result)))
        }
    });
    Ok(Varargs::from((LuaFunction::RUST_CLOSURE(closure), table.clone(), 0 as LUA_INT)))
}

/// Loads a lua script, from either a passed string or passed 'reader function'
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-load>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn load(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    enum Mode {
        Binary,
        Text,
        BinaryOrText,
    }

    let chunk = params.coerce_any::<LuaString, LuaFunction>(0)?;
    let chunkname = params.coerce_opt::<LuaString>(1)?;
    let mode = params.coerce_opt::<LuaString>(2)?;
    let env = params.coerce_opt::<LuaValue>(3)?;

    let mode = match mode {
        Some(string) => {
            match string.as_bytes() {
                b"b" => Mode::Binary,
                b"t" => Mode::Text,
                b"bt" => Mode::BinaryOrText,
                _ => {
                    return Err(LuaError::with_message(format!("Unknown load mode {} expected `b`, `t`, or `bt`", string)))?;
                }
            }
        }
        None => Mode::BinaryOrText
    };

    let name = if let Some(name) = chunkname {
        name
    } else {
        let callee_name = crate::vm::debug::callee_name(lua_vm);
        let callee_line = crate::vm::debug::callee_line(lua_vm);
        if let Some(c_name) = callee_name {
            if let Some(c_line) = callee_line {
                LuaString::from(format!("load() {} @ line {}", c_name, c_line))
            } else {
                LuaString::from(format!("load() {}", c_name))
            }
        } else {
            LuaString::from(format!("load() @ [UNKNOWN]"))
        }
    };

    let compile_result = match chunk {
        Union2::T(string) => {
            let mut bytes = string.as_bytes();
            match mode {
                Mode::Binary => bytecode::loader::LE64Loader::load_chunk(&mut bytes, Some(name), lua_vm.max_script_size()).map_err(LoadError::lua_message),   // TODO: Selectable loader
                Mode::Text => DefaultCompiler::compile_and_load(&mut bytes, Some(name), lua_vm.max_script_size()).map_err(CompileError::message),
                Mode::BinaryOrText => {
                    if bytes.len() > constants::LUA_SIGNATURE.len() && &bytes[0..constants::LUA_SIGNATURE.len()] == constants::LUA_SIGNATURE {
                        bytecode::loader::LE64Loader::load_chunk(&mut bytes, Some(name), lua_vm.max_script_size()).map_err(LoadError::lua_message)
                    } else {
                        DefaultCompiler::compile_and_load(&mut bytes, Some(name), lua_vm.max_script_size()).map_err(CompileError::message)
                    }
                }
            }
        }
        Union2::U(function) => {
            let mut chunk = Vec::new();
            loop {  // TODO: Add break condition in case of infinite loop
                let chunkpiece = function.call(lua_vm, &[])?;
                match chunkpiece.opt(0) {
                    None | Some(LuaValue::NIL) => break,
                    Some(LuaValue::STRING(string)) if string.len() == 0 => break,
                    Some(LuaValue::STRING(string)) => chunk.extend_from_slice(string.as_bytes()),
                    Some(value) => {
                        return Ok(Varargs::from((LuaValue::NIL, format!("reader function must return a string, found: {}", value))));
                    }
                }
            }

            let mut bytes = &chunk[..];
            match mode {
                Mode::Binary => bytecode::loader::LE64Loader::load_chunk(&mut bytes, Some(name), lua_vm.max_script_size()).map_err(LoadError::lua_message),   // TODO: Selectable loader
                Mode::Text => DefaultCompiler::compile_and_load(&mut bytes, Some(name), lua_vm.max_script_size()).map_err(CompileError::message),
                Mode::BinaryOrText => {
                    if bytes.len() > constants::LUA_SIGNATURE.len() && &bytes[0..constants::LUA_SIGNATURE.len()] == constants::LUA_SIGNATURE {
                        bytecode::loader::LE64Loader::load_chunk(&mut bytes, Some(name), lua_vm.max_script_size()).map_err(LoadError::lua_message)
                    } else {
                        DefaultCompiler::compile_and_load(&mut bytes, Some(name), lua_vm.max_script_size()).map_err(CompileError::message)
                    }
                }
            }
        }
    };

    match compile_result {
        Ok(prototype) => {
            let env = if let Some(environment) = env {
                environment
            } else {
                LuaValue::from(lua_vm.global_env.clone())
            };

            Ok(Varargs::from(LuaFunction::new_lua(prototype, vec![Upvalue::new_closed(env)])))
        }
        Err(error) => {
            Ok(Varargs::from((Varargs::fail_value(), error)))
        }
    }
}

/// Loads a lua script, from specified file or stdin.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-loadfile>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn loadfile(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    #[allow(unused)] let (_, _) = (lua_vm, params);   // Stub to silence unused variable warning
    todo!()
}


/// Returns the next hash key of a given table and (optional) starting key. Used for iteration of tables.
///
/// Generally used through [ `pairs`], which returns this function as iterator.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-next>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn next(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let table = params.coerce::<LuaTable>(0)?;
    let index = params.get(1).unwrap_or(&LuaValue::NIL);
    let result = table.next(index);
    Ok(result.map(Varargs::from).unwrap_or(Varargs::from(())))
}

/// Iterates over all table values, numerical and otherwise.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-pairs>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn pairs(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let val = params.first_value()?;
    if let Some(function) = val.index_metatable("__pairs", &lua_vm.metatables).not_nil() {
        let result = function.call(lua_vm, &[val.clone()])?;
        return Ok(result.select_range(0..=3));
    }

    Ok(Varargs::from((lua_func!(next), val.clone(), LuaValue::NIL)))
}

/// Protected call to passed function
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-pcall>
///
/// This function is not required for 'safe' calls to Lua functions; LuaErrors may be suppressed by simply dropping them.
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn pcall(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let (func, params) = params.split_first()
        .ok_or(InvalidValueError { expected: "function", found: "nil", }.error_for_argument(0))?;
    let result = match func.clone().call(lua_vm, params) {
        Ok(result) => Varargs::prepend(true, result),
        Err(err) if err.is_exit() => return Err(err),   // Re-throw exit-process pseudo-error
        Err(err) => Varargs::from((false, err.message())),
    };
    Ok(result)
}

/// Prints the string representation of the passed valued to LuaVM's "out"
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-print>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn print(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    if params.len() == 0 {
        writeln!(lua_vm.stdout())
            .map_err(|err| LuaError::with_message(format!("IO Error: {}", err)))?;
    } else {
        let (first, others) = params.split_first().unwrap();
        let text = first.lua_to_string(lua_vm)?;
        write!(lua_vm.stdout(), "{}", text)
            .map_err(|err| LuaError::with_message(format!("IO Error: {}", err)))?;
        for val in others {
            let text = val.lua_to_string(lua_vm)?;
            write!(lua_vm.stdout(), "\t{}", text)
                .map_err(|err| LuaError::with_message(format!("IO Error: {}", err)))?;
        }
        writeln!(lua_vm.stdout())
            .map_err(|err| LuaError::with_message(format!("IO Error: {}", err)))?;
    }
    Ok(Varargs::from(()))
}

/// Equivalent to [`print()`] that uses rust's debug formatting. Only available with debug assertions enabled.
///
/// Note: This function will be moved to the debug library in the future.
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
// TODO: Move to debug library
#[cfg(debug_assertions)]
pub fn debugprint(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let out = lua_vm.stdout();
    let result: Result<(), io::Error> = try {
        if params.len() == 0 {
            writeln!(out, "{:?}", "\n")?;
        } else {
            let (first, others) = params.split_first().unwrap();
            writeln!(out, "{:?}", first)?;
            for val in others {
                writeln!(out, "\t{:?}", val)?;
            }
            writeln!(out)?;
        }
    };
    match result {
        Ok(_) => Ok(Varargs::from(())),
        Err(err) => Err(LuaError::with_message(format!("IO Error: {}", err)))
    }
}

/// Checks equality without invoking __eq metamethod
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-rawequal>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn rawequal(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let lhs = params.coerce::<LuaValue>(0)?;
    let rhs = params.coerce::<LuaValue>(1)?;
    Ok(Varargs::from(lhs == rhs))
}

/// Raw table access without invoking __index metamethod
///
/// Rust functions may use [`LuaTable::raw_get`] directly, instead of calling this function.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-rawget>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn rawget(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let table = params.coerce::<LuaTable>(0)?;
    let index = params.coerce::<LuaValue>(1)?;
    Ok(Varargs::from(table.raw_get(index.try_key().error_for_argument(1)?)))
}

/// Table/String length without invoking __len metamethod
///
/// Rust functions may use [`LuaTable::len`] or [`LuaString::len`] directly, instead of calling this function.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-rawlen>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn rawlen(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    match params.first() {
        Some(LuaValue::TABLE(t)) => Ok(Varargs::from(t.len())),
        Some(LuaValue::STRING(s)) => Ok(Varargs::from(s.len())),
        Some(_) | None => Err(InvalidValueError { expected: "table or string", found: params.first_or_nil().type_name() }.error_for_argument(0))?
    }
}

/// Raw table access without invoking __newindex metamethod
///
/// Rust functions may use [`LuaTable::raw_set`] directly, instead of calling this function.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-rawset>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn rawset(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let table = params.coerce::<LuaTable>(0)?;
    let index = params.coerce::<LuaValue>(1)?;
    let value = params.coerce::<LuaValue>(2)?;

    table.raw_set(index.try_key().error_for_argument(1)?, value);
    Ok(Varargs::from(table))
}

/// Selects Nth value from varargs. (Or counts values)
///
/// Rust functions may use [`Varargs::n`] and [`Varargs::count`] directly, instead of calling this function.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-select>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn select(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let index = params.coerce_any::<LUA_INT, LuaString>(0)?;
    match index {
        Union2::T(index) => {
            if index > 0 {
                if (index as usize) < params.len() {
                    Ok(Varargs::from(&params[index as usize..]))
                } else {
                    Ok(Varargs::from(&[LuaValue::NIL; 0][..]))
                }
            } else if index < 0 {
                if index.abs() as usize > params.len() {
                    Err(GenericError::String(format!("Invalid index {}", index)).error_for_argument(0))?
                } else {
                    let absolute_index = params.len() - (index.abs() as usize);
                    Ok(Varargs::from(&params[absolute_index..]))
                }
            } else {
                Err(GenericError::String(format!("Invalid index {}", index)).error_for_argument(0))?
            }
        }
        Union2::U(index) => {
            if index.eq("#") {
                Ok(Varargs::from(params.len() - 1))
            } else {
                Err(InvalidValueError { expected: "# or number", found: params.first_or_nil().type_name() }.error_for_argument(0))?
            }
        }
    }
}

/// Sets metatable of a table
///
/// NOTE: This function is not equivalent to [`LuaTable::set_metatable_raw`], set_metatable_raw does not respect the '__metatable' field.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-setmetatable>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn setmetatable(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let table = params.coerce::<LuaTable>(0)?;
    let new_metatable = params.coerce::<LuaTable>(1)?;
    if let Some(_) = table.index_metatable("__metatable").not_nil() {
        Err(LuaError::new("cannot change protected metatable"))?;
    }
    table.set_metatable_raw(new_metatable.clone());
    Ok(Varargs::from(table.clone()))
}

/// Converts value to number, if a radix is specified as second argument only strings can be converted.
///
/// Rust functions may use [`LuaNumber::coerce_from`] directly, instead of calling this function.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-tonumber>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn tonumber(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let value = params.first_value()?;

    if let Some(base) = params.coerce_opt::<LUA_INT>(1)? {
        let radix = if base < 0 || base > 36 {
            Err(GenericError::Str("base out of range")).error_for_argument(1)?
        } else {
            base as u32
        };
        if let Some(string) = LuaString::coerce_from(value).error_for_argument(0)?.as_str() {
            Ok(Varargs::from(
                LUA_INT::from_str_radix(string.trim(), radix)
                    .map(LuaValue::from)
                    .unwrap_or(LuaValue::NIL)
            ))
        } else {
            Ok(Varargs::nil())
        }
    } else {
        match value {
            value @ LuaValue::NUMBER(_) => Ok(Varargs::from(value.clone())),
            value => {
                match LuaNumber::coerce_opt(value) {
                    None => Ok(Varargs::from(LuaValue::NIL)),
                    Some(number) => Ok(Varargs::from(number))
                }
            }
        }
    }
}

/// Converts value to string
///
/// NOTE: This function is not equivalent to [Display::fmt](std::fmt::Display::fmt), LuaValue's display does not respect the '__tostring' field.
///
/// Rust callers may use [`LuaValue::lua_to_string`] as a slightly more convenient alternative to this function if full compatibility is required.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-tostring>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
pub fn tostring(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    Ok(Varargs::from(format!("{}", params.first_value()?.lua_to_string(lua_vm)?)))
}

/// Returns lua type of passed value
///
/// Rust callers may use [`LuaType::type_name`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-type>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn lua_type(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    Ok(Varargs::from(params.first_value()?.type_name()))
}

/// Constant value made available as _VERSION
pub const VERSION: &str = "Lua 5.3";


/// Displays a warning message on [`LuaVM`]::stderr if warnings are enabled ([`LuaVM::warnings_enabled()`])
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-type>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn warn(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let first = params.coerce::<LuaString>(0)?;
    match first.as_bytes() {
        b"@on" => lua_vm.warnings_enabled = true,
        b"@off" => lua_vm.warnings_enabled = false,
        invalid_control if invalid_control.first().contains(&&b'@') => {}
        _ => if lua_vm.warnings_enabled {
            let out = lua_vm.stderr();
            let result: Result<(), io::Error> = try {
                write!(out, "lua warning: ")?;
                for i in 0..params.len() {
                    match params.coerce::<LuaString>(i) {
                        Ok(string) => write!(out, "{}", string)?,
                        Err(error) => return Err(LuaError::from(error))
                    }
                }
                writeln!(out)?;
            };
            if let Err(error) = result {
                return Err(LuaError::with_message(format!("{}", error)));
            }
        },
    }
    Ok(Varargs::empty())
}

/// Protected call to passed function, with user-set message handler
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-xpcall>
///
/// This function is not required for 'safe' calls to Lua functions; LuaErrors may be suppressed by simply dropping them.
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn xpcall(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let (func, remainder) = params.split_first()
        .ok_or(InvalidValueError { expected: "function", found: params.first_or_nil().type_name() }.error_for_argument(0))?;
    let (handler, params) = remainder.split_first()
        .ok_or(InvalidValueError { expected: "message handler", found: remainder.first_or_nil().type_name() }.error_for_argument(1))?;

    let (call_result, is_err) = match func.clone().call(lua_vm, params) {
        Ok(result) => (result, false),
        Err(err) if err.is_exit() => return Err(err),   // Re-throw exit-process pseudo-error
        Err(err) => (Varargs::from(err.message()), true),
    };

    let result = handler.clone().call(lua_vm, call_result.as_slice())?;
    Ok(Varargs::prepend(LuaValue::from(is_err), result))
}

/// Adds the basic functions library to the global environment of the specified LuaVM.
///
/// # Arguments
///
/// * `lua_vm`: LuaVM to add the basic library to
///
/// returns: ()
pub fn insert_basic_lib(lua_vm: &mut LuaVM) {
    set_global_func!(lua_vm, assert);
    set_global_func!(lua_vm, collectgarbage);
    #[cfg(feature = "file-io")]
    set_global_func!(lua_vm, dofile);
    // TODO: Split into separate function
    set_global_func!(lua_vm, error);
    set_global_func!(lua_vm, getmetatable);
    set_global_func!(lua_vm, ipairs);
    set_global_func!(lua_vm, load);
    set_global_func!(lua_vm, loadfile);
    set_global_func!(lua_vm, next);
    set_global_func!(lua_vm, pairs);
    set_global_func!(lua_vm, pcall);
    set_global_func!(lua_vm, print);
    set_global_func!(lua_vm, debugprint);
    set_global_func!(lua_vm, rawequal);
    set_global_func!(lua_vm, rawget);
    set_global_func!(lua_vm, rawlen);
    set_global_func!(lua_vm, rawset);
    set_global_func!(lua_vm, select);
    set_global_func!(lua_vm, setmetatable);
    set_global_func!(lua_vm, tonumber);
    set_global_func!(lua_vm, tostring);
    set_global_func!(lua_vm, "type", lua_type);
    lua_vm.global_env.raw_set("_VERSION", VERSION);
    set_global_func!(lua_vm, warn);
    set_global_func!(lua_vm, xpcall);
}
