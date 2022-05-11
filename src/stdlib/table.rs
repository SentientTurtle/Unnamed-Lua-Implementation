//! Module containing the Lua table functions library

use crate::vm::LuaVM;
use crate::error::{LuaError};
use crate::types::value::LuaValue;
use crate::types::value::string::LuaString;
use crate::types::varargs::Varargs;
use crate::types::value::table::LuaTable;
use crate::types::parameters::LuaParameters;
use crate::constants::types::LUA_INT;
use crate::types::CoerceFrom;
use crate::lua_func;


/// "Joins" table into concatenation; All elements of the table appended to each other, with an optional separator in between
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-table.concat>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn concat(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let mut is_utf8 = true;

    let table = params.coerce::<LuaTable>(0)?;
    let separator_option = params.coerce::<LuaString>(1).ok();
    let separator = match separator_option.as_ref() {
        None => b"",
        Some(sep) => {
            is_utf8 &= sep.is_utf8();
            sep.as_bytes()
        }
    };
    let start = params.coerce::<LUA_INT>(2).unwrap_or(1);
    let end = params.coerce::<LUA_INT>(3).ok();

    let mut buffer = Vec::new();
    let mut index = start;
    loop {
        if let Some(end) = end { if index > end { break; } }
        match table.raw_get(index) {
            LuaValue::NIL => break,
            val => {
                let string = LuaString::coerce_from(&val)?;
                is_utf8 &= string.is_utf8();
                if index != start {
                    buffer.extend_from_slice(separator)
                }
                buffer.extend_from_slice(string.as_bytes());
            }
        }
        index += 1;
    }

    if is_utf8 {
        match String::from_utf8(buffer) {   // Performance optimization candidate; This should never fail
            Ok(string) => Ok(Varargs::from(string)),
            Err(err) => {
                debug_assert!(false, "Byte-vec to String conversion failed; This should not happen as is_utf8 variable tracks if the string is valid utf-8!");
                Ok(Varargs::from(err.into_bytes()))
            }
        }
    } else {
        Ok(Varargs::from(buffer))
    }
}


/// Unpacks table into varargs
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-table.unpack>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn unpack(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {  // TODO: Create an array-slice method on LuaTable
    let table = params.coerce::<LuaTable>(0)?;
    let start = params.coerce::<LUA_INT>(1).unwrap_or(1);
    let end = params.coerce::<LUA_INT>(2).ok();

    let mut buffer = Vec::new();
    let mut index = start;
    loop {
        if let Some(end) = end { if index > end { break; } }
        match table.raw_get(index) {
            LuaValue::NIL => break,
            val => buffer.push(val),
        }
        index += 1;
    }

    Ok(Varargs::from(buffer))
}

/// Adds the table library to the global environment of the specified LuaVM.
///
/// # Arguments
///
/// * `lua_vm`: LuaVM to add the table library to
///
/// returns: ()
pub fn insert_table_lib(lua_vm: &mut LuaVM) {
    let table = LuaTable::empty();

    set_table_func!(table, concat);
    set_table_func!(table, unpack);
    // TODO: Other table library functions

    lua_vm.global_env.raw_set("table", table.clone());
    lua_vm.modules.insert("table", table);
}