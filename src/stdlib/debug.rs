//! Module containing the Lua debug library, as well as other assorted debug luafunctions

use crate::constants::types::LUA_INT;
use crate::error::{IntoArgumentError, LuaError};
use crate::types::parameters::LuaParameters;
use crate::types::value::LuaValue;
use crate::types::value::string::LuaString;
use crate::vm::LuaVM;
use crate::types::value::table::LuaTable;
use crate::types::value::thread::LuaThread;
use crate::types::varargs::Varargs;
use crate::util::{Union2};

/// Debug function to provide regex strings of patterns.
/// Will be removed as pattern implementation is moved away from regex-under-the-hood
pub fn pattern_regex(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let regex = crate::stdlib::string::pattern_old::compile_pattern(params.coerce::<LuaString>(0)?.as_bytes())
        .error_for_argument(0)?;
    Ok(Varargs::from(regex.as_str()))
}

/// Returns debug information about specified function/stackframe
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-debug.getinfo>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn getinfo(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    match params.coerce_any::<LuaThread, LUA_INT>(0)? {
        Union2::T(_thread) => {
            todo!()
        },
        Union2::U(_stack_index) => {
            todo!()
        }
    }
    //Ok(Varargs::fail())
}

/// Adds the debug library to the global environment of the specified LuaVM.
///
/// # Arguments
///
/// * `lua_vm`: LuaVM to add the debug library to
///
/// returns: ()
pub fn insert_debug_lib(lua_vm: &mut LuaVM) {
    let table = LuaTable::empty();

    set_table_func!(table, pattern_regex);
    set_table_func!(table, getinfo);

    lua_vm.global_env.raw_set("debug", table.clone());
    lua_vm.modules.insert("debug", table);
}