//! Module containing the Lua OS functions library
//!
//! Available functions vary depending on enabled cargo features

use crate::vm::LuaVM;
use crate::types::value::table::LuaTable;
use crate::types::value::LuaValue;
use crate::types::varargs::Varargs;
use crate::error::{LuaError, IntoArgumentError, GenericError};
use std::time::{Instant, Duration};
use crate::constants::types::{LUA_FLOAT};
use crate::types::parameters::LuaParameters;
use crate::types::value::string::LuaString;
use std::ffi::OsStr;

/// Returns approximate time (in seconds) since the start of the LuaVM
///
/// Timed from the creation of the LuaVM struct
///
/// Rust callers may use [`LuaVM::start_instant`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-os.clock>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn clock(lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    Ok(Varargs::from(
        Instant::now()
            .checked_duration_since(lua_vm.start_instant())
            .unwrap_or(Duration::from_secs(0))
            .as_secs_f64() as LUA_FLOAT
    ))
}

/// Returns date and time, in specified format
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-os.date>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn date(_lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    todo!()
}

/// Returns difference between two os.time() instants
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-os.difftime>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn difftime(_lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    todo!()
}

/// Starts a subprocess
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-os.execute>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[cfg(feature = "os-env")]
pub fn execute(_lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    todo!()
}

/// Raises 'ExitProcess' error
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-os.exit>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[cfg(all(feature = "os-env", feature = "os-env-exit"))]
pub fn exit(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let code = params.coerce_opt::<LUA_INT>(0)?.unwrap_or(0);
    Err(LuaError::exit_process(code))
}

/// Exits host process via std::process::exit
///
/// WARNING: This immediately exits the host program with no further cleanup or destructors.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-os.exit>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[cfg(all(feature = "os-env", not(feature = "os-env-exit")))]
pub fn exit(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let code = params.coerce_opt::<LUA_INT>(0)?.unwrap_or(0);
    std::process::exit(code.try_i32().unwrap_or(if code > 0 { i32::MAX } else { i32::MIN }))
}

/// Retrieves environment variable
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-os.getenv>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[cfg(feature = "os-env")]
pub fn getenv(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    use os_str_bytes::OsStrBytes;

    let key = params.coerce::<LuaString>(0)?;
    for (name, variable) in std::env::vars_os() {   // Loop as std::env::var() may panic on invalid input
        if name.to_raw_bytes() == key.as_bytes() {
            return Ok(Varargs::from(LuaString::from(variable.to_raw_bytes())));
        }
    }

    return Ok(Varargs::fail());
}

/// Removes specified file
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-os.remove>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[cfg(feature = "file-io")]
pub fn remove(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let path = params.coerce::<LuaString>(0)?;
    let os_path = path.as_str()
        .map(OsStr::new)
        .ok_or(GenericError::Str("Path must be valid UTF-8"))
        .error_for_argument(0)?;

    match std::fs::remove_file(os_path) {
        Ok(_) => Ok(Varargs::from(true)),
        Err(error) => Ok(Varargs::from((Varargs::fail_value(), format!("{}", error)))),
    }
}

/// Renames/moves specified file
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-os.rename>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[cfg(feature = "file-io")]
pub fn rename(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let old_path = params.coerce::<LuaString>(0)?;
    let new_path = params.coerce::<LuaString>(1)?;

    let old_os_path = old_path.as_str()
        .map(OsStr::new)
        .ok_or(GenericError::Str("Path must be valid UTF-8"))
        .error_for_argument(0)?;

    let new_os_path = new_path.as_str()
        .map(OsStr::new)
        .ok_or(GenericError::Str("Path must be valid UTF-8"))
        .error_for_argument(1)?;

    match std::fs::rename(old_os_path, new_os_path) {
        Ok(_) => Ok(Varargs::from(true)),
        Err(error) => Ok(Varargs::from((Varargs::fail_value(), format!("{}", error)))),
    }
}

/// Sets system locale
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-os.setlocale>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[cfg(feature = "os-env")]
pub fn setlocale(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    todo!()
}

/// Retrieves time in specified format
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-os.time>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn time(_lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    todo!()
}

/// Returns path of temporary file
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-os.tmpname>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[cfg(feature = "file-io")]
pub fn tmpname(_lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    return Ok(Varargs::from("./temp"));  // TODO: Make configurable
}

/// Adds the os library to the global environment of the specified LuaVM
///
/// Functions available depend on enabled cargo features
///
/// # Arguments
///
/// * `lua_vm`: LuaVM to add the os library to
///
/// returns: ()
pub fn insert_os_lib(lua_vm: &mut LuaVM) {
    let table = LuaTable::empty();

    set_table_func!(table, clock);
    set_table_func!(table, date);
    set_table_func!(table, difftime);
    #[cfg(feature = "os-env")]
    set_table_func!(table, execute);
    #[cfg(feature = "os-env")]
    set_table_func!(table, exit);
    #[cfg(feature = "os-env")]
    set_table_func!(table, getenv);
    #[cfg(feature = "file-io")]
    set_table_func!(table, remove);
    #[cfg(feature = "file-io")]
    set_table_func!(table, rename);
    #[cfg(feature = "os-env")]
    set_table_func!(table, setlocale);
    set_table_func!(table, time);
    #[cfg(feature = "file-io")]
    set_table_func!(table, tmpname);

    lua_vm.global_env.raw_set("os", table.clone());
    lua_vm.modules.insert("os", table);
}