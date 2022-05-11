//! Module containing the Lua coroutine library

use crate::error::{InvalidKeyError, LuaError};
use crate::types::parameters::LuaParameters;
use crate::types::value::function::{LuaFunction, RustClosure};
use crate::types::value::LuaValue;
use crate::vm::LuaVM;
use crate::types::value::table::LuaTable;
use crate::types::value::thread::LuaThread;
use crate::types::varargs::Varargs;

/// Closes specified coroutine, which must be dead or suspended
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-coroutine.close>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn close(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let coroutine = params.coerce::<LuaThread>(0)?;
    match lua_vm.close_coroutine(&coroutine) {
        Ok(()) => Ok(Varargs::from(true)),
        Err(error) => Ok(Varargs::from((false, error.message())))
    }
}

/// Creates new coroutine from specified function
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-coroutine.create>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn create(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    Ok(Varargs::from(
        LuaThread::new(
            params.coerce::<LuaFunction>(0)?
        )
    ))
}

/// Tests whether the specified coroutine (or the current coroutine if called without arguments) can currently yield
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-coroutine.isyieldable>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn isyieldable(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let coroutine = params.coerce_opt::<LuaThread>(0)?
        .unwrap_or_else(|| lua_vm.current_coroutine());
    Ok(Varargs::from(lua_vm.is_coroutine_yieldable(&coroutine)))
}

/// Resumes or starts specified coroutine, with optional specified arguments
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-coroutine.resume>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn resume(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let coroutine = params.coerce::<LuaThread>(0)?;
    match lua_vm.resume_coroutine(&coroutine, &params[1..]) {
        Ok(values) => Ok(Varargs::prepend(true, values)),
        Err(err) => Ok(Varargs::from((false, err.message())))
    }
}

/// Returns the currently running coroutine, and the boolean true if it is the outer 'main' thread
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-coroutine.running>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn running(lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let coroutine = lua_vm.current_coroutine();
    let is_main = lua_vm.is_coroutine_main(&coroutine);
    Ok(Varargs::from((coroutine, is_main)))
}

/// Returns statis of specified coroutine
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-coroutine.status>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn status(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let coroutine = params.coerce::<LuaThread>(0)?;
    if coroutine.is_dead() {
        Ok(Varargs::from("dead"))
    } else if !coroutine.has_started() {
        Ok(Varargs::from("suspended"))
    } else {
        todo!()
    }
}

/// Creates a coroutine 'wrapper' from a specified function; This wrapper is a function that resumes the created coroutine each time it is called
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-coroutine.wrap>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn wrap(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let coroutine = LuaThread::new(params.coerce::<LuaFunction>(0)?);
    Ok(Varargs::from(
        LuaFunction::RUST_CLOSURE(RustClosure::new(
            "wrapped-coroutine",
            move |lua_vm: &mut LuaVM, params: &[LuaValue]| {
                lua_vm.resume_coroutine(&coroutine, params)
            }
        ))
    ))
}

/// Yields the currently running coroutine
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-coroutine.yield>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn coroutine_yield(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    lua_vm.yield_current_coroutine(params)
}

/// Adds the coroutine library to the global environment of the specified LuaVM.
///
/// # Arguments
///
/// * `lua_vm`: LuaVM to add the coroutine library to
///
/// returns: ()
pub fn insert_coroutine_lib(lua_vm: &mut LuaVM) {
    let table = LuaTable::empty();

    let result: Result<(), InvalidKeyError> = try {
        set_table_func!(table, close);
        set_table_func!(table, create);
        set_table_func!(table, isyieldable);
        set_table_func!(table, resume);
        set_table_func!(table, running);
        set_table_func!(table, status);
        set_table_func!(table, wrap);
        set_table_func!(table, "yield", coroutine_yield);
    };
    result.expect("Raw set with string key should not error!");

    lua_vm.global_env.raw_set("coroutine", table.clone());
    lua_vm.modules.insert("coroutine", table);
}