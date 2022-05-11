//! Top level module for Lua's standard library
//!
//! See individual submodules for documentation of those libraries
//!
//! Libraries are loaded by adding them to the [`LuaVM.global_env`] table and [`LuaVM::modules`] map.
//! See source code of [`math::insert_math_lib`] for example

use crate::vm::LuaVM;

pub mod basic;
pub mod coroutine;
pub mod debug;
pub mod io;
pub mod math;
pub mod os;
pub mod package;
pub mod string;
pub mod table;
pub mod utf8;

///  Adds all standard libraries to the global environment of the specified LuaVM.
///
/// **Warning: This includes IO and OS libraries**
///
/// # Arguments
///
/// * `lua_vm`: LuaVM to add libraries to
///
/// returns: ()
///
#[allow(dead_code)]
pub fn insert_stdlib(lua_vm: &mut LuaVM) {
    basic::insert_basic_lib(lua_vm);
    coroutine::insert_coroutine_lib(lua_vm);
    package::insert_package_lib(lua_vm);
    string::insert_string_lib(lua_vm);
    utf8::insert_utf8_lib(lua_vm);
    table::insert_table_lib(lua_vm);
    math::insert_math_lib(lua_vm);
    io::insert_io_lib(lua_vm);
    os::insert_os_lib(lua_vm);
}