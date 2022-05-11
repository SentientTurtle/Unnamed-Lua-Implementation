//! Module for various macros

/// Boilerplate reduction macro.
/// Converts a function reference to a [LuaFunction](crate::types::value::function::LuaFunction) type
///
/// Example
/// ```
/// use lua_interpreter::lua_func;
/// use lua_interpreter::types::value::function::LuaFunction;
///
/// pub fn example_function(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> { Ok(Varargs::empty()) }
///
/// let lua_function: LuaFunction = lua_func!(example_function);
/// ```
#[macro_export]
macro_rules! lua_func {
    ($func:ident) => {
        crate::types::value::function::LuaFunction::RUST_FUNCTION(crate::types::value::function::RustFunction::from_parts(stringify!($func), $func))
    };
}

/// Boilerplate reduction macro.
/// Sets function in LuaVM's global environment
///
/// Example
/// ```
/// use lua_interpreter::set_global_func;
/// use lua_interpreter::types::value::function::LuaFunction;
///
/// pub fn example_function(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> { Ok(Varargs::empty()) }
///
/// pub fn insert_math_lib(lua_vm: &mut LuaVM) {
///     set_global_func!(lua_vm, example_function);
///     set_global_func!(lua_vm, "example_function2", example_function);
/// }
/// ```
#[macro_export]
macro_rules! set_global_func {
    ($luaVM:ident, $func:ident) => {
        set_global_func!($luaVM, stringify!($func), $func)
    };
    ($luaVM:ident, $name:expr, $func:ident) => {
        $luaVM.global_env.raw_set($name, LuaValue::FUNCTION(lua_func!($func)));
    };
}

/// Boilerplate reduction macro.
/// Sets function in specified table
///
/// Example
/// ```
/// use lua_interpreter::set_table_func;
/// use lua_interpreter::types::value::function::LuaFunction;
///
/// pub fn example_function(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> { Ok(Varargs::empty()) }
///
/// pub fn insert_math_lib(lua_vm: &mut LuaVM) {
///     let table = LuaTable::empty();
///     set_table_func!(table, example_function);
///     set_table_func!(table, "example_function2", example_function);
/// }
/// ```
#[macro_export]
macro_rules! set_table_func {
    ($table:ident, $func:ident) => {
        set_table_func!($table, stringify!($func), $func)
    };

    ($table:ident, $name:expr, $func:ident) => {
        $table.raw_set($name, lua_func!($func));
    };
}