//! Module containing the Lua package functions library
//!
//! Available features vary depending on enabled cargo features

use crate::vm::LuaVM;
use crate::error::{LuaError, IntoArgumentError, GenericError};
use crate::types::value::LuaValue;
use crate::types::varargs::Varargs;
use crate::types::value::table::LuaTable;
use crate::types::value::function::{LuaFunction, LuaClosure};
use crate::types::value::function::RustFunction;
use crate::types::parameters::LuaParameters;
use crate::types::value::string::LuaString;
use std::ffi::{OsString, OsStr};
use std::fs::File;
use crate::lua_func;
use crate::types::CoerceFrom;
use crate::compiler::{DefaultCompiler, LuaCompiler};
use crate::types::upvalue::Upvalue;
use std::path::Path;


/// Loads specified Lua module
///
/// Rust callers may instead load modules to [`LuaVM::global_env`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-require>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn require(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let module_name = params.coerce::<LuaString>(0)?;
    // If a builtin library, try loading those from the luaVM environment
    // TODO: Maybe replace with a more generic luaVM.modules.get() call, rather than matching the name?
    if let Some(name) = module_name.try_utf8().ok() {
        match name {
            "coroutine" | "package" | "string" | "utf8" | "table" | "math" | "io" | "os" | "debug" => {
                if let Some(value) = lua_vm.modules.get(name) {
                    return Ok(Varargs::from(value.clone()));    // TODO: Maybe set package.loaded? And move this below that check?
                }
            }
            _ => {}
        }
    }

    if let Some(package_module) = lua_vm.modules.get("package") {
        let loaded_table = package_module.raw_get("loaded");

        let module = loaded_table.index(
            &params[0], /* This is allowed because of the coerce-to-string @ line 1 of this function requires this value to exist */
            &lua_vm.metatables,
        ).error_for_argument(0)?;   // TODO: Verify that the rest of this function acknowledges metamethods
        if module.is_truthy() {    // if loaded[module] is false, then we also try to reload it. Tests expect this for unclear reason.
            return Ok(Varargs::from(module));
        }

        let searchers = package_module.raw_get("searchers");

        let mut error_value = Vec::from("module '");
        error_value.extend_from_slice(module_name.as_bytes());
        error_value.extend_from_slice(b"' not found:");

        for (_, search_function) in LuaTable::coerce_from(&searchers).map_err(|_| LuaError::new("package.searchers must be a table"))?.iter() {
            debug_assert!(params.len() >= 1);   // Coercion to module_name string should fail if there are no arguments provided
            let result = search_function.call(lua_vm, &params[0..=0])?;

            let [loader, data] = result.into_array::<2>();

            if let LuaValue::FUNCTION(loader_function) = &loader {
                let varargs = loader_function.call(lua_vm, &[LuaValue::from(module_name.clone()), data.clone()])
                    .map_err(|error| error.map(|message| GenericError::String(format!("error loading module '{}':\n\t{}", module_name, message))))?;
                let module = varargs.into_first();
                if module != LuaValue::NIL {
                    loaded_table.set_index(params[0].clone(), module, &lua_vm.metatables)?;
                } else if loaded_table.index(&params[0], &lua_vm.metatables)? == LuaValue::NIL {    // If package.loaded[module_name] has not been set by the loader function
                    loaded_table.set_index(params[0].clone(), LuaValue::BOOLEAN(true), &lua_vm.metatables)?;
                }

                let loaded = loaded_table.index(&params[0], &lua_vm.metatables)?;
                return Ok(Varargs::from([loaded, data]));
            } else if let LuaValue::STRING(message) = loader {
                error_value.extend_from_slice(message.as_bytes());
            } else {
                // Quietly drop error; Treat as nil
            }
        }

        Err(LuaError::with_message(LuaString::from(error_value)))?
    } else {
        Err(LuaError::new("Package module was unloaded!"))?
    }
}


/// Package loading config
///
/// Uses `/` as directory separator on all platforms; Platform-specific directory separator is substituted in by package functions
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-package.config>
pub const CONFIG: &str = "/\n;\n?\n!\n-";

/// Cache of loaded packages
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-package.loaded>
pub const LOADED: fn() -> LuaTable = LuaTable::empty;

/// Loads specified C library as lua module
///
/// NOTE: This is currently unimplemented, and likely to never be supported
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-package.loadlib>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn loadlib(_lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    Err(LuaError::new("Cannot load C libraries"))
}


/// Path template; Specifies which files and paths will be searched for modules
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-package.path>
pub const PATH: &str = "?.lua";


/// Package searchers
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-package.preload>
pub const PRELOAD: fn() -> LuaTable = LuaTable::empty;

/// Module searcher that looks into [`PRELOAD`] for a module loader
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
fn preload_searcher(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let module_name = params.coerce::<LuaString>(0)?;

    if let Some(package_module) = lua_vm.modules.get("package") {
        if let LuaValue::TABLE(table) = package_module.raw_get("preload") {
            let loader = table.raw_get(module_name.clone());
            if loader != LuaValue::NIL {
                Ok(Varargs::from((loader, ":preload:")))
            } else {
                Ok(Varargs::from(format!("\n\tno field package.preload['{}']", module_name)))
            }
        } else {
            Err(LuaError::new("\n\tpackage.preload must be a table!"))?
        }
    } else {
        Err(LuaError::new("\n\t'package' must be a table!"))?
    }
}

/// Module loader that loads from a given path
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[cfg(feature = "file-io")]
fn file_loader(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let module_name = params.get_or_nil(0);
    let module_path = params.coerce::<LuaString>(1)?;
    let path = module_path.try_utf8()
        .map(OsStr::new)
        .map_err(|_| GenericError::Str("path must be utf-8 encoded!").error_for_argument(0))?;

    match File::open(path) {
        Ok(mut file) => {
            match DefaultCompiler::compile_and_load(&mut file, Some(module_path.clone()), lua_vm.max_script_size()) {
                Ok(proto) => {
                    LuaFunction::LUA_CLOSURE(LuaClosure::new(proto, vec![Upvalue::new_closed(LuaValue::from(lua_vm.global_env.clone()))]))
                        .call(lua_vm, &[module_name.clone(), LuaValue::from(module_path)])
                }
                Err(err) => Err(GenericError::String(format!("{}", err)))?,
            }
        }
        Err(err) => Err(GenericError::String(format!("{}", err)))?,
    }
}

/// Module searcher that searches filesystem according to [`PATH`] template
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[cfg(feature = "file-io")]
fn lua_file_searcher(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let module_name = params.coerce::<LuaString>(0)?;

    if let Some(package_module) = lua_vm.modules.get("package") {
        let path = package_module.raw_get("path");

        if let LuaValue::STRING(_) = &path {
            let [module_path, err] = searchpath(lua_vm, &[module_name.into(), path])?.into_array();

            if let LuaValue::STRING(module_path_string) = &module_path {
                let path = module_path_string.try_utf8()
                    .map(OsStr::new)
                    .map_err(|_| LuaError::new("Path must be valid UTF-8"))?;

                if Path::new(&path).exists() {
                    return Ok(Varargs::from((LuaFunction::RUST_FUNCTION(RustFunction::from_parts("file_loader", file_loader)), module_path)));
                }
            }

            Ok(Varargs::from(err))
        } else {
            Err(LuaError::new("package.path must be a string"))?
        }
    } else {
        Err(LuaError::new("package.preload must be a table!"))?
    }
}

/// Searcher functions
///
/// Included searchers vary depending on enabled cargo features; The file searcher is only available with the "file-io" feature.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-package.searchers>
pub const SEARCHERS: fn() -> LuaTable = || {
    LuaTable::of_list([
        lua_func!(preload_searcher),
        #[cfg(feature = "file-io")] {
            lua_func!(lua_file_searcher)
        },
    ])
};

fn replace_bytes(search_text: &[u8], pattern: &[u8], replacement: &[u8]) -> Vec<u8> {
    if pattern.len() > 0 {
        let mut text_buffer = Vec::with_capacity(search_text.len());   // We assume sep.len() == rep.len()
        let mut find_buffer = Vec::with_capacity(pattern.len());

        for byte in search_text {
            if *byte == pattern[find_buffer.len()] {
                find_buffer.push(*byte);
                if find_buffer.len() == pattern.len() {
                    find_buffer.truncate(0);
                    text_buffer.extend_from_slice(replacement);
                }
            } else {
                text_buffer.extend_from_slice(&find_buffer[..]);    // No-op when find_buffer is empty
                find_buffer.truncate(0);
                text_buffer.push(*byte);
            }
        };

        text_buffer
    } else {
        Vec::from(search_text)
    }
}

// TODO: Implement additional file path flag to encode UTF-8 cleanly
// TODO: Exact file path handling "magic" to a different function for better documentation
// TODO: This might look for empty-string paths?
// This function is quite ugly and could use some cleanup; A lot of vectors are created and likely not needed

/// Searched for module in specified path
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-package.searchpath>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[cfg(feature = "file-io")]
pub fn searchpath(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let name = params.coerce::<LuaString>(0)?;
    let paths = params.coerce::<LuaString>(1)?;

    let sep = params.coerce::<LuaString>(2);    // Split for borrowing reasons
    let sep = sep.as_ref().map(LuaString::as_bytes).unwrap_or(b".");

    let rep = params.coerce::<LuaString>(3);    // Ditto
    let rep = rep.as_ref().map(LuaString::as_bytes).unwrap_or(&[std::path::MAIN_SEPARATOR as u8]);

    let name = replace_bytes(name.as_bytes(), sep, rep);
    let mut files = <[u8]>::split(paths.as_bytes(), |b| *b == b';')
        .map(|path|{
            if std::path::MAIN_SEPARATOR != '/' {
                let path = replace_bytes(path, b"/", &[std::path::MAIN_SEPARATOR as u8]);
                replace_bytes(&*path, b"?", &name[..])
            } else {
                replace_bytes(path, b"?", &name[..])
            }
        })
        .collect::<Vec<Vec<u8>>>();

    for (index, file) in files.iter().enumerate() {
        let os_string = {
            #[cfg(windows)] {
                use std::os::windows::ffi::OsStringExt;
                let file_wide = file.iter().map(|byte| *byte as u16).collect::<Vec<u16>>();

                <OsString as OsStringExt>::from_wide(&file_wide[..])
            }

            #[cfg(unix)] {
                use std::os::unix::ffi::OsStrExt;
                <OsString as OsStringExt>::from_vec(file)
            }
        };
        if let Ok(open_file) = File::open(os_string) {
            drop(open_file);
            let mut file = files.swap_remove(index);
            if std::path::MAIN_SEPARATOR != '/' {
                file = replace_bytes(&*file, &[std::path::MAIN_SEPARATOR as u8], b"/"); // Swap separators back before returning path
            }
            return Ok(Varargs::from(file));
        }
    }

    let mut error = Vec::new();
    for mut file in files {
        if std::path::MAIN_SEPARATOR != '/' {
            file = replace_bytes(&*file, &[std::path::MAIN_SEPARATOR as u8], b"/"); // Swap separators back before returning path
        }
        error.extend_from_slice(b"\n\tno file '");
        error.extend_from_slice(&file[..]);
        error.push(b'\'');
    }

    Ok(Varargs::from((LuaValue::NIL, error)))
}

/// Adds the package library to the global environment of the specified LuaVM.
///
/// # Arguments
///
/// * `lua_vm`: LuaVM to add the package library to
///
/// returns: ()
pub fn insert_package_lib(lua_vm: &mut LuaVM) {
    let table = LuaTable::empty();

    table.raw_set("config", CONFIG);
    table.raw_set("loaded", LOADED());
    set_table_func!(table, loadlib);
    table.raw_set("path", PATH);
    table.raw_set("preload", PRELOAD());
    table.raw_set("searchers", SEARCHERS());
    set_table_func!(table, searchpath);

    lua_vm.global_env.raw_set("require", lua_func!(require));
    lua_vm.global_env.raw_set("package", table.clone());
    lua_vm.modules.insert("package", table);
}
