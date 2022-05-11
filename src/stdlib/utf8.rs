//! Module containing the Lua utf8 functions library
//!
//! NOTE: This implementation of the lua utf8 library only supports up to 4-byte UTF-8 byte sequences; It does not support the Lua reference implementation's (non-spec) UTF-8 sequences up to 6 bytes in length.

use std::convert::TryFrom;
use std::iter::FromIterator;
use crate::constants::types::LUA_INT;
use crate::error::{GenericError, IntoArgumentError, LuaError};
use crate::types::parameters::LuaParameters;
use crate::types::value::LuaValue;
use crate::types::value::string::LuaString;
use crate::vm::LuaVM;
use crate::types::value::table::LuaTable;
use crate::types::varargs::Varargs;

/// Creates string from specified codepoints
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-utf8.char>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn char(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let mut buffer = Vec::new();
    for index in 0..params.len() {
        let integer = params.coerce::<LUA_INT>(index)?;
        let character = u32::try_from(integer)
            .map_err(|_| GenericError::Str("value out of range"))
            .and_then(|codepoint| {
                char::try_from(codepoint)
                    .map_err(|_| GenericError::Str("value out of range"))
            })
            .error_for_argument(index)?;
        buffer.push(character);
    }
    Ok(Varargs::from(String::from_iter(buffer))) // We could save some allocations by not using a buffer, but this comes at significant code-readability cost.
}

/// Lua string pattern that matches exactly 1 utf-8 byte sequence in a valid utf-8 string
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-utf8.charpattern>
pub const CHARPATTERN: &'static [u8] = b"[\0-\x7F\xC2-\xFD][\x80-\xBF]*";

/// Iterator that yields next codepoint in a string, see [`codes`]
fn next_codepoint(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let string = params.coerce::<LuaString>(0)?;
    let index_parameter = params.coerce::<LUA_INT>(1)?;
    if index_parameter < 0 || usize::try_from(index_parameter).map(|index| index >= string.len()).unwrap_or(false) {
        Ok(Varargs::from(()))
    } else {
        let bytes = string.as_bytes();
        let index = {
            let mut byte_index = usize::try_from(index_parameter).unwrap();
            if byte_index > 0 {
                byte_index -= 1;    // Decrement one; Lua is 1-indexed

                // Skip utf8 sequence at current byte
                if let Some(byte) = bytes.get(byte_index) {
                    byte_index += match *byte {
                        0b0000_0000..=0b0111_1111 => 1,
                        0b1100_0000..=0b1101_1111 => 2,
                        0b1110_0000..=0b1110_1111 => 3,
                        0b1111_0000..=0b1111_1111 => 4,
                        byte => {
                            Err(GenericError::String(format!("invalid UTF-8 code: Expected UTF8 start byte sequence at {} found {:X}", byte_index + 1, byte)))?
                        }
                    };
                } else {
                    Err(GenericError::String(format!("invalid UTF-8 code: Premature end of string @ {}", byte_index + 1)))?
                }
            }
            byte_index
        };
        if let Some(byte) = bytes.get(index) {
            let slice = match *byte {
                0b0000_0000..=0b0111_1111 => &bytes[index..=index],
                0b1100_0000..=0b1101_1111 => {
                    if index + 1 <= bytes.len() {
                        &bytes[index..=index + 1]
                    } else {
                        Err(GenericError::String(format!("invalid UTF-8 code: Premature end of string at {}", index + 2)))?
                    }
                }
                0b1110_0000..=0b1110_1111 => {
                    if index + 2 <= bytes.len() {
                        &bytes[index..=index + 2]
                    } else {
                        Err(GenericError::String(format!("invalid UTF-8 code: Premature end of string at {}", index + 2)))?
                    }
                }
                0b1111_0000..=0b1111_1111 => {
                    if index + 3 <= bytes.len() {
                        &bytes[index..=index + 3]
                    } else {
                        Err(GenericError::String(format!("invalid UTF-8 code: Premature end of string at {}", index + 2)))?
                    }
                }
                byte => {
                    Err(GenericError::String(format!("invalid UTF-8 code: Expected UTF8 start byte sequence at {} found {:X}", index + 1, byte)))?
                }
            };

            let char = match std::str::from_utf8(slice) {
                Ok(string) => string.chars().next().unwrap(),   // We create a string from at least 1 byte, so 1 character must be present or parsing will fail.
                Err(_) => Err(GenericError::String(format!("invalid UTF-8 code: Invalid UTF8 sequence at {}", index + 1)))?
            };

            Ok(Varargs::from((index + 1, char as LUA_INT)))
        } else {
            Ok(Varargs::from(()))
        }
    }
}

/// Iterates over codepoints in string
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-utf8.codes>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn codes(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    Ok(Varargs::from((lua_func!(next_codepoint), params.coerce::<LuaString>(0)?, 0usize)))
}

/// Returns codepoints in (sub)string
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-utf8.codepoints>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn codepoint(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let string = params.coerce::<LuaString>(0)?;
    let start_relative = params.coerce_opt::<LUA_INT>(1)?.unwrap_or(1);
    if string.len() == 0 { return Ok(Varargs::empty()); } // Check before parsing start/end, as that may error
    let start = string.relative_index(start_relative)?;
    let mut end = string.relative_index(params.coerce_opt::<LUA_INT>(2)?.unwrap_or(start_relative))?;
    if start > end { return Ok(Varargs::empty()); }
    match string {
        LuaString::UNICODE(string) if start == 0 && end.checked_add(1).contains(&string.len()) => {
            Ok(Varargs::from(string.chars().map(|c| LuaValue::from(c as LUA_INT)).collect::<Vec<LuaValue>>()))
        }
        _ => {
            let mut bytes = string.as_bytes();
            // Extend end to include any trailing utf8 continuation bytes of the last utf-8 character in the slice
            end = match bytes[end] {
                0b0000_0000..=0b0111_1111 => end,
                0b1100_0000..=0b1101_1111 => end + 1,
                0b1110_0000..=0b1110_1111 => end + 2,
                0b1111_0000..=0b1111_1111 => end + 3,
                _ if end > start => {
                    match bytes[end - 1] {
                        0b0000_0000..=0b0111_1111 => end,
                        0b1100_0000..=0b1101_1111 => end,
                        0b1110_0000..=0b1110_1111 => end + 1,
                        0b1111_0000..=0b1111_1111 => end + 2,
                        _ if end - 1 > start => {
                            match bytes[end - 2] {
                                0b0000_0000..=0b0111_1111 => end,
                                0b1100_0000..=0b1101_1111 => end,
                                0b1110_0000..=0b1110_1111 => end,
                                0b1111_0000..=0b1111_1111 => end + 1,
                                _ => end  // In case of invalid byte sequence, don't change 'end'. We'll just pass an invalid utf8 slice and error later.
                            }
                        }
                        _ => end
                    }
                }
                _ => end
            };
            if end >= bytes.len() {
                debug_assert!(bytes.len() > 0); // There is no valid `start` or `end` value for strings of length 0, so earlier string.relative_index will error.
                end = bytes.len() - 1;
            }
            bytes = &bytes[start..=end];
            match std::str::from_utf8(bytes) {
                Ok(string) => {
                    Ok(Varargs::from(string.chars().map(|c| LuaValue::from(c as LUA_INT)).collect::<Vec<LuaValue>>()))
                }
                Err(err) => Err(LuaError::with_string(format!("invalid UTF-8 code at byte {}", start + err.valid_up_to() + 1)))  // +1 as Lua indexes from 1;
            }
        }
    }
}

/// Returns length in codepoints of (sub)string
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-utf8.len>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn len(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let string = params.coerce::<LuaString>(0)?;
    let start_relative = params.coerce_opt::<LUA_INT>(1)?.unwrap_or(1);
    if start_relative == (string.len() + 1) as LUA_INT { return Ok(Varargs::from(0usize)); }
    let start = string.relative_index(start_relative)?;
    let mut end = string.relative_index(params.coerce_opt::<LUA_INT>(2)?.unwrap_or(-1))?;

    match string {
        LuaString::UNICODE(string) if start == 0 && end.checked_add(1).contains(&string.len()) => Ok(Varargs::from(string.chars().count())),
        _ => {
            let bytes = string.as_bytes();
            while end.saturating_add(1) < string.len() { // Extend end to include any trailing utf8 continuation bytes
                if bytes[end + 1] & 0b1100_0000 == 0b1000_0000 {
                    end += 1;
                } else {
                    break;
                }
            }
            match std::str::from_utf8(&bytes[start..=end]).map(|string| string.chars().count()).map_err(|err| err.valid_up_to()) {  // Optimization candidate: This does two iterations through the string where 1 will suffice.
                Ok(total) => Ok(Varargs::from(total)),
                Err(index) => Ok(Varargs::from((Varargs::fail_value(), index + 1)))
            }
        }
    }
}

/// Calculates byte-offset for a given character-offset
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-utf8.offset>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn offset(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let string = params.coerce::<LuaString>(0)?;
    let character_index = params.coerce::<LUA_INT>(1)?;
    let index_opt = params.coerce_opt::<LUA_INT>(2)?;

    if character_index == 0 {
        if string.len() == 0 && index_opt.unwrap_or(1) == 1 {
            Ok(Varargs::from(1usize))
        } else {
            let mut index = string.relative_index(index_opt.unwrap_or(1))?;
            let bytes = string.as_bytes();
            while index > 0 && (bytes[index] & 0b1100_0000 == 0b1000_0000) {
                index -= 1;
            }
            if bytes[index] & 0b1100_0000 != 0b1000_0000 {
                Ok(Varargs::from(index + 1))
            } else {
                Ok(Varargs::fail())
            }
        }
    } else {
        let (index, slice) = if character_index > 0 {
            let index = if index_opt.contains(&((string.len() + 1) as LUA_INT)) {
                string.len()
            } else {
                string.relative_index(index_opt.unwrap_or(1))?
            };
            (index, &string.as_bytes()[index..])
        } else {
            let index = match index_opt {
                Some(index) if index != (string.len() + 1) as LUA_INT => {
                    string.relative_index(index)?
                }
                Some(_) | None => {
                    string.len()
                }
            };
            (index, &string.as_bytes()[..index])
        };
        match std::str::from_utf8(slice) {
            Ok(utf8_slice) => {
                if character_index > 0 {
                    // Chain here for a special case; utf8.offset will also return the starting index of the next character to be appended to this string
                    if let Some((char_index, _)) = utf8_slice.char_indices().chain(std::iter::once((string.len() - index, '\0'))).skip((character_index - 1) as usize).next() {
                        Ok(Varargs::from(char_index + index + 1))
                    } else {
                        Ok(Varargs::fail())
                    }
                } else {
                    if let Some((char_index, _)) = utf8_slice.char_indices().rev().skip((-1 - character_index) as usize).next() {
                        Ok(Varargs::from(char_index + 1))
                    } else {
                        Ok(Varargs::fail())
                    }
                }
            }
            Err(_) => Err(LuaError::new("initial position is a continuation byte"))
        }
    }
}


/// Adds the utf8 library to the global environment of the specified LuaVM.
///
/// # Arguments
///
/// * `lua_vm`: LuaVM to add the table library to
///
/// returns: ()
pub fn insert_utf8_lib(lua_vm: &mut LuaVM) {
    let table = LuaTable::empty();

    set_table_func!(table, char);
    table.raw_set("charpattern", CHARPATTERN);
    set_table_func!(table, codes);
    set_table_func!(table, codepoint);
    set_table_func!(table, len);
    set_table_func!(table, offset);

    lua_vm.global_env.raw_set("utf8", table.clone());
    lua_vm.modules.insert("utf8", table);
}