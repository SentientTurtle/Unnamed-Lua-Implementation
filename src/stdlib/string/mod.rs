//! Module containing the Lua "string" library

use crate::lua_func;
use crate::vm::LuaVM;
use crate::error::{GenericError, InvalidValueError, IntoArgumentError, LuaError, InvalidConcatenationError};
use crate::stdlib::string::pattern_old::compile_pattern;
use crate::util::{Union3};
use regex::bytes::Captures;

use crate::types::value::LuaValue;
use crate::types::value::string::LuaString;
use crate::types::varargs::Varargs;
use crate::types::value::function::{LuaFunction, LuaClosure, RustClosure};
use crate::types::value::table::LuaTable;
use crate::types::parameters::LuaParameters;
use crate::types::{LuaType, CoerceFrom};
use crate::constants::types::{LUA_INT, LUA_INT_UNSIGNED};
use std::fmt;
use nom::{FindSubstring, AsBytes, IResult};
use nom::multi::{many0, many1};
use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::character::complete::none_of;
use nom::bytes::complete::{tag, is_a};
use nom::sequence::tuple;
use std::fmt::{Debug, Formatter};
use core::iter;

pub mod pattern;


/// Returns byte values of the passed (sub)string
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-string.byte>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn byte(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let string = params.coerce::<LuaString>(0)?;
    let start = params.coerce_opt::<LUA_INT>(1)?.unwrap_or(1);
    let end = params.coerce_opt::<LUA_INT>(2)?.unwrap_or(start);

    let return_vec: Vec<LuaValue> = string.slice_bytes(start..=end)?.into_iter().map(|b| LuaValue::from(*b as usize)).collect();
    Ok(Varargs::from(return_vec))
}


/// Creates a string from a sequence of unicode codepoint integers
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.char>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn char(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let mut vec = Vec::new();
    for (index, param) in params.iter().enumerate() {
        match param {
            LuaValue::NUMBER(num) if (0..255_i64).contains(&num.try_int().unwrap_or(-1)) => { // Check that "num" is an integer between 0 and 255
                vec.push(num.try_int().unwrap() as u8);
            }
            _ => return Err(
                InvalidValueError {
                    expected: "byte (0-254)",
                    found: params.get_or_nil(index).type_name(),
                }.error_for_argument(index)
            )?
        }
    }

    Ok(Varargs::from(LuaString::from(vec)))
}

/// Creates a binary string of the bytecode representation of the passed function, optionally stripping debug information
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.dump>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn dump(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> { // TODO: Implement strip debug info option
    let closure = params.coerce::<LuaClosure>(0)?;
    let mut bytes = Vec::new();
    use crate::bytecode::dumper::Dumper;
    if let Err(error) = crate::bytecode::dumper::LE64Dumper::dump_chunk(closure.prototype(), &mut bytes) {
        Err(LuaError::with_message(format!("{}", error)))
    } else {
        Ok(Varargs::from(bytes))
    }
}

/// Looks for first instance of the specified pattern/string
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.find>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn find(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let string = params.coerce::<LuaString>(0)?;
    let pattern = params.coerce::<LuaString>(1)?;
    let index = params.coerce_opt::<LUA_INT>(2)?.unwrap_or(1);

    let plain_match = params.get(3).map(|v| if let LuaValue::BOOLEAN(b) = v { *b } else { false }).unwrap_or(false);
    if !plain_match {
        let regex = compile_pattern(pattern.as_bytes()).error_for_argument(1)?;
        match regex.captures(string.slice_bytes(index..)?) {
            Some(captures) => {
                let mut return_values = Vec::new();
                let m = captures.get(0).expect("Captures must always match whole group");
                return_values.push(LuaValue::from(m.start() + 1));
                return_values.push(LuaValue::from(m.end() + 1));
                for capture in captures.iter().skip(1) {
                    if let Some(m) = capture {
                        return_values.push(LuaValue::from(m.as_bytes()))
                    }
                }
                Ok(Varargs::from(return_values))
            }
            None => Ok(Varargs::nil()),
        }
    } else {
        match string.as_bytes().find_substring(pattern.as_bytes()) {
            Some(m) => Ok(Varargs::from((m + 1, m + pattern.len()))),
            None => Ok(Varargs::nil())
        }
    }
}

#[allow(unused)]
#[derive(Debug)]
struct FormatFlags {
    left_justify: bool,
    force_sign: bool,
    pad_sign: bool,
    hash: bool, // The "#" formatting flag serves multiple purposes, so a generic name is used
}


enum FormatElement {
    RawString(Vec<u8>),
    Format {
        flags: FormatFlags,
        width: Option<(usize, bool)>,
        precision: Option<usize>,
        specifier: u8,
    },
}

impl Debug for FormatElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FormatElement::RawString(vec) => {
                f.debug_tuple("FormatElement::String").field(&String::from_utf8_lossy(&vec[..])).finish()
            }
            FormatElement::Format { flags, width, precision, specifier } => {
                f.debug_struct("FormatElement")
                    .field("flags", flags)
                    .field("width", width)
                    .field("precision", precision)
                    .field("specifier", &(*specifier as char))
                    .finish()
            }
        }
    }
}

/// Type alias
type StringFormat = Vec<FormatElement>;

/// Parses a format string and provides a StringFormat (Vec<FormatElement>)
///
/// # Arguments
///
/// * `format`: Format string, in bytes, to parse
///
/// returns: Result<(&[u8], Vec<FormatElement, Global>), Err<Error<I>>>
///
fn parse_format(format: &[u8]) -> IResult<&[u8], StringFormat> {
    many0(
        alt((
            map(
                many1(none_of("%")),
                |v| FormatElement::RawString(v.iter().map(|c| *c as u8).collect()),
            ),
            map(
                tag("%%"),
                |_| FormatElement::RawString(Vec::from(&b"%"[..])),
            ),
            map(
                tuple((
                    tag("%"),
                    map(
                        opt(is_a("-+ #")),
                        |v: Option<&[u8]>| {
                            let v = v.unwrap_or(b"");
                            FormatFlags {
                                left_justify: v.contains(&b'-'),
                                force_sign: v.contains(&b'+'),
                                pad_sign: v.contains(&b' '),
                                hash: v.contains(&b'#'),
                            }
                        },
                    ),
                    map(
                        opt(is_a("0123456789")),
                        |num: Option<&[u8]>| {
                            let num = num.unwrap_or(b"");
                            if num.len() > 0 {
                                Some((std::str::from_utf8(num).unwrap().parse::<usize>().unwrap(), num[0] == 0))
                            } else {
                                None
                            }
                        },
                    ),
                    opt(tuple((
                        tag("."),
                        map(
                            is_a("0123456789"),
                            |num: &[u8]| if num.len() > 0 {
                                Some(std::str::from_utf8(num).unwrap().parse::<usize>().unwrap())
                            } else {
                                Some(0)
                            },
                        )
                    ))),
                    alt((
                        tag("d"), tag("i"), tag("u"), tag("o"), tag("x"), tag("X"), tag("f"), tag("F"),
                        tag("e"), tag("E"), tag("g"), tag("G"), tag("a"), tag("A"), tag("c"), tag("s"),
                        tag("p"), tag("n"), tag("q")
                    ))
                )),
                |tuple| FormatElement::Format {
                    flags: tuple.1,
                    width: tuple.2,
                    precision: tuple.3.and_then(|t| t.1),
                    specifier: tuple.4[0],
                },
            )
        ))
    )(format)
}


/// String format function, equivalent to sprintf
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.format>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn format(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let format_string = params.coerce::<LuaString>(0)?;
    match parse_format(format_string.as_bytes()) {
        Ok((_, format)) => {
            let mut index = 1;
            let mut results = Vec::new();
            for element in format {
                match element {
                    FormatElement::RawString(string) => results.extend_from_slice(&string[..]),
                    FormatElement::Format { flags, width: width_options, precision: precision_option, specifier } => {
                        match specifier {
                            b'd' | b'i' => {
                                let value = params.coerce::<LUA_INT>(index)?;
                                index += 1;

                                // Convert value to string without sign
                                let mut string = value.abs().to_string();

                                match (width_options, precision_option) {
                                    (None, Some(precision)) => {
                                        // Pad digits to [precision] with zeros
                                        string = format!("{:0width$}", string, width = precision);
                                        if value < 0 {
                                            string = format!("-{}", string);
                                        } else if flags.force_sign {
                                            string = format!("+{}", string);
                                        }
                                    }
                                    (Some((width, pad_zeros)), None) => {
                                        if flags.left_justify {     // Pads with space regardless of pad_zero flag
                                            // Add sign
                                            if value < 0 {
                                                string = format!("-{}", string);
                                            } else if flags.force_sign {
                                                string = format!("+{}", string);
                                            }
                                            string = format!("{:<width$}", string, width = width);
                                        } else {
                                            if pad_zeros {
                                                string = format!("{:0width$}", string, width = width);
                                                // Add sign
                                                if value < 0 {
                                                    string = format!("-{}", string);
                                                } else if flags.force_sign {
                                                    string = format!("+{}", string);
                                                } else {
                                                    string = format!(" {}", string);
                                                }
                                            } else {
                                                // Add sign
                                                if value < 0 {
                                                    string = format!("-{}", string);
                                                } else if flags.force_sign {
                                                    string = format!("+{}", string);
                                                }
                                                string = format!("{:width$}", string, width = width);
                                            }
                                        }
                                    }
                                    (Some((width, _)), Some(precision)) => {
                                        string = format!("{:0width$}", string, width = precision);
                                        if value < 0 {
                                            string = format!("-{}", string);
                                        } else if flags.force_sign {
                                            string = format!("+{}", string);
                                        }

                                        if flags.left_justify {     // Pads with space regardless of pad_zero flag
                                            string = format!("{:<width$}", string, width = width);
                                        } else {
                                            string = format!("{:width$}", string, width = width);
                                        }
                                    }
                                    (None, None) => {
                                        if value < 0 {
                                            string = format!("-{}", string);
                                        } else if flags.force_sign {
                                            string = format!("+{}", string);
                                        }
                                    }
                                }
                                results.extend_from_slice(string.as_bytes())
                            }
                            // 'u' => {}
                            // 'o' => {}
                            specifier @ (b'x' | b'X') => {
                                let value = params.coerce::<LUA_INT>(index)? as LUA_INT_UNSIGNED;   // Transmute to unsigned
                                index += 1;
                                // Convert value to string without sign
                                let string;

                                match (width_options, precision_option) {
                                    (None, Some(precision)) => {
                                        if flags.hash {
                                            if specifier == b'x' {
                                                if flags.left_justify {
                                                    string = format!("{:#<.precision$x}", value, precision = precision);
                                                } else {
                                                    string = format!("{:#>.precision$x}", value, precision = precision);
                                                }
                                            } else {
                                                if flags.left_justify {
                                                    string = format!("{:#<.precision$X}", value, precision = precision);
                                                } else {
                                                    string = format!("{:#>.precision$X}", value, precision = precision);
                                                }
                                            }
                                        } else {
                                            if specifier == b'x' {
                                                if flags.left_justify {
                                                    string = format!("{:<.precision$x}", value, precision = precision);
                                                } else {
                                                    string = format!("{:>.precision$x}", value, precision = precision);
                                                }
                                            } else {
                                                if flags.left_justify {
                                                    string = format!("{:<.precision$X}", value, precision = precision);
                                                } else {
                                                    string = format!("{:>.precision$X}", value, precision = precision);
                                                }
                                            }
                                        }
                                    }
                                    (Some((width, pad_zeros)), None) => {
                                        if flags.hash {
                                            if specifier == b'x' {
                                                if flags.left_justify {
                                                    if pad_zeros {
                                                        string = format!("{:#<0width$x}", value, width = width);
                                                    } else {
                                                        string = format!("{:#<width$x}", value, width = width);
                                                    }
                                                } else {
                                                    if pad_zeros {
                                                        string = format!("{:#>0width$x}", value, width = width);
                                                    } else {
                                                        string = format!("{:#>width$x}", value, width = width);
                                                    }
                                                }
                                            } else {
                                                if flags.left_justify {
                                                    if pad_zeros {
                                                        string = format!("{:#<0width$X}", value, width = width);
                                                    } else {
                                                        string = format!("{:#<width$X}", value, width = width);
                                                    }
                                                } else {
                                                    if pad_zeros {
                                                        string = format!("{:#>0width$X}", value, width = width);
                                                    } else {
                                                        string = format!("{:#>width$X}", value, width = width);
                                                    }
                                                }
                                            }
                                        } else {
                                            if specifier == b'x' {
                                                if flags.left_justify {
                                                    if pad_zeros {
                                                        string = format!("{:<0width$x}", value, width = width);
                                                    } else {
                                                        string = format!("{:<width$x}", value, width = width);
                                                    }
                                                } else {
                                                    if pad_zeros {
                                                        string = format!("{:>0width$x}", value, width = width);
                                                    } else {
                                                        string = format!("{:>width$x}", value, width = width);
                                                    }
                                                }
                                            } else {
                                                if flags.left_justify {
                                                    if pad_zeros {
                                                        string = format!("{:<0width$X}", value, width = width);
                                                    } else {
                                                        string = format!("{:<width$X}", value, width = width);
                                                    }
                                                } else {
                                                    if pad_zeros {
                                                        string = format!("{:>0width$X}", value, width = width);
                                                    } else {
                                                        string = format!("{:>width$X}", value, width = width);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    (Some((width, pad_zeros)), Some(precision)) => {
                                        if flags.hash {
                                            if specifier == b'x' {
                                                if flags.left_justify {
                                                    if pad_zeros {
                                                        string = format!("{:#<0width$.precision$x}", value, width = width, precision = precision);
                                                    } else {
                                                        string = format!("{:#<width$.precision$x}", value, width = width, precision = precision);
                                                    }
                                                } else {
                                                    if pad_zeros {
                                                        string = format!("{:#>0width$.precision$x}", value, width = width, precision = precision);
                                                    } else {
                                                        string = format!("{:#>width$.precision$x}", value, width = width, precision = precision);
                                                    }
                                                }
                                            } else {
                                                if flags.left_justify {
                                                    if pad_zeros {
                                                        string = format!("{:#<0width$.precision$X}", value, width = width, precision = precision);
                                                    } else {
                                                        string = format!("{:#<width$.precision$X}", value, width = width, precision = precision);
                                                    }
                                                } else {
                                                    if pad_zeros {
                                                        string = format!("{:#>0width$.precision$X}", value, width = width, precision = precision);
                                                    } else {
                                                        string = format!("{:#>width$.precision$X}", value, width = width, precision = precision);
                                                    }
                                                }
                                            }
                                        } else {
                                            if specifier == b'x' {
                                                if flags.left_justify {
                                                    if pad_zeros {
                                                        string = format!("{:<0width$.precision$x}", value, width = width, precision = precision);
                                                    } else {
                                                        string = format!("{:<width$.precision$x}", value, width = width, precision = precision);
                                                    }
                                                } else {
                                                    if pad_zeros {
                                                        string = format!("{:>0width$.precision$x}", value, width = width, precision = precision);
                                                    } else {
                                                        string = format!("{:>width$.precision$x}", value, width = width, precision = precision);
                                                    }
                                                }
                                            } else {
                                                if flags.left_justify {
                                                    if pad_zeros {
                                                        string = format!("{:<0width$.precision$X}", value, width = width, precision = precision);
                                                    } else {
                                                        string = format!("{:<width$.precision$X}", value, width = width, precision = precision);
                                                    }
                                                } else {
                                                    if pad_zeros {
                                                        string = format!("{:>0width$.precision$X}", value, width = width, precision = precision);
                                                    } else {
                                                        string = format!("{:>width$.precision$X}", value, width = width, precision = precision);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    (None, None) => {
                                        if flags.hash {
                                            if specifier == b'x' {
                                                string = format!("{:#x}", value);
                                            } else {
                                                string = format!("{:#X}", value);
                                            }
                                        } else {
                                            if specifier == b'x' {
                                                string = format!("{:x}", value);
                                            } else {
                                                string = format!("{:X}", value);
                                            }
                                        }
                                    }
                                }
                                results.extend_from_slice(string.as_bytes());
                            }
                            // 'f' | 'F' => {}
                            // 'e' | 'E' => {}
                            // 'g' | 'G' => {}
                            // 'a' | 'A' => {}
                            // 'c' => {}
                            b's' => {
                                let value = params.coerce::<LuaString>(index)?; // TODO: This should use tostring rather than mere coercion
                                index += 1;

                                let mut bytes = value.as_bytes();
                                if let Some(precision) = precision_option {
                                    if precision < bytes.len() {
                                        bytes = &bytes[..=precision];
                                    }
                                }

                                if let Some((width, _)) = width_options {
                                    if width > bytes.len() {
                                        if flags.left_justify {
                                            results.extend_from_slice(bytes);
                                            results.extend(iter::repeat(b' ').take(width - bytes.len()));
                                        } else {
                                            results.extend(iter::repeat(b' ').take(width - bytes.len()));
                                            results.extend_from_slice(bytes);
                                        }
                                    }
                                } else {
                                    results.extend_from_slice(bytes)
                                }
                            }
                            // 'q' => {}
                            _ => unreachable!("Unexpected specifier: {}", specifier as char)
                        }
                    }
                };
            }

            Ok(Varargs::from(LuaString::from(results)))
        }
        Err(err) => {
            return Err(GenericError::String(format!("{}", err)).error_for_argument(0))?;
        }
    }
}

/// Iterator over pattern matches
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.gmatch>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn gmatch(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let string = params.coerce::<LuaString>(0)?;
    let pattern = params.coerce::<LuaString>(1)?;

    let regex = compile_pattern(pattern.as_bytes()).error_for_argument(1)?;

    let mut matches = regex.captures_iter(string.as_bytes())
        .map(|m| {
            m.iter()
                .enumerate()
                .map(|(index, capture)| {
                    if let Some(regex_match) = capture {
                        if regex_match.as_bytes().len() == 0 && m.name(&*format!("empty{}", index)).is_some() {    // This is an atrocious hack
                            LuaValue::from(regex_match.start())
                        } else {
                            LuaValue::from(regex_match.as_bytes())
                        }
                    } else {
                        LuaValue::NIL
                    }
                })
                .collect()
        })   // Inefficient, could later be hack-optimized by passing references to `string` (which is heap-allocated and thus outlives this function)
        .collect::<Vec<Vec<LuaValue>>>()
        .into_iter();

    let closure: RustClosure = RustClosure::new(
        "gmatch-closure",
        move |_: &mut LuaVM, _: &[LuaValue]| Ok(Varargs::from(matches.next().unwrap_or(Vec::new()))),
    );
    Ok(Varargs::from(LuaFunction::RUST_CLOSURE(closure)))
}

#[derive(Debug)]
enum ReplacementElement {
    RawString(u8),
    Capture(u8),
}

fn parse_repl(input: &[u8]) -> Result<Vec<ReplacementElement>, GenericError> {
    let mut vec = Vec::new();
    let mut iter = input.iter();
    while let Some(byte) = iter.next() {
        match byte {
            b'%' => {
                if let Some(escaped) = iter.next() {
                    match escaped {
                        b'%' => vec.push(ReplacementElement::RawString(b'%')),
                        b'0'..=b'9' => vec.push(ReplacementElement::Capture(escaped - b'0')),
                        _ => return Err(GenericError::String(format!("Unexpected escape character: {}", *byte as char)))
                    }
                } else {
                    return Err(GenericError::Str("Replacement string ends with escape!"));
                }
            }
            _ => vec.push(ReplacementElement::RawString(*byte))
        }
    }
    Ok(vec)
}

/// Provides pattern substitution
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn gsub(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let string = params.coerce::<LuaString>(0)?;
    let pattern = params.coerce::<LuaString>(1)?;

    let repl = match params.get(2) {
        Some(LuaValue::STRING(s)) => Union3::T(parse_repl(s.as_bytes()).error_for_argument(2)?),
        Some(LuaValue::TABLE(t)) => Union3::U(t),
        Some(LuaValue::FUNCTION(f)) => Union3::V(f),
        _ => Err(InvalidValueError { expected: "string, table or function", found: params.get_or_nil(2).type_name() }.error_for_argument(2))?
    };

    let n = params.coerce::<LUA_INT>(3).ok().map(|i| i.max(0) as usize);

    if n.contains(&0) {
        return Ok(Varargs::from((string.clone(), 0usize)));
    }

    let mut replacement_func_call_error = None;

    let regex = compile_pattern(pattern.as_bytes()).error_for_argument(1)?;
    let mut substitution_count = 0usize;
    let result = match repl {
        Union3::T(repl) => {
            regex.replacen(
                string.as_bytes(),
                n.unwrap_or(usize::MAX),
                |captures: &Captures| {
                    substitution_count += 1;
                    let mut replacement: Vec<u8> = Vec::new();
                    for element in &repl {
                        match element {
                            ReplacementElement::RawString(s) => replacement.push(*s),
                            ReplacementElement::Capture(c) => {
                                if let Some(m) = captures.get(*c as usize) {
                                    replacement.extend_from_slice(m.as_bytes())
                                }
                            }
                        }
                    }
                    replacement
                },
            )
        }
        Union3::U(table) => {
            regex.replacen(
                string.as_bytes(),
                n.unwrap_or(std::usize::MAX),
                |captures: &Captures| {
                    substitution_count += 1;
                    if let Ok(s) = LuaString::coerce_from(&table.raw_get(captures.get(0).expect("gsub replacement without 0-capture!").as_bytes())) {
                        s
                    } else {
                        LuaString::from("")
                    }
                },
            )
        }
        Union3::V(function) => {
            regex.replacen(
                string.as_bytes(),
                n.unwrap_or(std::usize::MAX),
                |captures: &Captures| {
                    substitution_count += 1;
                    if replacement_func_call_error.is_none() {
                        let arguments: Vec<LuaValue> = captures.iter().map(|c| c.map(|m| LuaValue::from(LuaString::from(m.as_bytes()))).unwrap_or(LuaValue::NIL)).collect();
                        let args = if arguments.len() > 1 {
                            &arguments[1..]
                        } else {
                            &arguments[..]
                        };
                        match function.call(lua_vm, args) {
                            Ok(result) => {
                                if let Some(s) = result.opt(0).map(LuaString::coerce_from).and_then(Result::ok) {
                                    s
                                } else {
                                    LuaString::from(captures.get(0).expect("gsub replacement without 0-capture!").as_bytes())
                                }
                            }
                            Err(err) => {
                                replacement_func_call_error.replace(err);
                                LuaString::from("")
                            }
                        }
                    } else {
                        LuaString::from("")
                    }
                },
            )
        }
    };
    Ok(Varargs::from((result.as_bytes(), substitution_count)))
}

/// Returns length of a LuaString, in bytes.
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.len>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn len(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    Ok(Varargs::from(
        params.coerce::<LuaString>(0)?
            .len()
    ))
}

/// Converts string to lowercase
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.lower>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn lower(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let mut string = Vec::from(params.coerce::<LuaString>(0)?.as_bytes());
    string.make_ascii_lowercase();  // Replace with locale dependant lower-case once needed
    Ok(Varargs::from(string))
}

/// Returns captures from single pattern match
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.match>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn string_match(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let string = params.coerce::<LuaString>(0)?;
    let pattern = params.coerce::<LuaString>(1)?;
    let index = params.coerce_opt::<LUA_INT>(2)?.unwrap_or(1);

    let regex = compile_pattern(pattern.as_bytes()).error_for_argument(1)?;
    if let Some(captures) = regex.captures(string.slice_bytes(index..)?) {
        if captures.len() > 1 {
            let collection = captures.iter()
                .skip(1)
                .map(|match_opt| {
                    match_opt.map(|m| LuaValue::from(m.as_bytes()))
                        .unwrap_or(LuaValue::NIL)
                })
                .collect::<Vec<LuaValue>>();

            Ok(Varargs::from(collection))
        } else {
            Ok(Varargs::from(captures.get(0).expect("capture group 0 is always present in a match").as_bytes()))
        }
    } else {
        Ok(Varargs::from(LuaValue::NIL))
    }
}

/// Packs passed variables into a binary-encoded blob according to specified format
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.pack>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn pack(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let _format_string = params.coerce::<LuaString>(0)?;
    debug_assert!(params.len() > 0);    // We can assume this as the above fails with length 0
    let _arguments = &params[1..];

    unimplemented!()
}

/// Returns blob size for pack format
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.pack>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn packsize(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    use std::mem;
    use crate::constants::types::*;

    let format_string = params.coerce::<LuaString>(0)?;
    let mut size = 0usize;
    for byte in format_string.as_bytes() {
        let addend = match byte {
            b'b' | b'B' => mem::size_of::<HOST_BYTE>(),
            b'j' | b'J' => mem::size_of::<LUA_INT>(),   // TODO: Other pack formats
            _ => Err(GenericError::String(format!("Unknown pack format option: {}", *byte as char)).error_for_argument(0))?
        };
        if let Some(new_size) = size.checked_add(addend) {
            size = new_size
        } else {
            Err(GenericError::Str("Pack format too big!"))?
        }
    }
    Ok(Varargs::from(size))
}

/// Repeats a given string an amount and separator
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.rep>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn rep(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let string = params.coerce::<LuaString>(0)?;
    let repetitions = params.coerce::<LUA_INT>(1)?;
    let separator = params.coerce::<LuaString>(2).ok();

    if repetitions > 0 {
        let size = string.len()
            .checked_mul(repetitions as usize)  // Multiply string length by repetitions to get length of the new string before the separator
            .and_then(|s| // Add cumulative length of the separators
                separator.as_ref().map(LuaString::len).unwrap_or(0usize)
                    .checked_mul(repetitions as usize)
                    .and_then(|sep_len| sep_len.checked_add(s))
            )
            .ok_or(InvalidConcatenationError::TooLarge)?;  // If we overflowed anywhere in the above calculation, raise the ConcatenationTooLarge error
        // TODO: Check on the above size, as it currently permits strings into the exabyte range

        let buffer = if separator.is_none() || separator.as_ref().map(LuaString::len).contains(&0) {
            string.as_bytes().repeat(repetitions as usize)
        } else {
            let separator = separator.unwrap(); // We already checked is_none above.
            let mut buffer = Vec::with_capacity(size);
            for i in 0..repetitions {
                if i != 0 {
                    buffer.extend_from_slice(separator.as_bytes())
                }
                buffer.extend_from_slice(string.as_bytes())
            }
            buffer
        };

        Ok(Varargs::from(buffer))
    } else {
        Ok(Varargs::from(""))
    }
}

/// Reverses the given string
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.reverse>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn reverse(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    Ok(Varargs::from(
        params.coerce::<LuaString>(0)?
            .as_bytes()
            .iter()
            .rev()
            .copied()
            .collect::<Vec<u8>>()
    ))
}

/// Creates substring
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.sub>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn sub(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let string = params.coerce::<LuaString>(0)?;
    let from = params.coerce::<LUA_INT>(1)?;
    let to = params.coerce_opt::<LUA_INT>(2)?.unwrap_or(-1);

    Ok(Varargs::from(string.slice_bytes(from..=to)?))
}

/// Unpacks binary blob into lua values
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.unpack>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn unpack(_lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    unimplemented!()
}

/// Converts string to uppercase
///
/// <https://www.lua.org/manual/5.4/manual.html#pdf-string.upper>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn upper(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let mut string = Vec::from(params.coerce::<LuaString>(0)?.as_bytes());
    string.make_ascii_uppercase();  // Replace with locale dependant upper-case once needed
    Ok(Varargs::from(string))
}

/// Adds the string library to the global environment of the specified LuaVM. Also sets the metatable of the 'string' type ([`crate::types::value::string::LuaString`])
///
/// # Arguments
///
/// * `lua_vm`: LuaVM to add the string library to
///
/// returns: ()
pub fn insert_string_lib(lua_vm: &mut LuaVM) {
    let table = LuaTable::empty();

    set_table_func!(table, byte);
    set_table_func!(table, char);
    set_table_func!(table, dump);
    set_table_func!(table, find);
    set_table_func!(table, format);
    set_table_func!(table, gmatch);
    set_table_func!(table, gsub);
    set_table_func!(table, len);
    set_table_func!(table, lower);
    set_table_func!(table, "match", string_match);
    set_table_func!(table, pack);
    set_table_func!(table, packsize);
    set_table_func!(table, rep);
    set_table_func!(table, reverse);
    set_table_func!(table, sub);
    set_table_func!(table, unpack);
    set_table_func!(table, upper);

    let metatable = LuaTable::empty();
    metatable.raw_set("__index", table.clone());
    lua_vm.metatables.string.replace(metatable);
    lua_vm.global_env.raw_set("string", table.clone());
    lua_vm.modules.insert("string", table.clone());
}

/// Current implementation of Lua modules. To be reimplemented in the [`pattern`] rust module
pub mod pattern_old {
    use crate::error::GenericError;
    use regex::bytes::Regex;
    use nom::branch::alt;
    use nom::combinator::{map, complete, opt};
    use nom::bytes::complete::{tag, take};
    use nom::sequence::{preceded, tuple};
    use nom::character::complete::{none_of, one_of};
    use nom::IResult;
    use nom::multi::{many1, many0};
    use nom::character::complete::char;
    use crate::types::value::LuaValue;

    trait PatternComponent {
        fn compile_to_regex(&self, buf: &mut Vec<u8>) -> Result<(), GenericError>;

        fn write_escape(byte: u8, buf: &mut Vec<u8>) {
            if let b'.' | b'^' | b'$' | b'*' | b'+' | b'?' | b'(' | b')' | b'[' | b'{' | b'\\' | b'|' = byte {
                buf.push('\\' as u8);
                buf.push(byte);
            } else if byte.is_ascii_graphic() {
                buf.push(byte)
            } else {
                let first = if byte / 16 > 9 {
                    b'a' + (byte / 16) - 10
                } else {
                    b'0' + (byte / 16)
                };
                let second = if byte % 16 > 9 {
                    b'a' + (byte % 16) - 10
                } else {
                    b'0' + (byte % 16)
                };
                buf.extend_from_slice(&[b'\\', b'x', first, second])
            }
        }

        fn escape_set(byte: u8, buf: &mut Vec<u8>) {
            if let b'^' | b'-' | b']' | b'\\' = byte {
                buf.push('\\' as u8);
                buf.push(byte);
            } else if byte.is_ascii_graphic() {
                buf.push(byte)
            } else {
                let first = if byte / 16 > 9 {
                    b'a' + (byte / 16) - 10
                } else {
                    b'0' + (byte / 16)
                };
                let second = if byte % 16 > 9 {
                    b'a' + (byte % 16) - 10
                } else {
                    b'0' + (byte % 16)
                };
                buf.extend_from_slice(&[b'\\', b'x', first, second])
            }
        }
    }

    #[derive(Debug)]
    struct AnyCharacter {}

    impl PatternComponent for AnyCharacter {
        fn compile_to_regex(&self, buf: &mut Vec<u8>) -> Result<(), GenericError> {
            buf.push('.' as u8);
            Ok(())
        }
    }

    fn parse_any_character(i: &[u8]) -> IResult<&[u8], AnyCharacter> {
        map(tag("."), |_| AnyCharacter {})(i)
    }

    #[derive(Debug)]
    struct Character(u8);

    impl PatternComponent for Character {
        fn compile_to_regex(&self, buf: &mut Vec<u8>) -> Result<(), GenericError> {
            Self::write_escape(self.0, buf);
            Ok(())
        }
    }

    fn parse_character(i: &[u8]) -> IResult<&[u8], Character> {
        alt((
            map(preceded(tag("%"), none_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")), |c| Character(c as u8)),
            map(none_of("^$()%.[]"), |c| Character(c as u8)),   // The documentation includes the quantifier characters *+-? in the none-of here, but the reference implementation does not
        ))(i)
    }

    #[derive(Debug)]
    enum CharacterClass {
        Letter,
        ControlCharacter,
        Digit,
        Printable,
        Lowercase,
        Punctuation,
        Space,
        Uppercase,
        Alphanumeric,
        Hexadecimal,
        NotLetter,
        NotControlCharacter,
        NotDigit,
        NotPrintable,
        NotLowercase,
        NotPunctuation,
        NotSpace,
        NotUppercase,
        NotAlphanumeric,
        NotHexadecimal,
    }

    impl PatternComponent for CharacterClass {
        fn compile_to_regex(&self, buf: &mut Vec<u8>) -> Result<(), GenericError> {
            match self {
                CharacterClass::Letter => buf.extend_from_slice(b"\\x41-\\x5a\\x61-\\x7a"),
                CharacterClass::ControlCharacter => buf.extend_from_slice(b"\\x00-\\x1f\\x7f-\\x7f"),
                CharacterClass::Digit => buf.extend_from_slice(b"\\x30-\\x39"),
                CharacterClass::Printable => buf.extend_from_slice(b"\\x21-\\x7E"),
                CharacterClass::Lowercase => buf.extend_from_slice(b"\\x61-\\x7A"),
                CharacterClass::Punctuation => buf.extend_from_slice(b"\\x21-\\x2F\\x3A-\\x40\\x5B-\\x60\\x7B-\\x7E"),
                CharacterClass::Space => buf.extend_from_slice(b"\\x09-\\x0D\\x20-\\x20"),
                CharacterClass::Uppercase => buf.extend_from_slice(b"\\x41-\\x5A"),
                CharacterClass::Alphanumeric => buf.extend_from_slice(b"\\x30-\\x39\\x41-\\x5A\\x61-\\x7A"),
                CharacterClass::Hexadecimal => buf.extend_from_slice(b"\\x30-\\x39\\x41-\\x46\\x61-\\x66"),
                CharacterClass::NotLetter => buf.extend_from_slice(b"\\x00-\\x40\\x5B-\\x60\\x7B-\\xFF"),
                CharacterClass::NotControlCharacter => buf.extend_from_slice(b"\\x20-\\x7E\\x80-\\xFF"),
                CharacterClass::NotDigit => buf.extend_from_slice(b"\\x00-\\x2F\\x3A-\\xFF"),
                CharacterClass::NotPrintable => buf.extend_from_slice(b"\\x00-\\x20\\x7F-\\xFF"),
                CharacterClass::NotLowercase => buf.extend_from_slice(b"\\x00-\\x60\\x7B-\\xFF"),
                CharacterClass::NotPunctuation => buf.extend_from_slice(b"\\x00-\\x20\\x30-\\x39\\x41-\\x5A\\x61-\\x7A\\x7F-\\xFF"),
                CharacterClass::NotSpace => buf.extend_from_slice(b"\\x00-\\x08\\x0E-\\x1F\\x21-\\xFF"),
                CharacterClass::NotUppercase => buf.extend_from_slice(b"\\x00-\\x40\\x5B-\\xFF"),
                CharacterClass::NotAlphanumeric => buf.extend_from_slice(b"\\x00-\\x2F\\x3A-\\x40\\x5B-\\x60\\x7B-\\xFF"),
                CharacterClass::NotHexadecimal => buf.extend_from_slice(b"\\x00-\\x2F\\x3A-\\x40\\x47-\\x60\\x67-\\xFF"),
            }
            Ok(())
        }
    }

    fn parse_character_class(i: &[u8]) -> IResult<&[u8], CharacterClass> {
        alt((
            map(tag("%a"), |_| CharacterClass::Letter),
            map(tag("%c"), |_| CharacterClass::ControlCharacter),
            map(tag("%d"), |_| CharacterClass::Digit),
            map(tag("%g"), |_| CharacterClass::Printable),
            map(tag("%l"), |_| CharacterClass::Lowercase),
            map(tag("%p"), |_| CharacterClass::Punctuation),
            map(tag("%s"), |_| CharacterClass::Space),
            map(tag("%u"), |_| CharacterClass::Uppercase),
            map(tag("%w"), |_| CharacterClass::Alphanumeric),
            map(tag("%x"), |_| CharacterClass::Hexadecimal),
            map(tag("%A"), |_| CharacterClass::NotLetter),
            map(tag("%C"), |_| CharacterClass::NotControlCharacter),
            map(tag("%D"), |_| CharacterClass::NotDigit),
            map(tag("%G"), |_| CharacterClass::NotPrintable),
            map(tag("%L"), |_| CharacterClass::NotLowercase),
            map(tag("%P"), |_| CharacterClass::NotPunctuation),
            map(tag("%S"), |_| CharacterClass::NotSpace),
            map(tag("%U"), |_| CharacterClass::NotUppercase),
            map(tag("%W"), |_| CharacterClass::NotAlphanumeric),
            map(tag("%X"), |_| CharacterClass::NotHexadecimal),
        ))(i)
    }

    #[derive(Debug)]
    enum SetComponent {
        Character(Character),
        CharacterClass(CharacterClass),
        Range { start: u8, end: u8 },
    }

    impl PatternComponent for SetComponent {
        fn compile_to_regex(&self, buf: &mut Vec<u8>) -> Result<(), GenericError> {
            match self {
                SetComponent::Character(c) => c.compile_to_regex(buf)?,
                SetComponent::CharacterClass(c) => c.compile_to_regex(buf)?,
                SetComponent::Range { start, end } => {
                    Self::escape_set(*start, buf);
                    buf.push('-' as u8);
                    Self::escape_set(*end, buf);
                }
            }
            Ok(())
        }
    }

    fn parse_set_component(i: &[u8]) -> IResult<&[u8], SetComponent> {
        alt((
            map(tuple((
                take(1usize),
                tag("-"),
                take(1usize)
            )), |parts: (&[u8], &[u8], &[u8])| SetComponent::Range { start: parts.0[0], end: parts.2[0] }),
            map(parse_character_class, |c| SetComponent::CharacterClass(c)),
            map(parse_character, |c| SetComponent::Character(c))
        ))(i)
    }

    type PatternSet = Vec<SetComponent>;

    impl PatternComponent for PatternSet {
        fn compile_to_regex(&self, buf: &mut Vec<u8>) -> Result<(), GenericError> {
            for setcomponent in self {
                setcomponent.compile_to_regex(buf)?
            }
            Ok(())
        }
    }

    fn parse_pattern_set(i: &[u8]) -> IResult<&[u8], PatternSet> {
        many1(parse_set_component)(i)
    }

    #[derive(Debug)]
    enum CharacterClassOrSet {
        AnyCharacter(AnyCharacter),
        Character(Character),
        CharacterClass(CharacterClass),
        Set(PatternSet),
        ComplementSet(PatternSet),
    }

    impl PatternComponent for CharacterClassOrSet {
        fn compile_to_regex(&self, buf: &mut Vec<u8>) -> Result<(), GenericError> {
            match self {
                CharacterClassOrSet::CharacterClass(c) => {
                    buf.push('[' as u8);
                    c.compile_to_regex(buf)?;
                    buf.push(']' as u8);
                }
                CharacterClassOrSet::Set(s) => {
                    buf.push('[' as u8);
                    s.compile_to_regex(buf)?;
                    buf.push(']' as u8);
                }
                CharacterClassOrSet::ComplementSet(s) => {
                    buf.push('[' as u8);
                    buf.push('^' as u8);
                    s.compile_to_regex(buf)?;
                    buf.push(']' as u8);
                }
                CharacterClassOrSet::AnyCharacter(c) => c.compile_to_regex(buf)?,
                CharacterClassOrSet::Character(c) => c.compile_to_regex(buf)?
            }
            Ok(())
        }
    }

    fn parse_character_class_or_set(i: &[u8]) -> IResult<&[u8], CharacterClassOrSet> {
        alt((
            map(tuple((
                tag("[^"),
                parse_pattern_set,
                tag("]")
            )), |tup| CharacterClassOrSet::ComplementSet(tup.1)),
            map(tuple((
                tag("["),
                parse_pattern_set,
                tag("]")
            )), |tup| CharacterClassOrSet::Set(tup.1)),
            map(parse_character_class, CharacterClassOrSet::CharacterClass),
            map(parse_any_character, CharacterClassOrSet::AnyCharacter),
            map(parse_character, CharacterClassOrSet::Character)
        ))(i)
    }

    #[derive(Debug)]
    enum Quantifier {
        ZeroOrOne,
        ZeroOrMore,
        OneOrMore,
        ZeroOrMoreLazy,
    }

    impl PatternComponent for Quantifier {
        fn compile_to_regex(&self, buf: &mut Vec<u8>) -> Result<(), GenericError> {
            buf.extend_from_slice(match self {
                Quantifier::ZeroOrOne => b"?",
                Quantifier::ZeroOrMore => b"*",
                Quantifier::OneOrMore => b"+",
                Quantifier::ZeroOrMoreLazy => b"*?",
            });
            Ok(())
        }
    }

    fn parse_quantifier(i: &[u8]) -> IResult<&[u8], Quantifier> {
        alt((
            map(tag("?"), |_| Quantifier::ZeroOrOne),
            map(tag("*"), |_| Quantifier::ZeroOrMore),
            map(tag("+"), |_| Quantifier::OneOrMore),
            map(tag("-"), |_| Quantifier::ZeroOrMoreLazy)
        ))(i)
    }

    #[allow(unused)]
    #[derive(Debug)]
    enum PatternItem {
        SingleClass(CharacterClassOrSet),
        QuantifiedClass(CharacterClassOrSet, Quantifier),
        CaptureIndex(u8),
        Balanced { left: u8, right: u8 },
        Frontier(PatternSet),
    }

    impl PatternComponent for PatternItem {
        fn compile_to_regex(&self, buf: &mut Vec<u8>) -> Result<(), GenericError> {
            match self {
                PatternItem::SingleClass(c) => c.compile_to_regex(buf)?,
                PatternItem::QuantifiedClass(c, q) => {
                    c.compile_to_regex(buf)?;
                    q.compile_to_regex(buf)?;
                }
                PatternItem::CaptureIndex(_) => return Err(GenericError::Str("Capture-index is unsupported!")),
                PatternItem::Balanced { .. } => return Err(GenericError::Str("Balanced characters are unsupported!")),
                PatternItem::Frontier(_) => return Err(GenericError::Str("Frontier sets are unsupported!")),
            }
            Ok(())
        }
    }

    fn parse_pattern_item(i: &[u8]) -> IResult<&[u8], PatternItem> {
        alt((
            complete(map(preceded(tag("%"), one_of("123456789")), |s| PatternItem::CaptureIndex(s as u8))),
            complete(map(preceded(tag("%b"), tuple((take(1usize), take(1usize)))), |(left, right): (&[u8], &[u8])| PatternItem::Balanced { left: left[0], right: right[0] })),
            complete(map(preceded(tag("%f"), parse_pattern_set), |s| PatternItem::Frontier(s))),
            complete(map(tuple((parse_character_class_or_set, parse_quantifier)), |(c, q)| PatternItem::QuantifiedClass(c, q))),
            complete(map(parse_character_class_or_set, |c| PatternItem::SingleClass(c)))
        ))(i)
    }

    #[derive(Debug)]
    enum PatternItemOrGroup {
        PatternItem(PatternItem),
        Group(Vec<PatternItem>),
    }

    impl PatternComponent for PatternItemOrGroup {
        fn compile_to_regex(&self, buf: &mut Vec<u8>) -> Result<(), GenericError> {
            match self {
                PatternItemOrGroup::PatternItem(item) => item.compile_to_regex(buf),
                PatternItemOrGroup::Group(items) => {
                    buf.push(b'(');
                    for item in items {
                        item.compile_to_regex(buf)?;
                    }
                    buf.push(b')');
                    Ok(())
                }
            }
        }
    }

    #[derive(Debug)]
    pub struct Pattern {
        anchored_at_start: bool,
        sequence: Vec<PatternItemOrGroup>,
        anchored_at_end: bool,
    }

    impl PatternComponent for Pattern {
        fn compile_to_regex(&self, buf: &mut Vec<u8>) -> Result<(), GenericError> {
            buf.extend_from_slice(b"(?m-u)");
            if self.anchored_at_start { buf.push('^' as u8) }
            for item in &self.sequence {
                item.compile_to_regex(buf)?;
            }
            if self.anchored_at_end { buf.push('$' as u8) }
            Ok(())
        }
    }

    fn parse_pattern(i: &[u8]) -> IResult<&[u8], Pattern> {
        complete(
            map(
                tuple((
                    opt(complete(char('^'))),
                    many0(alt((
                        map(tuple((tag("("), many0(parse_pattern_item), tag(")"))), |tup| PatternItemOrGroup::Group(tup.1)),
                        map(parse_pattern_item, |pat| PatternItemOrGroup::PatternItem(pat))
                    ))),
                    opt(complete(char('$')))
                )),
                |(start, sequence, end)| Pattern { anchored_at_start: start.is_some(), sequence, anchored_at_end: end.is_some() },
            )
        )(i)
    }

    pub fn compile_pattern(bytes: &[u8]) -> Result<Regex, GenericError> {
        let pattern = parse_pattern(bytes);
        match pattern {
            Ok((remainder, pattern)) => {
                if remainder.len() == 0 {
                    let mut regex: Vec<u8> = Vec::new();
                    pattern.compile_to_regex(&mut regex)?;
                    Ok(
                        Regex::new(
                            std::str::from_utf8(&regex[..])
                                .expect("Generated regex is not UTF-8!")
                        )
                            .expect("Generated regex must be valid!")
                    )
                } else {
                    Err(GenericError::Value(LuaValue::from(format!("Invalid pattern, could not parse: {}", String::from_utf8_lossy(remainder)))))
                }
            }
            Result::Err(err) => Err(GenericError::String(format!("{}", err)))
        }
    }
}
