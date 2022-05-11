//! Module containing the Lua math functions library

use crate::vm::LuaVM;
use crate::types::value::table::LuaTable;
use crate::types::value::LuaValue;
use crate::types::varargs::Varargs;
use crate::error::{LuaError, CannotCompareError};
use crate::constants::types::{LUA_INT, LUA_FLOAT, LUA_INT_UNSIGNED};
use crate::types::parameters::LuaParameters;
use crate::types::value::number::LuaNumber;
use nom::lib::std::cmp::Ordering;
use crate::types::{LuaType};
use crate::lua_func;

/// Numerical absolute value
///
/// Rust callers may use [`LuaNumber::abs`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.abs>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn abs(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LuaNumber>(0)?;
    Ok(Varargs::from(number.abs()))
}

/// Numerical inverse cosine
///
/// Rust callers may use [`LuaNumber::as_float`] and [`LUA_FLOAT::acos`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.acos>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn acos(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LUA_FLOAT>(0)?;
    Ok(Varargs::from(number.acos()))
}

/// Numerical inverse sine
///
/// Rust callers may use [`LuaNumber::as_float`] and [`LUA_FLOAT::asin`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.asin>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn asin(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LUA_FLOAT>(0)?;
    Ok(Varargs::from(number.asin()))
}

/// Numerical inverse tangent
///
/// Rust callers may use [`LuaNumber::as_float`] and [`LUA_FLOAT::atan`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.atan>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn atan(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let y = params.coerce::<LUA_FLOAT>(0)?;
    let x = params.coerce::<LUA_FLOAT>(1).unwrap_or(1.0);
    Ok(Varargs::from(y.atan2(x))) // atan2 matches Lua behavior
}

/// Round number to nearest greater-or-equal integer
///
/// Rust callers may use [`LuaNumber::ceil`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.ceil>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn ceil(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LuaNumber>(0)?;
    Ok(Varargs::from(number.ceil()))
}

/// Numerical cosine
///
/// Rust callers may use [`LuaNumber::as_float`] and [`LUA_FLOAT::cos`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.atan>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn cos(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LUA_FLOAT>(0)?;
    Ok(Varargs::from(number.cos()))
}

/// Converts radians to degrees; Multiplying by (180/pi)
///
/// Rust callers may use [`LuaNumber::as_float`] and [`LUA_FLOAT::to_degrees`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.atan>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn deg(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LUA_FLOAT>(0)?;
    Ok(Varargs::from(number.to_degrees()))
}

/// Returns Euler's number exponentiated to the passed value
///
/// Rust callers may use [`LuaNumber::as_float`] and [`LUA_FLOAT::exp`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.exp>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn exp(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LUA_FLOAT>(0)?;
    Ok(Varargs::from(number.exp()))
}

/// Rounds number to nearest less-than-or-equal integer
///
/// Rust callers may use [`LuaNumber::ceil`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.floor>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn floor(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LuaNumber>(0)?;
    Ok(Varargs::from(number.floor()))
}

/// Numerical modulo, rounds towards zero
///
/// NOTE: This is not equivalent to [LuaNumber::rem](std::ops::Rem::rem) or the Lua '%' operator
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.fmod>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn fmod(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let x = params.coerce::<LuaNumber>(0)?;
    let y = params.coerce::<LuaNumber>(1)?;
    match x {
        LuaNumber::INT(lhs) => {
            match y {
                LuaNumber::INT(rhs) => {
                    Ok(Varargs::from(lhs % rhs))
                }
                LuaNumber::FLOAT(rhs) => {
                    let lhs = lhs as LUA_FLOAT;
                    Ok(Varargs::from(lhs % rhs))
                }
            }
        }
        LuaNumber::FLOAT(lhs) => {
            let rhs = match y {
                LuaNumber::INT(rhs) => rhs as f64,
                LuaNumber::FLOAT(rhs) => rhs,
            };
            Ok(Varargs::from(lhs % rhs))
        }
    }
}

/// Positive infinity
pub const HUGE: LUA_FLOAT = LUA_FLOAT::INFINITY;

/// Numerical logarithm, optional base (default natural log)
///
/// Rust callers may use [`LuaNumber::as_float`] and [`LUA_FLOAT::log`] (or [`LUA_FLOAT::ln`]) directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.fmod>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn log(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let x = params.coerce::<LUA_FLOAT>(0)?;
    let base = params.coerce::<LUA_FLOAT>(1).ok();
    Ok(Varargs::from(match base {
        None => x.ln(),
        Some(base) => x.log(base),
    }))
}

/// Returns 'max' argument of two or more parameters; Whichever argument is greater than the others.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.max>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn max(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let first = params.coerce::<LuaValue>(0)?;
    let mut maximum = &first;
    for value in &params[1..] {
        match value.partial_cmp(maximum) {
            None => Err(CannotCompareError { lhs_type: value.type_name(), rhs_type: maximum.type_name() })?,
            Some(Ordering::Less) | Some(Ordering::Equal) => {}
            Some(Ordering::Greater) => {
                maximum = value;
            }
        }
    }
    Ok(Varargs::from(maximum.clone()))
}

/// Maximum integer value supported; Identical to LUA_INT::MAX;
pub const MAX_INTEGER: LUA_INT = LUA_INT::MAX;

/// Returns 'min' argument of two or more parameters; Whichever argument is lesser than the others.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.max>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn min(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let first = params.coerce::<LuaValue>(0)?;
    let mut minimum = &first;
    for value in &params[1..] {
        match value.partial_cmp(minimum) {
            None => Err(CannotCompareError { lhs_type: value.type_name(), rhs_type: minimum.type_name() })?,
            Some(Ordering::Greater) | Some(Ordering::Equal) => {}
            Some(Ordering::Less) => {
                minimum = value;
            }
        }
    }
    Ok(Varargs::from(minimum.clone()))
}

/// Minimum integer value supported; Identical to LUA_INT::MIN;
pub const MIN_INTEGER: LUA_INT = LUA_INT::MIN;

/// Splits numerical value into integral and fractional parts
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.modf>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn modf(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LuaNumber>(0)?;
    match number {
        LuaNumber::INT(_) => Ok(Varargs::from((number, LuaNumber::from(0.0)))),
        LuaNumber::FLOAT(f) => {
            let truncated = f.trunc();
            if (truncated as LUA_INT) as LUA_FLOAT == truncated {
                Ok(Varargs::from((truncated as LUA_INT, f.fract())))
            } else {
                Ok(Varargs::from((truncated, f.fract())))
            }
        }
    }
}

/// Pi constant
pub const PI: LUA_FLOAT = std::f64::consts::PI as LUA_FLOAT;

/// Converts radians to degrees; Multiplying by (pi/180)
///
/// Rust callers may use [`LuaNumber::as_float`] and [`LUA_FLOAT::to_radians`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.rad>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn rad(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LUA_FLOAT>(0)?;
    Ok(Varargs::from(number.to_radians()))
}

/// Generates random number
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.random>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn random(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let m = params.coerce_opt::<LUA_FLOAT>(0)?;  // Maybe replace this messy construct with a Ok/Err/None triplet return from try_coerce
    let n = params.coerce_opt::<LUA_FLOAT>(1)?;
    let (lower, upper) = match (m, n) {
        (None, None) => (0 as LUA_FLOAT, 1 as LUA_FLOAT),
        (Some(upper), None) => (1 as LUA_FLOAT, upper),
        (Some(lower), Some(upper)) => (lower, upper),
        (None, Some(_)) => unreachable!("None parameter cannot be followed by existent parameter"),
    };

    let diff = upper - lower;

    if diff >= (0 as LUA_FLOAT) {
        let value = rand::random::<LUA_FLOAT>();
        Ok(Ok(Varargs::from(lower + (value * diff))))
    } else {
        Err(LuaError::new("range is empty!"))
    }?
}

/// Sets random number generator seed.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.randomseed>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
#[allow(unused)]
pub fn randomseed(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let m = params.coerce::<LUA_INT>(0);
    let n = params.coerce::<LUA_INT>(1);
    todo!()
}

/// Numerical sine
///
/// Rust callers may use [`LuaNumber::as_float`] and [`LUA_FLOAT::sin`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.atan>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn sin(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LUA_FLOAT>(0)?;
    Ok(Varargs::from(number.sin()))
}

/// Numerical square root
///
/// Rust callers may use [`LuaNumber::as_float`] and [`LUA_FLOAT::sqrt`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.atan>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn sqrt(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LUA_FLOAT>(0)?;
    Ok(Varargs::from(number.sqrt()))
}

/// Numerical tangent
///
/// Rust callers may use [`LuaNumber::as_float`] and [`LUA_FLOAT::tan`] directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.atan>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn tan(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LUA_FLOAT>(0)?;
    Ok(Varargs::from(number.tan()))
}

/// Returns integer value of passed argument, or 'fail' if value cannot be coerced to an integer
///
/// Rust callers may use [`LuaNumber::try_int`] or [LUA_INT::coerce_from](crate::types::CoerceFrom::coerce_from) directly instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.tointeger>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn tointeger(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    match params.coerce::<LUA_INT>(0) {
        Ok(int) => Ok(Varargs::from(int)),
        _ => Ok(Varargs::fail())
    }
}

/// Returns numerical type of value, `"integer"` for integers, `"float"` for floating point values, 'fail' if value is not a number.
///
/// Rust callers may instead match on the enum variants [`LuaNumber::INT`] and/or [`LuaNumber::FLOAT`] instead
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.tointeger>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn numtype(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let number = params.coerce::<LuaNumber>(0);
    match number {
        Ok(LuaNumber::INT(_)) => Ok(Varargs::from("int")),
        Ok(LuaNumber::FLOAT(_)) => Ok(Varargs::from("float")),
        _ => Ok(Varargs::fail())
    }
}

/// Unsigned Less-Than test. Requires both parameters to be integers
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-math.ult>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn ult(_lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let m = params.coerce::<LUA_INT>(0)?;
    let n = params.coerce::<LUA_INT>(0)?;
    Ok(Varargs::from((m as LUA_INT_UNSIGNED) < (n as LUA_INT_UNSIGNED)))
}

/// Adds the math library to the global environment of the specified LuaVM.
///
/// # Arguments
///
/// * `lua_vm`: LuaVM to add the math library to
///
/// returns: ()
pub fn insert_math_lib(lua_vm: &mut LuaVM) {
    let table = LuaTable::empty();

    set_table_func!(table, abs);
    set_table_func!(table, acos);
    set_table_func!(table, asin);
    set_table_func!(table, atan);
    set_table_func!(table, ceil);
    set_table_func!(table, cos);
    set_table_func!(table, deg);
    set_table_func!(table, exp);
    set_table_func!(table, floor);
    set_table_func!(table, fmod);
    table.raw_set("huge", HUGE);
    set_table_func!(table, log);
    set_table_func!(table, max);
    table.raw_set("maxinteger", MAX_INTEGER);
    set_table_func!(table, min);
    table.raw_set("mininteger", MIN_INTEGER);
    set_table_func!(table, modf);
    table.raw_set("pi", PI);
    set_table_func!(table, rad);
    set_table_func!(table, random);
    set_table_func!(table, randomseed);
    set_table_func!(table, sin);
    set_table_func!(table, sqrt);
    set_table_func!(table, tan);
    set_table_func!(table, tointeger);
    set_table_func!(table, "type", numtype);
    set_table_func!(table, ult);

    lua_vm.global_env.raw_set("math", table.clone());
    lua_vm.modules.insert("math", table);
}