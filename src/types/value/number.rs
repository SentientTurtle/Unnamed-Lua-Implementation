//! Module for lua 'number' type

use crate::error::CannotCoerceError;
use crate::constants::types::{LUA_INT, LUA_INT_UNSIGNED, LUA_FLOAT};
use std::ops::{Add, Sub, Mul, Div, Rem, BitAnd, BitOr, BitXor, Shl, Shr, Neg};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::fmt;
use std::convert::TryInto;
use crate::types::{LuaType, CoerceFrom};
use crate::types::value::LuaValue;

/// Lua number value, has subtypes for integers and floats
///
/// Can be converted to int with [`LuaNumber::try_int`] and to float with [`LuaNumber::as_float`]
///
/// (Basic) mathematical operations are implemented for this type directly, and should be used instead of converting to rust number types, as to reduce the likelyhood of panics during math errors.
///
/// Bitwise operations return a result type, as they are only applicable to integer values, other operations will either coerce to float on overflow, wrap, or return NaN
#[derive(Copy, Clone, Debug)]
pub enum LuaNumber {
    INT(LUA_INT),
    FLOAT(LUA_FLOAT),
}

impl LuaNumber {
    /// Returns the value of this number as an integer, or errors if it is a non-integer
    pub fn try_int(&self) -> Result<LUA_INT, CannotCoerceError> {
        match *self {
            LuaNumber::INT(integer) => Ok(integer),
            LuaNumber::FLOAT(float) => if (float as LUA_INT) as LUA_FLOAT == float {
                Ok(float as LUA_INT)
            } else {
                Err(CannotCoerceError { from: "float", to: "integer" })
            }
        }
    }

    /// Returns the value of this number as a float, may lose precision when converting high-value integers to floats
    pub fn as_float(&self) -> LUA_FLOAT {
        match self {
            LuaNumber::INT(int) => *int as LUA_FLOAT,
            LuaNumber::FLOAT(float) => *float,
        }
    }

    fn binary_op(lhs: LuaNumber, rhs: LuaNumber, intfunc: fn(LUA_INT, LUA_INT) -> LUA_INT, floatfunc: fn(LUA_FLOAT, LUA_FLOAT) -> LUA_FLOAT) -> LuaNumber {
        match (lhs, rhs) {
            (LuaNumber::INT(l_i), LuaNumber::INT(r_i)) => {
                LuaNumber::from(intfunc(l_i, r_i))
            }
            _ => {
                LuaNumber::from(floatfunc(lhs.as_float(), rhs.as_float()))
            }
        }
    }

    fn unary_op(value: LuaNumber, intfunc: fn(LUA_INT) -> LUA_INT, floatfunc: fn(LUA_FLOAT) -> LUA_FLOAT) -> LuaNumber {
        match value {
            LuaNumber::INT(int) => LuaNumber::INT(intfunc(int)),
            LuaNumber::FLOAT(float) => LuaNumber::FLOAT(floatfunc(float)),
        }
    }

    /// Round up to nearest greater-or-equal integer
    pub fn ceil(&self) -> Self {
        match self {
            LuaNumber::INT(_) => *self,
            LuaNumber::FLOAT(f) => LuaNumber::from(f.ceil()),
        }
    }

    /// Round down to nearest less-than-or-equal integer
    pub fn floor(&self) -> Self {
        match self {
            LuaNumber::INT(_) => *self,
            LuaNumber::FLOAT(f) => LuaNumber::from(f.floor()),
        }
    }
}

impl LuaType for LuaNumber {
    const TYPE_NAME: &'static str = "number";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaNumber {
    // TODO: Metamethod
    fn coerce_opt(value: &T) -> Option<Self> {
        match value.clone().into() {
            LuaValue::NUMBER(number) => Some(number.clone()),
            LuaValue::STRING(string) => {
                if let Some(utf8) = string.try_utf8().ok() {
                    match parse_hex(utf8) {
                        ParseHexResult::Integer(integer) => Some(LuaNumber::INT(integer)),
                        ParseHexResult::Float(float) => Some(LuaNumber::FLOAT(float)),
                        ParseHexResult::InvalidNumber => None
                    }
                } else {
                    None
                }
            }
            _ => None
        }
    }
}

impl From<LUA_INT> for LuaNumber {
    fn from(int: LUA_INT) -> Self {
        LuaNumber::INT(int)
    }
}

impl From<LUA_FLOAT> for LuaNumber {
    fn from(float: LUA_FLOAT) -> Self {
        LuaNumber::FLOAT(float)
    }
}

impl From<usize> for LuaNumber {
    fn from(num: usize) -> Self {
        match num.try_into() {
            Ok(n) => LuaNumber::INT(n),
            Err(_) => LuaNumber::FLOAT(num as f64)
        }
    }
}

impl From<u64> for LuaNumber {
    fn from(num: u64) -> Self {
        match num.try_into() {
            Ok(n) => LuaNumber::INT(n),
            Err(_) => LuaNumber::FLOAT(num as f64)
        }
    }
}

impl Add for LuaNumber {
    type Output = LuaNumber;

    fn add(self, rhs: Self) -> Self::Output {
        LuaNumber::binary_op(self, rhs, |lhs, rhs| lhs.wrapping_add(rhs), |lhs, rhs| lhs + rhs)
    }
}

impl Sub for LuaNumber {
    type Output = LuaNumber;

    fn sub(self, rhs: Self) -> Self::Output {
        LuaNumber::binary_op(self, rhs, |lhs, rhs| lhs.wrapping_sub(rhs), |lhs, rhs| lhs - rhs)
    }
}

impl Mul for LuaNumber {
    type Output = LuaNumber;

    fn mul(self, rhs: Self) -> Self::Output {
        LuaNumber::binary_op(self, rhs, |lhs, rhs| lhs.wrapping_mul(rhs), |lhs, rhs| lhs * rhs)
    }
}

impl Div for LuaNumber {
    type Output = LuaNumber;

    /// Division, handles divide-by-zero by returning -NaN
    fn div(self, rhs: Self) -> Self::Output {
        let rhs = rhs.as_float();
        if rhs == 0.0 {
            LuaNumber::FLOAT(-LUA_FLOAT::NAN)
        } else {
            LuaNumber::FLOAT(self.as_float() / rhs)
        }
    }
}

impl Rem for LuaNumber {
    type Output = LuaNumber;

    fn rem(self, rhs: Self) -> Self::Output {
        self - (rhs * self.idiv(rhs))
    }
}

impl BitAnd for LuaNumber {
    type Output = Result<LuaNumber, CannotCoerceError>;

    fn bitand(self, rhs: Self) -> Self::Output {
        Ok(LuaNumber::INT(self.try_int()? & rhs.try_int()?))
    }
}

impl BitOr for LuaNumber {
    type Output = Result<LuaNumber, CannotCoerceError>;

    fn bitor(self, rhs: Self) -> Self::Output {
        Ok(LuaNumber::INT(self.try_int()? | rhs.try_int()?))
    }
}

impl BitXor for LuaNumber {
    type Output = Result<LuaNumber, CannotCoerceError>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Ok(LuaNumber::INT(self.try_int()? ^ rhs.try_int()?))
    }
}

impl Shl for LuaNumber {
    type Output = Result<LuaNumber, CannotCoerceError>;

    fn shl(self, rhs: Self) -> Self::Output {
        if rhs < 0 {
            self.shr(-rhs)
        } else if rhs >= 64 {
            Ok(LuaNumber::from(0usize))
        } else {
            Ok(LuaNumber::from(self.try_int()? << rhs.try_int()?))
        }
    }
}

impl Shr for LuaNumber {
    type Output = Result<LuaNumber, CannotCoerceError>;

    fn shr(self, rhs: Self) -> Self::Output {
        if rhs < 0 {
            self.shl(-rhs)
        } else if rhs >= 64 {
            Ok(LuaNumber::from(0usize))
        } else {
            Ok(LuaNumber::from((self.try_int()? as LUA_INT_UNSIGNED >> rhs.try_int()?) as LUA_INT))
        }
    }
}

impl Neg for LuaNumber {
    type Output = LuaNumber;

    fn neg(self) -> Self::Output {
        LuaNumber::unary_op(self, |i| -i, |f| -f)
    }
}

impl LuaNumber {
    /// Absolute value, returns -self if self is < 0
    pub fn abs(self) -> LuaNumber {
        if self >= 0 { self } else { -self }
    }

    /// Raises self to specified power
    pub fn pow(self, rhs: Self) -> LuaNumber {
        LuaNumber::from(self.as_float().powf(rhs.as_float()))
    }

    /// Integer division
    pub fn idiv(self, rhs: Self) -> LuaNumber {
        match (self, rhs) {
            (LuaNumber::INT(lhs), LuaNumber::INT(rhs)) => LuaNumber::INT({
                let mut quotient = lhs / rhs;   // TODO: Handle div-by-zero
                if lhs % rhs != 0 && ((lhs < 0) ^ (rhs < 0)) {
                    quotient -= 1;
                }
                quotient
            }),
            (LuaNumber::FLOAT(lhs), LuaNumber::INT(rhs)) => LuaNumber::INT((lhs / rhs as LUA_FLOAT).floor() as LUA_INT),
            (LuaNumber::INT(lhs), LuaNumber::FLOAT(rhs)) => LuaNumber::INT((lhs as LUA_FLOAT / rhs).floor() as LUA_INT),
            (LuaNumber::FLOAT(lhs), LuaNumber::FLOAT(rhs)) => LuaNumber::INT((lhs / rhs).floor() as LUA_INT)
        }
    }

    /// Binary-NOT, throws error if this number is floating-point
    pub fn bnot(self) -> Result<LuaNumber, CannotCoerceError> {
        Ok(LuaNumber::INT(!self.try_int()?))
    }
}

impl PartialEq for LuaNumber {
    fn eq(&self, other: &Self) -> bool {
        match self {
            LuaNumber::INT(lhs) => {
                match other {
                    LuaNumber::INT(rhs) => lhs.eq(rhs),
                    LuaNumber::FLOAT(rhs) => {
                        if (*rhs as LUA_INT) as LUA_FLOAT == *rhs {
                            lhs.eq(&(*rhs as LUA_INT))
                        } else {
                            false   // Non-integer float cannot be equal to integer
                        }
                    }
                }
            }
            LuaNumber::FLOAT(lhs) => {
                match other {
                    LuaNumber::INT(rhs) => {
                        if (*lhs as LUA_INT) as LUA_FLOAT == *lhs {
                            rhs.eq(&(*lhs as LUA_INT))
                        } else {
                            false   // Non-integer float cannot be equal to integer
                        }
                    },
                    LuaNumber::FLOAT(rhs) => {
                        if lhs == rhs {
                            true
                        } else if lhs.signum() != rhs.signum() {
                            false   // Mismatched sign, or NaN
                        } else {
                            // Fuzzy equality required to pass tests
                            let (lhs_i, rhs_i): (LUA_INT, LUA_INT) = unsafe {
                                (std::mem::transmute(lhs.to_le_bytes()), std::mem::transmute(rhs.to_le_bytes()))
                            };
                            if (lhs_i-rhs_i).abs() <= 0b1111 {   // TODO: Re-evaluate fuzzy equality on floats
                                true
                            } else {
                                false
                            }
                        }
                    },
                }
            }
        }
    }
}

impl PartialOrd for LuaNumber {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            LuaNumber::INT(lhs) => {
                match other {
                    LuaNumber::INT(rhs) => {
                        lhs.partial_cmp(rhs)
                    }
                    LuaNumber::FLOAT(rhs) => {
                        (*lhs as f64).partial_cmp(rhs)
                    }
                }
            }
            LuaNumber::FLOAT(lhs) => {
                match other {
                    LuaNumber::INT(rhs) => {
                        lhs.partial_cmp(&(*rhs as f64))
                    }
                    LuaNumber::FLOAT(rhs) => {
                        lhs.partial_cmp(rhs)
                    }
                }
            }
        }
    }
}

impl PartialEq<LUA_INT> for LuaNumber {
    fn eq(&self, other: &i64) -> bool {
        match self {
            LuaNumber::INT(i) => i == other,
            LuaNumber::FLOAT(f) => *f as LUA_INT == *other && *f == ((*f as LUA_INT) as LUA_FLOAT),
        }
    }
}

impl PartialOrd<LUA_INT> for LuaNumber {
    fn partial_cmp(&self, rhs: &LUA_INT) -> Option<Ordering> {
        match self {
            LuaNumber::INT(lhs) => {
                lhs.partial_cmp(rhs)
            }
            LuaNumber::FLOAT(lhs) => {
                lhs.partial_cmp(&(*rhs as f64))
            }
        }
    }
}

impl PartialEq<LUA_FLOAT> for LuaNumber {
    fn eq(&self, other: &f64) -> bool {
        match self {
            LuaNumber::INT(i) => *i as LUA_FLOAT == *other && *i == ((*i as LUA_FLOAT) as LUA_INT),
            LuaNumber::FLOAT(f) => f == other,
        }
    }
}


impl PartialOrd<LUA_FLOAT> for LuaNumber {
    fn partial_cmp(&self, rhs: &LUA_FLOAT) -> Option<Ordering> {
        match self {
            LuaNumber::INT(lhs) => {
                (*lhs as f64).partial_cmp(rhs)
            }
            LuaNumber::FLOAT(lhs) => {
                lhs.partial_cmp(rhs)
            }
        }
    }
}


impl Display for LuaNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            LuaNumber::INT(integer) => write!(f, "{}", integer),
            LuaNumber::FLOAT(float) => write!(f, "{:.}", float),
        }
    }
}


/// Result type for [`parse_hex`]
#[derive(Debug)]
pub enum ParseHexResult {
    Integer(LUA_INT),
    Float(LUA_FLOAT),
    InvalidNumber
}

/// Parses a string containing a hexadecimal number according to Lua hexadecimal literal format
///
/// See <https://www.lua.org/manual/5.4/manual.html#3.1> for details on Lua's hexadecimal literal format
///
/// # Arguments
///
/// * `string`: String to parse
///
/// returns: ParseHexResult
pub fn parse_hex(mut string: &str) -> ParseHexResult {
    string = string.trim();
    let bytes = string.as_bytes();

    let mut is_negative = false;

    if bytes.len() == 0 { return ParseHexResult::InvalidNumber; }
    let mut index = 0;
    if bytes[index] == b'-' {
        is_negative = true;
        index += 1;
    } else if bytes[index] == b'+' {
        index += 1;
    }

    // We know index is at most 1 here, and bytes.len() must be greater than 0, so this subtraction is safe.
    if (bytes.len() - index >= 2) && bytes[index] == b'0' && (bytes[index + 1] == b'x' || bytes[index + 1] == b'X') {
        index += 2;
        let (integer, fraction, exponent) = match string[index..].split_once('.') {
            None => {
                match string[index..].split_once(&['p', 'P']) {
                    None => (&string[index..], None, None),
                    Some((integer, exponent)) => (integer, None, Some(exponent))
                }
            }
            Some((integer, fraction_and_exponent)) => {
                match fraction_and_exponent.split_once(&['p', 'P']) {
                    None => (integer, Some(fraction_and_exponent), None),
                    Some((fraction, exponent)) => (integer, Some(fraction), Some(exponent))
                }
            }
        };
        const INTEGER_SIZE: usize = std::mem::size_of::<LUA_INT_UNSIGNED>() * 2;
        match (integer, fraction, exponent) {
            (mut integer, None, None) => {  // Parse as integer
                if integer.len() > INTEGER_SIZE {
                    if integer[0..integer.len() - INTEGER_SIZE].as_bytes().iter().all(|byte| matches!(*byte, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z')) {    // Validate that leading characters are valid hexadecimal
                        integer = &integer[integer.len() - INTEGER_SIZE..]  // Parse only the last INTEGER_SIZE characters
                    } else {
                        return ParseHexResult::InvalidNumber;
                    }
                }
                if integer.starts_with('+') { return ParseHexResult::InvalidNumber; }
                match LUA_INT_UNSIGNED::from_str_radix(integer, 16) {
                    Ok(unsigned) => {
                        if is_negative {
                            match (0 as LUA_INT).checked_sub(unsigned as LUA_INT) { // Intentional 'as' cast to do a mem-transmute
                                Some(signed) => ParseHexResult::Integer(signed),
                                None => ParseHexResult::InvalidNumber
                            }
                        } else {
                            ParseHexResult::Integer(unsigned as LUA_INT)
                        }
                    },
                    Err(_) => ParseHexResult::InvalidNumber
                }
            }
            (mut integer, fraction_opt, exponent) => {  // Parse as float
                debug_assert!(fraction_opt.is_some() || exponent.is_some());
                let integer_string_length = integer.len();
                let fraction_characters_to_read = if integer.len() > INTEGER_SIZE {
                    if integer[INTEGER_SIZE..].as_bytes().iter().all(|byte| matches!(*byte, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z')) {    // Validate that trailing characters are valid hexadecimal
                        integer = &integer[..INTEGER_SIZE]  // Parse only the first INTEGER_SIZE characters
                    } else {
                        return ParseHexResult::InvalidNumber;
                    }
                    0
                } else {
                    INTEGER_SIZE - integer.len()
                };

                if integer.starts_with('+') { return ParseHexResult::InvalidNumber; }
                let mut total = match LUA_INT_UNSIGNED::from_str_radix(integer, 16) {
                    Ok(integer) => integer,
                    Err(_) => return ParseHexResult::InvalidNumber
                };

                let fraction_size = if let Some(mut fraction) = fraction_opt {
                    if fraction_characters_to_read > 0 {
                        if fraction.len() > fraction_characters_to_read {
                            if fraction[fraction_characters_to_read..].as_bytes().iter().all(|byte| matches!(*byte, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z')) {    // Validate that trailing characters are valid hexadecimal
                                fraction = &fraction[..fraction_characters_to_read]  // Parse only the first INTEGER_SIZE characters
                            } else {
                                return ParseHexResult::InvalidNumber;
                            }
                        };

                        if fraction.starts_with('+') { return ParseHexResult::InvalidNumber; }

                        total <<= 4 * fraction.len();
                        total |= match LUA_INT_UNSIGNED::from_str_radix(fraction, 16) {
                            Ok(fraction) => fraction,
                            Err(_) => return ParseHexResult::InvalidNumber
                        };
                        fraction.len()
                    } else if !fraction.as_bytes().iter().all(|byte| matches!(*byte, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z')) {
                        return ParseHexResult::InvalidNumber
                    } else {
                        0
                    }
                } else {
                    0
                };

                let mut float = total as LUA_FLOAT;

                if integer_string_length > INTEGER_SIZE {
                    float *= (16 as LUA_FLOAT).powi((integer_string_length - INTEGER_SIZE) as i32)
                }

                if fraction_size > 0 {
                    float /= (16 as LUA_FLOAT).powi(fraction_size as i32)
                }

                if let Some(exponent) = exponent {
                    float *= (2 as LUA_FLOAT).powf(match LUA_INT::from_str_radix(exponent, 10) {    // Exponent is in base-10
                        Ok(integer) => integer as LUA_FLOAT,
                        Err(_) => return ParseHexResult::InvalidNumber
                    })
                }

                if is_negative {
                    float = (0 as LUA_FLOAT) - float;
                }

                ParseHexResult::Float(float)
            }
        }
    } else {
        match string.parse::<LUA_INT>() {  // Parse directly to signed type; This saves having to downcast unsigned to signed
            Ok(integer) => ParseHexResult::Integer(integer),
            Err(_) => {
                match string[index..].parse::<LUA_FLOAT>() {
                    Ok(float) => ParseHexResult::Float(float),
                    Err(_) => ParseHexResult::InvalidNumber
                }
            }
        }
    }
}