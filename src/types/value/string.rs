//! Module for Lua 'string' type

use std::rc::Rc;
use crate::error::{InvalidValueError, StringIndexOutOfRangeError};
use std::fmt::{Debug, Display, Formatter};
use std::fmt;
use std::cmp::Ordering;
use crate::types::{LuaType, CoerceFrom};
use crate::types::value::LuaValue;
use crate::types::value::number::LuaNumber;
use std::borrow::Cow;
use std::collections::Bound;
use std::convert::TryFrom;
use std::ops::RangeBounds;
use crate::constants::types::LUA_INT;

/// Lua string type, maintains different variants for utf-8 strings and binary strings
///
/// This type must be instantiated via the From implementations or constructor methods, as uses require variants to be correct
///
/// While matching on this enum is safe, use of [`LuaString::as_bytes`] and [`LuaString::as_str`] is preferred
#[derive(Clone)]
pub enum LuaString {    // Potential small performance increase: add subtypes for &'static str and &'static [u8]
    UNICODE(Rc<str>),
    BINARY(Rc<[u8]>),
}

impl LuaType for LuaString {
    const TYPE_NAME: &'static str = "string";
}

impl Debug for LuaString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LuaString::UNICODE(string) => f.debug_tuple("LuaString").field(&string.len()).field(string).finish(),
            LuaString::BINARY(string) => f.debug_tuple("LuaString").field(&string.len()).field(string).finish()
        }
    }
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaString {
    fn coerce_opt(value: &T) -> Option<Self> {
        match value.clone().into() {
            LuaValue::STRING(string) => Some(string),
            LuaValue::NUMBER(number) => {
                let string = match number {
                    LuaNumber::INT(int) => int.to_string(),
                    LuaNumber::FLOAT(float) => float.to_string(),
                };
                Some(LuaString::from(string))
            }
            _ => None
        }
    }
}

impl From<&str> for LuaString {
    fn from(string: &str) -> Self {
        LuaString::UNICODE(Rc::from(string))
    }
}

impl From<String> for LuaString {
    fn from(string: String) -> Self {
        LuaString::UNICODE(Rc::from(string))
    }
}

impl From<&[u8]> for LuaString {
    /// This function is safe for both utf-8 and non-utf8 binary strings; Performs utf-8 validity check
    fn from(string: &[u8]) -> Self {
        match std::str::from_utf8(string) {
            Ok(s) => LuaString::UNICODE(Rc::from(s)),
            Err(_) => LuaString::BINARY(Rc::from(string)),
        }
    }
}

impl From<Box<[u8]>> for LuaString {
    /// This function is safe for both utf-8 and non-utf8 binary strings; Performs utf-8 validity check
    fn from(boxed: Box<[u8]>) -> Self {
        match std::str::from_utf8(&boxed) {
            Ok(_string) => unsafe {
                LuaString::UNICODE(Rc::from(std::str::from_boxed_utf8_unchecked(boxed)))
            },
            Err(_) => {
                LuaString::BINARY(Rc::from(boxed))
            }
        }
    }
}

impl From<Vec<u8>> for LuaString {
    /// This function is safe for both utf-8 and non-utf8 binary strings; Performs utf-8 validity check
    fn from(vec: Vec<u8>) -> Self {
        match std::str::from_utf8(&*vec) {
            Ok(_string) => unsafe {
                LuaString::UNICODE(Rc::from(std::str::from_boxed_utf8_unchecked(vec.into_boxed_slice())))
            },
            Err(_) => {
                LuaString::BINARY(Rc::from(vec))
            }
        }
    }
}

impl LuaString {
    /// Concatenates multiple values into a LuaString
    ///
    /// # Arguments
    ///
    /// * `values`: Values to concatenate
    ///
    /// returns: LuaString
    pub fn concat<'a, T: IntoIterator<Item=&'a [u8]>>(values: T) -> LuaString {
        let buffer: Vec<u8> = values.into_iter()
            .flat_map(<[u8]>::iter)
            .copied()
            .collect();
        LuaString::from(buffer)
    }

    /// Converts Lua 'relative' index into absolute index
    pub fn relative_index(&self, index: LUA_INT) -> Result<usize, StringIndexOutOfRangeError> {
        let absolute_index: Option<usize> = try {
            if index < 0 {
                self.len().checked_sub(usize::try_from(index.checked_neg()?).ok()?)?
            } else {
                usize::try_from(index).ok()?.checked_sub(1)?
            }
        };
        absolute_index
            .filter(|index| *index < self.len())
            .ok_or(StringIndexOutOfRangeError { index })
    }

    /// Slices this string based on Lua 'relative' indices
    pub fn slice_bytes<T: RangeBounds<LUA_INT>>(&self, range: T) -> Result<&[u8], StringIndexOutOfRangeError> {
        match (range.start_bound(), range.end_bound()) {
            (Bound::Unbounded, Bound::Unbounded) => Ok(self.as_bytes()),
            (start_bound, Bound::Unbounded) => {
                let start =  match start_bound {
                    Bound::Included(start) => self.relative_index(*start)?,
                    Bound::Excluded(start) => self.relative_index(*start)?.saturating_add(1),
                    Bound::Unbounded => unreachable!(),
                };
                if self.len() == 0 || start >= self.len() {
                    Ok(&[])
                } else {
                    Ok(&self.as_bytes()[start..])
                }
            }
            (start_bound, Bound::Included(end)) => {
                let mut end = self.relative_index(*end)?;
                let start =  match start_bound {
                    Bound::Included(start) => self.relative_index(*start)?,
                    Bound::Excluded(start) => self.relative_index(*start)?.saturating_add(1),
                    Bound::Unbounded => 0,
                };
                if self.len() == 0 || start > end || start >= self.len() {
                    Ok(&[])
                } else {
                    end = end.min(self.len() - 1);
                    Ok(&self.as_bytes()[start..=end])
                }
            }
            (start_bound, Bound::Excluded(end)) => {
                let mut end = self.relative_index(*end)?;
                let start =  match start_bound {
                    Bound::Included(start) => self.relative_index(*start)?,
                    Bound::Excluded(start) => self.relative_index(*start)?.saturating_add(1),
                    Bound::Unbounded => 0,
                };
                if start > end || start >= self.len() {
                    Ok(&[])
                } else {
                    end = end.min(self.len());
                    Ok(&self.as_bytes()[start..end])
                }
            }
        }
    }

    /// Returns this string as byte-slice; Valid for both utf-8 and binary strings
    pub fn as_bytes(&self) -> &[u8] {
        match self {
            LuaString::UNICODE(string) => string.as_bytes(),
            LuaString::BINARY(bytes) => bytes,
        }
    }

    /// Returns this string as `&str` if it is a utf-8 string, else returns none
    ///
    /// See [`LuaString::try_utf8`] for variant of this function that yields a try-able error
    pub fn as_str(&self) -> Option<&str> {
        match self {
            LuaString::UNICODE(string) => Some(string),
            LuaString::BINARY(_) => None,
        }
    }

    /// Performs lossy conversion to utf-8, see [`String::from_utf8_lossy`]
    pub fn to_utf8_lossy(&self) -> Cow<str> {
        match self {
            LuaString::UNICODE(string) => Cow::from(&**string),
            LuaString::BINARY(bytes) => String::from_utf8_lossy(bytes),
        }
    }



    /// Returns this string as `&str` if it is a utf-8 string, else throws error
    ///
    /// See [`LuaString::as_str`] for variant of this function that returns Option
    pub fn try_utf8(&self) -> Result<&str, InvalidValueError> {
        match self {
            LuaString::UNICODE(unicode) => Ok(unicode),
            LuaString::BINARY(bytes) => {
                debug_assert!(std::str::from_utf8(bytes).is_err());
                Err(InvalidValueError { expected: "unicode string", found: "binary string" })
            }
        }
    }

    /// Length of this string, in bytes
    pub fn len(&self) -> usize {
        match self {
            LuaString::UNICODE(s) => s.len(),
            LuaString::BINARY(s) => s.len(),
        }
    }

    /// Returns true if this is a utf-8 string
    pub fn is_utf8(&self) -> bool {
        match self {
            LuaString::UNICODE(_) => true,
            LuaString::BINARY(_) => false,
        }
    }
}

impl AsRef<[u8]> for LuaString {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl Display for LuaString {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            LuaString::UNICODE(string) => {
                write!(f, "{}", *string)
            }
            LuaString::BINARY(bytes) => {
                write!(f, "{:X?}", bytes)
            }
        }
    }
}

impl PartialEq for LuaString {
    fn eq(&self, other: &Self) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}

impl PartialEq<str> for LuaString {
    fn eq(&self, other: &str) -> bool {
        match self {
            LuaString::UNICODE(u) => u.as_ref() == other,
            LuaString::BINARY(b) => b.as_ref() == other.as_bytes(),
        }
    }
}

impl PartialOrd for LuaString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_bytes().partial_cmp(&other.as_bytes())
    }
}
