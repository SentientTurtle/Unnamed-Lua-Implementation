//! Top level module for main types used in interacting with Lua
//!
//! See [`value`] module (and submodules) for Lua value types

use std::convert::TryFrom;
use crate::constants::types::{LUA_FLOAT, LUA_INT};
use crate::error::CannotCoerceError;
use crate::types::value::LuaValue;
use crate::types::value::number::LuaNumber;

pub mod varargs;
pub mod parameters;
pub mod value;
pub mod upvalue;

/// Trait for LuaTypes, containing additional debug/error information about the type, shown to Lua scripts
pub trait LuaType: 'static {
    /// Static type name
    ///
    /// Equivalent Lua term for the implementing rust type; E.g. "value" for LuaValue.
    /// Should be specific and unique if possible, as this value is used in errors and debug information
    const TYPE_NAME: &'static str;

    /// Dynamic type name of this value, as returned by Lua's `type()` function.
    ///
    /// This differs from [`Self::TYPE_NAME`] by returning the concrete type name of this instance.
    ///
    /// E.g. LuaValue::TABLE has a [`Self::TYPE_NAME`] of "value" and a [`Self::type_name`] of "table"
    ///
    /// This function must be implemented for types that contain other LuaTypes
    fn type_name(&self) -> &'static str {
        Self::TYPE_NAME
    }
}

impl LuaType for bool {
    const TYPE_NAME: &'static str = "boolean";
}

impl LuaType for usize {
    const TYPE_NAME: &'static str = "positive int";
}

impl LuaType for LUA_INT {
    const TYPE_NAME: &'static str = "int";
}

impl LuaType for LUA_FLOAT {
    const TYPE_NAME: &'static str = "float";
}

/// Conversion from rust references to "Lua Pointers"; Following Lua's equality and identity rules.
///
/// Used internally by the interpreter to handle equality and for debug information
pub(crate) trait AsLuaPointer {
    fn as_lua_pointer(&self) -> usize;
}

/// Utility function to convert a reference to a usize pointer, primarily for use in `AsLuaPointer` implementations
#[inline(always)]
pub(self) fn ref_to_pointer<T: ?Sized>(rf: &T) -> usize {
    rf as *const T as *const () as usize
}

// TODO: Refactor to take value instead of reference
/// Trait for Lua type coercion
pub trait CoerceFrom<T: Into<LuaValue> + Clone>: LuaType + Sized {
    /// Attempts to coerce value to Self, yielding None if coercion is not possible
    ///
    /// If failed coercion is returned up the stack as an error, [`CoerceFrom::coerce_from`] should be used
    ///
    /// # Arguments
    ///
    /// * `value`: Value to coerce
    ///
    /// returns: Option<Self>
    fn coerce_opt(value: &T) -> Option<Self>;

    /// Attempts to coerce value to Self, yielding error if coercion is not possible
    ///
    /// If error is ignored or not returned up the stack, [`CoerceFrom::coerce_opt`] should be used
    ///
    /// # Arguments
    ///
    /// * `value`: Value to coerce
    ///
    /// returns: Result<Self, CannotCoerceError>
    #[inline(always)] // Ensure unused errors are optimized out, as error construction involves cloning
    fn coerce_from(value: &T) -> Result<Self, CannotCoerceError> {
        Self::coerce_opt(value).ok_or(CannotCoerceError { from: T::into(T::clone(value)).type_name(), to: Self::TYPE_NAME })
    }
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for bool {
    fn coerce_opt(value: &T) -> Option<Self> {
        Some(match value.clone().into() {
            LuaValue::NIL => false,
            LuaValue::BOOLEAN(b) => b,
            _ => true
        })
    }
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for usize {
    fn coerce_opt(value: &T) -> Option<Self> {
        LUA_INT::coerce_opt(value)
            .map(usize::try_from)
            .and_then(Result::ok)
    }
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LUA_INT {
    fn coerce_opt(value: &T) -> Option<Self> {
        match value.clone().into() {
            LuaValue::NUMBER(LuaNumber::INT(int)) => Some(int),
            LuaValue::NUMBER(LuaNumber::FLOAT(float)) => if (float as LUA_INT) as LUA_FLOAT == float { Some(float as LUA_INT) } else { None }
            value => LuaNumber::coerce_opt(&value).and_then(|number| number.try_int().ok())
        }
    }
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LUA_FLOAT {
    fn coerce_opt(value: &T) -> Option<Self> {
        match value.clone().into() {
            LuaValue::NUMBER(LuaNumber::INT(int)) => Some(int as LUA_FLOAT),
            LuaValue::NUMBER(LuaNumber::FLOAT(float)) => Some(float),
            value => LuaNumber::coerce_opt(&value).map(|number| number.as_float())
        }
    }
}


/// Zero-sized type to represent Lua's "nil" value in rust generics
pub struct Nil;

impl LuaType for Nil {
    const TYPE_NAME: &'static str = "nil";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for Nil {
    fn coerce_opt(value: &T) -> Option<Self> {
        if let LuaValue::NIL = value.clone().into() {
            Some(Nil)
        } else {
            None
        }
    }
}
