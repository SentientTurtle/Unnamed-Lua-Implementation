//! Module for lua function parameters
//!
//! The main type for Lua parameters is [`&[LuaValue]`], for which this module provides an extension trait ([`LuaParameters`])

use crate::types::value::LuaValue;
use crate::error::{ArgumentError, IntoArgumentError, InvalidValueError};
use crate::types::{LuaType, CoerceFrom};
use crate::util::Union2;

/// Extension trait for Lua function parameters
pub trait LuaParameters {
    /// Alias for [`<[]>::get`], used in default functions below
    fn get(&self, index: usize) -> Option<&LuaValue>;

    /// Returns first parameter, or error if no parameters were given
    fn first_value(&self) -> Result<&LuaValue, ArgumentError> {
        self.get(0).ok_or(
            ArgumentError::InvalidValue {
                error: InvalidValueError { expected: "value", found: "no value" },
                argument_index: 0
            }
        )
    }

    /// Returns first parameter, or nil if no parameters were given
    fn first_or_nil(&self) -> &LuaValue {
        self.get(0).unwrap_or(&LuaValue::NIL)
    }

    /// Returns specified parameter, or nil if index is out of range
    ///
    /// # Arguments
    ///
    /// * `index`: Index of parameter, starting at 0
    ///
    /// returns: &LuaValue
    fn get_or_nil(&self, index: usize) -> &LuaValue {
        self.get(index).unwrap_or(&LuaValue::NIL)
    }

    /// Unpacks first N arguments into an array, filling missing values with nil
    fn unpack<const N: usize>(&self) -> [&LuaValue; N] {
        let mut array = [&LuaValue::NIL; N];
        for i in 0..N {
            array[i] = self.get_or_nil(i)
        }
        array
    }

    /// Unpacks first N arguments into an array of owned LuaValues, filling missing values with nil
    fn unpack_owned<const N: usize>(&self) -> [LuaValue; N] {
        let mut array = [LuaValue::CONST_NIL; N];
        for i in 0..N {
            array[i] = self.get(i)
                .map(Clone::clone)
                .unwrap_or(LuaValue::NIL)
        }
        array
    }

    /// Returns specified parameter, coerced to T
    ///
    /// If index is out of range, an attempt is made to coerce nil to T
    ///
    /// # Arguments
    ///
    /// * `index`: Index of parameter, starting at 0
    ///
    /// returns: Result<T, ArgumentError>
    fn coerce<T: CoerceFrom<LuaValue>>(&self, index: usize) -> Result<T, ArgumentError> where T: LuaType {
        let value = self.get_or_nil(index);
        T::coerce_from(value)
            .map_err(|arg_err| arg_err.error_for_argument(index))
    }

    /// Returns specified parameter, coerced to T
    ///
    /// If index is out of range, returns Ok(None)
    ///
    /// # Arguments
    ///
    /// * `index`: Index of parameter, starting at 0
    ///
    /// returns: Result<T, ArgumentError>
    fn coerce_opt<T: CoerceFrom<LuaValue>>(&self, index: usize) -> Result<Option<T>, ArgumentError> where T: LuaType {
        self.get(index)
            .map(|value| T::coerce_from(value))
            .transpose()
            .map_err(|err| err.error_for_argument(index))
    }


    /// Returns specified parameter, coerced to either T or U, erroring if value cannot be coerced to either type
    ///
    /// If index is out of range, an attempt is made to coerce nil to T or U
    ///
    /// # Arguments
    ///
    /// * `index`: Index of parameter, starting at 0
    ///
    /// returns: Result<T, ArgumentError>
    fn coerce_any<T: CoerceFrom<LuaValue>, U: CoerceFrom<LuaValue>>(&self, index: usize) -> Result<Union2<T, U>, ArgumentError> where T: LuaType, U: LuaType {
        let value = self.get_or_nil(index);
        T::coerce_from(value)
            .map(Union2::T)
            .or_else(|_| U::coerce_from(value).map(Union2::U))
            .map_err(|err| err.error_for_argument(index))
    }
}

impl LuaParameters for [LuaValue] {
    #[inline(always)]   // Direct delegation
    fn get(&self, index: usize) -> Option<&LuaValue> {
        self.get(index)
    }
}
