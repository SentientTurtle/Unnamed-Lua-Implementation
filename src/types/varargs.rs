//! Module for Lua varargs
//!
//! See <https://www.lua.org/manual/5.4/manual.html#3.4.11> for details on varargs

use crate::types::value::LuaValue;
use std::ops::{Bound, RangeBounds};

/// Lua varargs, containing (zero to) multiple lua values
///
/// Used as return type for Lua functions
#[derive(Debug)]
pub struct Varargs {
    inner: Vec<LuaValue>
}

impl Varargs {
    /// Constructs a varargs with a single nil value
    pub fn nil() -> Varargs {
        Varargs { inner: vec![LuaValue::NIL] }
    }

    /// Constructs a varargs with a single "fail" value
    ///
    /// See: <https://www.lua.org/manual/5.4/manual.html#6>
    pub fn fail() -> Varargs {
        Varargs {
            inner: vec![Varargs::fail_value()]
        }
    }

    /// Returns the current "fail" value; nil
    ///
    /// See: <https://www.lua.org/manual/5.4/manual.html#6>
    pub fn fail_value() -> LuaValue {
        LuaValue::NIL
    }

    /// Constructs a varargs with no values, does not heap-allocate
    pub fn empty() -> Varargs {
        Varargs { inner: Vec::new() }
    }

    /// Returns first value, or nil if this varargs contains no values
    pub fn first(&self) -> &LuaValue {
        self.inner.get(0).unwrap_or(&LuaValue::NIL)
    }

    /// Returns true if this varargs has 'fail' as it's first value
    pub fn is_fail(&self) -> bool {
        *self.first() == Varargs::fail_value()
    }

    /// Returns first value, or nil if this varargs contains no values
    pub fn into_first(mut self) -> LuaValue {
        self.inner.drain(..).next().unwrap_or(LuaValue::NIL)
    }

    /// Returns first N values as an array, padding with nil
    pub fn into_array<const N: usize>(self) -> [LuaValue; N] {
        let mut array = [LuaValue::CONST_NIL; N];
        for (index, value) in self.inner.into_iter().enumerate() {
            if index < N {
                array[index] = value
            } else {
                break
            }
        }
        array
    }

    /// Amount of values in this varargs
    pub fn count(&self) -> usize {
        self.inner.len()
    }

    /// Returns specified value, or nil if index is out of range
    ///
    /// # Arguments
    ///
    /// * `index`: Index of value to get
    ///
    /// returns: &LuaValue
    pub fn n(&self, index: usize) -> &LuaValue {
        self.inner.get(index).unwrap_or(&LuaValue::NIL)
    }

    /// Returns specified value, or None if index is out of range
    ///
    /// # Arguments
    ///
    /// * `index`: Index of value to get
    ///
    /// returns: &LuaValue
    pub fn opt(&self, index: usize) -> Option<&LuaValue> {
        self.inner.get(index)
    }


    /// Slices this varargs into a new varargs, copies values
    ///
    /// No padding is performed, returned varargs may be smaller than range
    /// Does not error on out of bounds
    ///
    /// # Arguments
    ///
    /// * `range`: Range to slice
    ///
    /// returns: Varargs
    pub fn select_range<T: RangeBounds<usize>>(&self, range: T) -> Varargs {
        macro_rules! slice_end_bound {
            ($skipwhile:expr, $range:expr) => {
                match $range.end_bound() {
                    Bound::Included(i) => $skipwhile.take_while(|kv| kv.0 <= *i).map(|kv| kv.1.clone()).collect(),
                    Bound::Excluded(i) => $skipwhile.take_while(|kv| kv.0 < *i).map(|kv| kv.1.clone()).collect(),
                    Bound::Unbounded => $skipwhile.take_while(|_| true).map(|kv| kv.1.clone()).collect(),
                }
            };
        }

        //noinspection RsLiveness           Used in macro below
        let iter = self.inner.iter().enumerate();
        let inner = match range.start_bound() {
            Bound::Included(i) => slice_end_bound!(iter.skip_while(|kv| kv.0 != *i), range),
            Bound::Excluded(i) => slice_end_bound!(iter.skip_while(|kv| kv.0 != (i + 1)), range),
            Bound::Unbounded => slice_end_bound!(iter.skip_while(|kv| kv.1 == &LuaValue::NIL), range),
        };
        Varargs { inner }
    }

    /// Returns slice of the values in this varargs
    pub fn as_slice(&self) -> &[LuaValue] {
        &self.inner[..]
    }

    /// Prepends a prefix to varargs
    pub fn prepend<T: Into<Varargs>>(prefix: T, varargs: Self) -> Varargs {
        let mut new_varargs = prefix.into();
        new_varargs.inner.extend(varargs.inner.into_iter());
        new_varargs
    }
}

impl From<()> for Varargs {
    fn from(_val: ()) -> Self {
        Varargs::empty()
    }
}

impl<T: Into<LuaValue>> From<T> for Varargs {
    fn from(val: T) -> Self {
        Varargs {
            inner: vec![val.into()]
        }
    }
}

impl<const N: usize> From<[LuaValue; N]> for Varargs {
    fn from(array: [LuaValue; N]) -> Self {
        Varargs {
            inner: Vec::from(array)
        }
    }
}

impl From<&[LuaValue]> for Varargs {
    fn from(slice: &[LuaValue]) -> Self {
        Varargs { inner: Vec::from(slice) }
    }
}

impl From<Vec<LuaValue>> for Varargs {
    fn from(inner: Vec<LuaValue>) -> Self {
        Varargs { inner }
    }
}

impl<T: Into<LuaValue>, U: Into<LuaValue>> From<(T, U)> for Varargs {
    fn from(tuple: (T, U)) -> Self {
        Varargs { inner: vec![tuple.0.into(), tuple.1.into()] }
    }
}

impl<T: Into<LuaValue>, U: Into<LuaValue>, V: Into<LuaValue>> From<(T, U, V)> for Varargs {
    fn from(tuple: (T, U, V)) -> Self {
        Varargs { inner: vec![tuple.0.into(), tuple.1.into(), tuple.2.into()] }
    }
}
