//! Module for [`LuaValue`]
//!
//! See value type modules for details on each concrete Lua type
//!
//! See <https://www.lua.org/manual/5.4/manual.html#2.1> for details on Lua's value types

use std::borrow::Cow;
use crate::types::value::number::LuaNumber;
use crate::types::value::string::LuaString;
use crate::types::value::userdata::UserData;
use crate::types::value::function::LuaFunction;
use crate::types::value::thread::LuaThread;
use crate::types::value::table::LuaTable;
use crate::error::{CannotCoerceError, InvalidValueError, LuaError, AttemptToCallNonFunctionError, InvalidKeyError, CannotIndexTypeError, InvalidIndexationError};
use crate::constants::types::{LUA_FLOAT, LUA_INT};
use std::ops::Not;
use std::fmt::{Debug, Display, Formatter};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Sub, Mul, Div, Rem, BitAnd, BitOr, BitXor, Shl, Shr, Neg};
use std::cmp::Ordering;
use std::hint::unreachable_unchecked;
use crate::types::{AsLuaPointer, LuaType, CoerceFrom};
use crate::vm::LuaVM;
use crate::types::varargs::Varargs;

pub mod number;
pub mod string;
pub mod table;
pub mod userdata;
pub mod function;
pub mod thread;

/// Metatables for various types
pub struct TypeMetatables {
    pub(crate) boolean: Option<LuaTable>,
    pub(crate) number: Option<LuaTable>,
    pub(crate) string: Option<LuaTable>,
    pub(crate) function: Option<LuaTable>,
    pub(crate) thread: Option<LuaTable>,
}

/// Top level type for singular Lua values
///
/// Should be treated as "nullable" via the [`LuaValue::NIL`] variant
#[derive(Clone, PartialEq)]
pub enum LuaValue {
    /// Lua 'nil' value & type
    NIL,
    /// Lua boolean type
    BOOLEAN(bool),
    /// Lua number type
    NUMBER(LuaNumber),
    /// Lua string type
    STRING(LuaString),
    /// Lua userdata type
    USERDATA(UserData),
    /// Lua function type
    FUNCTION(LuaFunction),
    /// Lua thread type
    THREAD(LuaThread),
    /// Lua table type
    TABLE(LuaTable),
}

impl LuaValue {
    /// Constant LuaValue::NIL, used for array construction
    pub const CONST_NIL: LuaValue = LuaValue::NIL;

    /// Returns the length of this LuaValue
    ///
    /// For String values, this is the length of the string in bytes.
    /// For Table values, this is the size of the table
    ///
    /// TODO: Update this documentation if table-length is changed to refer to array-part
    pub fn len(&self) -> Result<LuaValue, InvalidValueError> {
        match self {
            LuaValue::STRING(s) => Ok(LuaValue::NUMBER(LuaNumber::from(s.len()))),
            LuaValue::TABLE(t) => Ok(LuaValue::NUMBER(LuaNumber::from(t.len()))),
            _ => Err(InvalidValueError { expected: "String or table", found: self.type_name() })
        }
    }

    /// Concatenates several LuaStrings. Implementation of Lua's `..` operator.
    ///
    /// Raises error if non-string LuaValues are passed.
    ///
    /// Rust callers may use [`LuaString::concat`] directly instead
    pub fn concat(values: &[&LuaValue]) -> Result<LuaValue, LuaError> {
        let mut new_len = 0usize;
        for value in values {
            if let LuaValue::STRING(s) = value {
                new_len = (new_len.checked_add(s.len())).ok_or(LuaError::new("Concatenation too large"))?
            } else {
                if let Ok(s) = LuaString::coerce_from(*value) {
                    new_len = (new_len.checked_add(s.len())).ok_or(LuaError::new("Concatenation too large"))?
                } else {
                    return Err(LuaError::from(InvalidValueError { expected: "string", found: value.type_name() }));
                }
            }
        }
        let mut buffer: Vec<u8> = Vec::with_capacity(new_len); // TODO: Fix potential for huge allocations
        for value in values {
            if let LuaValue::STRING(s) = value {
                buffer.extend_from_slice(s.as_bytes());
            } else {
                if let Ok(s) = LuaString::coerce_from(*value) {
                    buffer.extend_from_slice(s.as_bytes());
                } else {
                    unsafe { unreachable_unchecked() }  // We already checked that each value is a ::STRING in the above loop
                }
            }
        }
        Ok(LuaValue::STRING(LuaString::from(buffer)))
    }

    /// Indexes metatable of this value, returning NIL if value has no metatable, or metatable has no field for key
    pub fn index_metatable<'a, K: Into<LuaKey>>(&self, key: K, metatables: &TypeMetatables) -> LuaValue {
        match self {
            LuaValue::NIL => LuaValue::NIL, // NIL has no metatable
            LuaValue::BOOLEAN(_) => metatables.boolean.as_ref().map(|table| table.raw_get(key)).unwrap_or(LuaValue::NIL),  // Metatable lookups are a raw get
            LuaValue::NUMBER(_) => metatables.number.as_ref().map(|table| table.raw_get(key)).unwrap_or(LuaValue::NIL),
            LuaValue::STRING(_) => metatables.string.as_ref().map(|table| table.raw_get(key)).unwrap_or(LuaValue::NIL),
            LuaValue::USERDATA(userdata) => userdata.metatable().map(|table| table.raw_get(key)).unwrap_or(LuaValue::NIL),
            LuaValue::FUNCTION(_) => metatables.function.as_ref().map(|table| table.raw_get(key)).unwrap_or(LuaValue::NIL),
            LuaValue::TABLE(table) => table.index_metatable(key),
            LuaValue::THREAD(_) => metatables.thread.as_ref().map(|table| table.raw_get(key)).unwrap_or(LuaValue::NIL),
        }
    }

    /// Returns a copy of this value's metatable
    pub fn clone_metatable(&self, metatables: &TypeMetatables) -> Option<LuaTable> {
        match self {
            LuaValue::NIL => None,
            LuaValue::BOOLEAN(_) => metatables.boolean.clone(),
            LuaValue::NUMBER(_) => metatables.number.clone(),
            LuaValue::STRING(_) => metatables.string.clone(),
            LuaValue::USERDATA(userdata) => userdata.metatable().cloned(),
            LuaValue::FUNCTION(_) => metatables.function.clone(),
            LuaValue::TABLE(table) => table.clone_metatable(),
            LuaValue::THREAD(_) => metatables.thread.clone(),
        }
    }

    /// Indexes into this value, implementing Lua indexation `self[key]`
    pub fn index(&self, key: &LuaValue, metatables: &TypeMetatables) -> Result<LuaValue, InvalidIndexationError> {
        if let LuaValue::TABLE(table) = self {
            if let LuaValue::NIL = key {
                // Metatable lookup, else nil
                if let Some(value) = self.index_metatable("__index", metatables).not_nil() {
                    return value.index(key, metatables);   // TODO: Ensure table ownership is an acyclic-graph
                }
                Ok(LuaValue::NIL)
            } else {
                match table.raw_get(key.clone().try_key()?) {
                    LuaValue::NIL => {    // Fallthrough to metatable lookup
                        if let Some(value) = self.index_metatable("__index", metatables).not_nil() {
                            return value.index(key, metatables);   // TODO: Ensure table ownership is an acyclic-graph
                        }
                        Ok(LuaValue::NIL)
                    }
                    val => return Ok(val)
                }
            }
        } else {
            if let Some(value) = self.index_metatable("__index", metatables).not_nil() {
                return value.index(key, metatables);   // TODO: Ensure table ownership is an acyclic-graph
            }
            Err(CannotIndexTypeError { indexed_type: self.type_name() })?
        }
    }

    /// Writes value into this value, implementing Lua indexation `self[key] = value`
    ///
    /// # Arguments
    ///
    /// * `key`: Key of index to set
    /// * `value`: Value to write to index
    /// * `metatables`: Metatables for Lua Types, generally retrieved from [`LuaVM::metatables`]
    ///
    /// returns: Result<(), InvalidIndexationError>
    pub fn set_index(&self, key: LuaValue, value: LuaValue, metatables: &TypeMetatables) -> Result<(), InvalidIndexationError> {
        if let LuaValue::TABLE(table) = self {
            match table.raw_get(key.clone().try_key()?) {
                LuaValue::NIL => {
                    // check __newindex first
                    if let Some(metavalue) = self.index_metatable("__newindex", metatables).not_nil() {
                        return match metavalue {   // If __newindex is a function, call it. Else, attempt to index it.
                            LuaValue::FUNCTION(function) => todo!(),
                            table @ _ => table.set_index(key, value, metatables),
                        };
                    }
                    table.raw_set(key.try_key()?, value);
                }
                _ => table.raw_set(key.try_key()?, value),
            }
        } else {
            if let Some(metavalue) = self.index_metatable("__newindex", metatables).not_nil() {
                return match metavalue {   // If __newindex is a function, call it. Else, attempt to index it.
                    LuaValue::FUNCTION(function) => todo!(),
                    table @ _ => table.set_index(key, value, metatables),
                };
            }
            Err(CannotIndexTypeError { indexed_type: self.type_name() })?
        }
        Ok(())
    }

    /// Handles the `__call` metamethod
    /// Returns a pair of the function to call, and parameters to prefix
    pub(crate) fn prep_call_with_metatable(self, metatables: &TypeMetatables) -> Result<(LuaFunction, Vec<LuaValue>), AttemptToCallNonFunctionError> {
        self.prep_call_with_metatable_and_prefix(Vec::new(), metatables)
    }

    /// Prepares call given a parameter prefix
    fn prep_call_with_metatable_and_prefix(self, prefix: Vec<LuaValue>, metatables: &TypeMetatables) -> Result<(LuaFunction, Vec<LuaValue>), AttemptToCallNonFunctionError> {
        match self {
            LuaValue::FUNCTION(function) => Ok((function, Vec::new())),
            _ => {
                if let Some(value) = self.index_metatable("__call", metatables).not_nil() {
                    let (function, mut new_prefix) = value.prep_call_with_metatable_and_prefix(prefix, metatables)?;
                    new_prefix.push(self);
                    Ok((function, new_prefix))
                } else {
                    Err(AttemptToCallNonFunctionError { called_type: self.type_name() })
                }
            }
        }
    }

    /// Calls this LuaValue, errors if this value is not a function and does not have a `__call` metamethod.
    ///
    /// Implements Lua calls `self(param1, param2, ..., paramN)`
    ///
    /// # Arguments
    ///
    /// * `lua_vm`: LuaVM for this call
    /// * `params`: Parameters of this call
    ///
    /// returns: Result<Varargs, LuaError>
    pub fn call(self, lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
        if let LuaValue::FUNCTION(function) = self {
            function.call(lua_vm, params)
        } else {
            let (function, mut param_prefix) = self.prep_call_with_metatable(&lua_vm.metatables)?;
            if param_prefix.len() > 0 { // Avoid copying params if we don't need to prefix
                param_prefix.extend_from_slice(params);
                function.call(lua_vm, &param_prefix[..])
            } else {
                function.call(lua_vm, params)
            }
        }
    }

    /// Call a method of this LuaValue, errors if the method field is not a function and does not have a `__call` metamethod.
    ///
    /// Implements Lua calls `self:name(param1, param2, ..., paramN)`
    ///
    /// # Arguments
    ///
    /// * `name`: Name of method to call
    /// * `lua_vm`: LuaVM for this call
    /// * `params`: Parameters of this call
    ///
    /// returns: Result<Varargs, LuaError>
    pub fn method_call(self, name: &str, lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
        let method = self.index(&LuaValue::from(name), &lua_vm.metatables)?;
        let mut arguments = Vec::with_capacity(params.len() + 1);
        arguments.push(self);
        arguments.extend_from_slice(params);
        method.call(lua_vm, &arguments)
    }

    /// Returns reference to this value if it is not NIL, or None if it is NIL
    ///
    /// See [`LuaValue::not_nil`] for variant that takes value
    pub fn non_nil(&self) -> Option<&Self> {
        match self {
            LuaValue::NIL => None,
            _ => Some(self)
        }
    }

    /// Returns this value if it is not NIL, or None if it is NIL
    ///
    /// See [`LuaValue::not_nil`] for variant that takes reference
    pub fn not_nil(self) -> Option<Self> {
        match self {
            LuaValue::NIL => None,
            _ => Some(self)
        }
    }

    /// Returns True if this is value is "truthy"; All values besides `NIL` and `false` are truthy
    pub fn is_truthy(&self) -> bool {
        match self {
            LuaValue::NIL => false,
            LuaValue::BOOLEAN(boolean) => *boolean,
            _ => true
        }
    }

    /// Converts LuaValue to String representation. Implements Lua `tostring()`
    ///
    /// # Arguments
    ///
    /// * `lua_vm`: LuaVM, used to resolve __tostring
    ///
    /// returns: Result<Cow<LuaValue>, LuaError>
    pub fn lua_to_string(&self, lua_vm: &mut LuaVM) -> Result<Cow<LuaValue>, LuaError> {
        if let Some(function) = self.index_metatable("__tostring", &lua_vm.metatables).not_nil() {
            return Ok(Cow::Owned(function.call(lua_vm, &[self.clone()])?.into_first()));
        }
        Ok(Cow::Borrowed(self))
    }
}

impl LuaType for LuaValue {
    const TYPE_NAME: &'static str = "value";

    fn type_name(&self) -> &'static str {
        match self {
            LuaValue::NIL => super::Nil.type_name(),
            LuaValue::BOOLEAN(b) => b.type_name(),
            LuaValue::NUMBER(n) => n.type_name(),
            LuaValue::STRING(s) => s.type_name(),
            LuaValue::USERDATA(u) => u.type_name(),
            LuaValue::FUNCTION(f) => f.type_name(),
            LuaValue::THREAD(t) => t.type_name(),
            LuaValue::TABLE(t) => t.type_name(),
        }
    }
}

impl Default for LuaValue {
    fn default() -> Self {
        LuaValue::NIL
    }
}

// TODO: Copy all From<> implementations from variants

impl From<bool> for LuaValue {
    fn from(b: bool) -> Self {
        LuaValue::BOOLEAN(b)
    }
}

impl From<LuaNumber> for LuaValue {
    fn from(n: LuaNumber) -> Self {
        LuaValue::NUMBER(n)
    }
}

impl From<LuaString> for LuaValue {
    fn from(s: LuaString) -> Self {
        LuaValue::STRING(s)
    }
}

impl From<LUA_INT> for LuaValue {
    fn from(i: LUA_INT) -> Self {
        LuaValue::NUMBER(LuaNumber::INT(i))
    }
}

impl From<LUA_FLOAT> for LuaValue {
    fn from(f: LUA_FLOAT) -> Self {
        LuaValue::NUMBER(LuaNumber::FLOAT(f))
    }
}

impl From<usize> for LuaValue {
    fn from(i: usize) -> Self {
        LuaValue::NUMBER(LuaNumber::from(i))
    }
}

impl From<u64> for LuaValue {
    fn from(i: u64) -> Self {
        LuaValue::NUMBER(LuaNumber::from(i))
    }
}

impl From<&str> for LuaValue {
    fn from(string: &str) -> Self {
        LuaValue::STRING(LuaString::from(string))
    }
}

impl From<String> for LuaValue {
    fn from(s: String) -> Self {
        LuaValue::STRING(LuaString::from(s))
    }
}

impl From<&[u8]> for LuaValue {
    fn from(string: &[u8]) -> Self {
        LuaValue::STRING(LuaString::from(string))
    }
}

impl From<Box<[u8]>> for LuaValue {
    fn from(b: Box<[u8]>) -> Self {
        LuaValue::STRING(LuaString::from(b))
    }
}

impl From<Vec<u8>> for LuaValue {
    fn from(v: Vec<u8>) -> Self {
        LuaValue::STRING(LuaString::from(v))
    }
}

impl From<UserData> for LuaValue {
    fn from(u: UserData) -> Self {
        LuaValue::USERDATA(u)
    }
}

impl From<LuaFunction> for LuaValue {
    fn from(f: LuaFunction) -> Self {
        LuaValue::FUNCTION(f)
    }
}

impl From<LuaThread> for LuaValue {
    fn from(t: LuaThread) -> Self {
        LuaValue::THREAD(t)
    }
}

impl From<LuaTable> for LuaValue {
    fn from(t: LuaTable) -> Self {
        LuaValue::TABLE(t)
    }
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaValue {
    fn coerce_opt(value: &T) -> Option<Self> {
        Some(T::clone(value).into())
    }
}

impl LuaValue {
    pub fn nil() -> LuaValue {
        LuaValue::NIL
    }
}

impl Add for &LuaValue {
    type Output = Result<LuaValue, CannotCoerceError>;
    fn add(self, rhs: Self) -> Self::Output {
        Ok(LuaValue::from(LuaNumber::coerce_from(self)? + LuaNumber::coerce_from(rhs)?))
    }
}

impl Sub for &LuaValue {
    type Output = Result<LuaValue, CannotCoerceError>;
    fn sub(self, rhs: Self) -> Self::Output {
        Ok(LuaValue::from(LuaNumber::coerce_from(self)? - LuaNumber::coerce_from(rhs)?))
    }
}

impl Mul for &LuaValue {
    type Output = Result<LuaValue, CannotCoerceError>;
    fn mul(self, rhs: Self) -> Self::Output {
        Ok(LuaValue::from(LuaNumber::coerce_from(self)? * LuaNumber::coerce_from(rhs)?))
    }
}

impl Div for &LuaValue {
    type Output = Result<LuaValue, CannotCoerceError>;
    fn div(self, rhs: Self) -> Self::Output {
        Ok(LuaValue::from(LuaNumber::coerce_from(self)? / LuaNumber::coerce_from(rhs)?))
    }
}

impl Rem for &LuaValue {
    type Output = Result<LuaValue, CannotCoerceError>;
    fn rem(self, rhs: Self) -> Self::Output {
        Ok(LuaValue::from(LuaNumber::coerce_from(self)? % LuaNumber::coerce_from(rhs)?))
    }
}

impl LuaValue {
    pub fn pow(&self, rhs: &Self) -> Result<LuaValue, CannotCoerceError> {
        Ok(LuaValue::from(LuaNumber::coerce_from(self)?.pow(LuaNumber::coerce_from(rhs)?)))
    }
}

impl LuaValue {
    pub fn idiv(&self, rhs: &Self) -> Result<LuaValue, CannotCoerceError> {
        Ok(LuaValue::from(LuaNumber::coerce_from(self)?.idiv(LuaNumber::coerce_from(rhs)?)))
    }
}

impl BitAnd for &LuaValue {
    type Output = Result<LuaValue, CannotCoerceError>;
    fn bitand(self, rhs: Self) -> Self::Output {
        Ok(LuaValue::from((LuaNumber::coerce_from(self)? & LuaNumber::coerce_from(rhs)?)?))
    }
}

impl BitOr for &LuaValue {
    type Output = Result<LuaValue, CannotCoerceError>;
    fn bitor(self, rhs: Self) -> Self::Output {
        Ok(LuaValue::from((LuaNumber::coerce_from(self)? | LuaNumber::coerce_from(rhs)?)?))
    }
}

impl BitXor for &LuaValue {
    type Output = Result<LuaValue, CannotCoerceError>;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Ok(LuaValue::from((LuaNumber::coerce_from(self)? ^ LuaNumber::coerce_from(rhs)?)?))
    }
}

impl Shl for &LuaValue {
    type Output = Result<LuaValue, CannotCoerceError>;
    fn shl(self, rhs: Self) -> Self::Output {
        Ok(LuaValue::from((LuaNumber::coerce_from(self)? << LuaNumber::coerce_from(rhs)?)?))
    }
}

impl Shr for &LuaValue {
    type Output = Result<LuaValue, CannotCoerceError>;
    fn shr(self, rhs: Self) -> Self::Output {
        Ok(LuaValue::from((LuaNumber::coerce_from(self)? >> LuaNumber::coerce_from(rhs)?)?))
    }
}

impl Neg for &LuaValue {
    type Output = Result<LuaValue, CannotCoerceError>;
    fn neg(self) -> Self::Output {
        Ok(LuaValue::from(-LuaNumber::coerce_from(self)?))
    }
}

impl LuaValue {
    pub fn bnot(&self) -> Result<LuaValue, CannotCoerceError> {
        Ok(LuaValue::from(LuaNumber::coerce_from(self)?.bnot()?))
    }
}

impl Not for &LuaValue {
    type Output = Result<LuaValue, CannotCoerceError>;

    fn not(self) -> Self::Output {
        Ok(LuaValue::from(!bool::coerce_from(self)?))
    }
}

impl PartialOrd for LuaValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (LuaValue::NUMBER(lhs), LuaValue::NUMBER(rhs)) => lhs.partial_cmp(rhs),
            (LuaValue::STRING(lhs), LuaValue::STRING(rhs)) => lhs.partial_cmp(rhs),
            _ => None
        }
    }
}

impl Debug for LuaValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LuaValue::NIL => f.debug_tuple("LuaNil").finish(),
            LuaValue::BOOLEAN(bool) => f.debug_tuple("LuaBool").field(bool).finish(),
            LuaValue::NUMBER(number) => f.debug_tuple("LuaNumber").field(number).finish(),
            LuaValue::STRING(string) => f.debug_tuple("LuaString").field(string).finish(),
            LuaValue::USERDATA(userdata) => f.debug_tuple("LuaUserdata").field(userdata).finish(),
            LuaValue::FUNCTION(function) => f.debug_tuple("LuaFunction").field(function).finish(),
            LuaValue::THREAD(thread) => f.debug_tuple("LuaThread").field(thread).finish(),
            LuaValue::TABLE(table) => f.debug_tuple("LuaTable").field(table).finish(),
        }
    }
}

impl Display for LuaValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            LuaValue::NIL => write!(f, "NIL"),
            LuaValue::BOOLEAN(b) => write!(f, "{}", b),
            LuaValue::NUMBER(n) => write!(f, "{}", n),
            LuaValue::STRING(s) => write!(f, "{}", s),
            LuaValue::USERDATA(u) => write!(f, "{}", u),
            LuaValue::FUNCTION(function) => write!(f, "{}", function),
            LuaValue::THREAD(thread) => write!(f, "{}", thread),
            LuaValue::TABLE(table) => write!(f, "{}", table),
        }
    }
}

/// Newtype for LuaValue that is a valid hash key i.e. not NIL or NaN
///
/// Also implements [`Eq`] for full equality
#[derive(Debug, Clone)]
pub struct LuaKey {
    inner: LuaValue,
}

impl LuaValue {
    /// Returns this value as a LuaKey if it is a valid key, Err otherwise
    pub fn try_key(self) -> Result<LuaKey, InvalidKeyError> {
        if self.non_nil().is_none() {
            Err(InvalidKeyError::KeyIsNil)
        } else if self != self {    // As a safeguard against bugs, instead of testing for NaN, we test for the reflexive property directly as this guarantees that Eq implementation is valid
            Err(InvalidKeyError::KeyIsNaN)
        } else {
            return Ok(LuaKey { inner: self });
        }
    }
}

impl Eq for LuaKey {}

impl PartialEq for LuaKey {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl Hash for LuaKey {
    fn hash<H: Hasher>(&self, state: &mut H) {  // Does not use Lua's hashing algorithm
        match &self.inner {
            LuaValue::NIL => unreachable!(),    // NIL is an invalid key, and thus should never be hashed
            LuaValue::BOOLEAN(b) => state.write_u8(if *b { 1 } else { 0 }),
            LuaValue::NUMBER(n) => {
                match n {
                    LuaNumber::INT(int) => state.write_i64(*int),
                    LuaNumber::FLOAT(float) => {
                        if (*float as LUA_INT) as LUA_FLOAT == *float {
                            state.write_i64(*float as LUA_INT)
                        } else {
                            state.write(&float.to_le_bytes())
                        }
                    }
                }
            }
            LuaValue::STRING(s) => state.write(s.as_bytes()),
            LuaValue::USERDATA(u) => state.write_usize(u.as_lua_pointer()),
            LuaValue::FUNCTION(f) => state.write_usize(f.as_lua_pointer()),
            LuaValue::THREAD(t) => state.write_usize(t.as_lua_pointer()),
            LuaValue::TABLE(t) => state.write_usize(t.as_lua_pointer()),
        }
    }
}

// TODO: Copy over all From<> implementations for LuaValue; This cannot be done with a blanket implementation as not all LuaValues are valid keys.
impl From<bool> for LuaKey {
    fn from(b: bool) -> Self {
        LuaKey { inner: LuaValue::BOOLEAN(b) }
    }
}

impl From<LuaNumber> for LuaKey {
    fn from(n: LuaNumber) -> Self {
        LuaKey { inner: LuaValue::NUMBER(n) }
    }
}

impl From<LuaString> for LuaKey {
    fn from(s: LuaString) -> Self {
        LuaKey { inner: LuaValue::STRING(s) }
    }
}

impl From<LUA_INT> for LuaKey {
    fn from(i: LUA_INT) -> Self {
        LuaKey { inner: LuaValue::NUMBER(LuaNumber::INT(i)) }
    }
}

impl From<usize> for LuaKey {
    fn from(i: usize) -> Self {
        LuaKey { inner: LuaValue::NUMBER(LuaNumber::from(i)) }
    }
}

impl From<u64> for LuaKey {
    fn from(i: u64) -> Self {
        LuaKey { inner: LuaValue::NUMBER(LuaNumber::from(i)) }
    }
}

impl From<&str> for LuaKey {
    fn from(string: &str) -> Self {
        LuaKey { inner: LuaValue::STRING(LuaString::from(string)) }
    }
}

impl From<String> for LuaKey {
    fn from(s: String) -> Self {
        LuaKey { inner: LuaValue::STRING(LuaString::from(s)) }
    }
}

impl From<&[u8]> for LuaKey {
    fn from(string: &[u8]) -> Self {
        LuaKey { inner: LuaValue::STRING(LuaString::from(string)) }
    }
}

impl From<Box<[u8]>> for LuaKey {
    fn from(b: Box<[u8]>) -> Self {
        LuaKey { inner: LuaValue::STRING(LuaString::from(b)) }
    }
}

impl From<Vec<u8>> for LuaKey {
    fn from(v: Vec<u8>) -> Self {
        LuaKey { inner: LuaValue::STRING(LuaString::from(v)) }
    }
}

impl From<UserData> for LuaKey {
    fn from(u: UserData) -> Self {
        LuaKey { inner: LuaValue::USERDATA(u) }
    }
}

impl From<LuaFunction> for LuaKey {
    fn from(f: LuaFunction) -> Self {
        LuaKey { inner: LuaValue::FUNCTION(f) }
    }
}

impl From<LuaThread> for LuaKey {
    fn from(t: LuaThread) -> Self {
        LuaKey { inner: LuaValue::THREAD(t) }
    }
}

impl From<LuaTable> for LuaKey {
    fn from(t: LuaTable) -> Self {
        LuaKey { inner: LuaValue::TABLE(t) }
    }
}