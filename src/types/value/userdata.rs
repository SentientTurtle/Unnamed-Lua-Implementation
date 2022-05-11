//! Module for Lua 'userdata' type

use crate::types::value::table::LuaTable;
use std::any::Any;
use std::rc::Rc;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::types::{AsLuaPointer, ref_to_pointer, LuaType, CoerceFrom};
use crate::types::value::LuaValue;
use crate::vm::LuaVM;
use crate::error::InvalidValueError;

/// Trait for values that can be stored in userdata
pub trait UserDataValue {
    /// Name for implementer's type,
    const TYPE_NAME: &'static str;

    /// Retrieves metatable for this userdata type
    ///
    /// Expensive metatable creation should be cached in [`LuaVM::registry`]
    ///
    /// # Arguments
    ///
    /// * `lua_vm`: LuaVM for userdata
    ///
    /// returns: Option<LuaTable>
    ///
    /// # Example implementation
    ///
    /// ```
    /// use lua_interpreter::types::CoerceFrom;
    /// use lua_interpreter::types::value::table::LuaTable;
    /// fn userdata_metatable(lua_vm: &mut LuaVM) -> Option<LuaTable> {
    ///    LuaTable::coerce_opt(&lua_vm.registry.raw_get("filehandle-metatable"))
    /// }
    /// ```
    fn userdata_metatable(lua_vm: &mut LuaVM) -> Option<LuaTable>;
}

/// Struct for international implementation of userdata
#[derive(Debug)]
struct UserDataImpl {
    metatable: Option<LuaTable>,
    value: Box<dyn Any>,
    type_name: &'static str // Kind a hack, but the associated constant cannot be retrieved through UserDataValue
}

/// Lua Userdata, value that wraps arbitrary rust values into a Lua value
///
/// Wrapped values must implement [`UserDataValue`], existing types should be wrapped in a newtype
///
/// Warning: It is not possible to unwrap userdata and retrieve the original concrete type, to accomplish such unwrapping, the userdata value must be wrapped in a Cell providing interior mutability
#[derive(Clone, Debug)]
pub struct UserData {
    inner: Rc<UserDataImpl>,
}

impl LuaType for UserData {
    const TYPE_NAME: &'static str = "userdata";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for UserData {
    fn coerce_opt(value: &T) -> Option<Self> {
        if let LuaValue::USERDATA(userdata) = value.clone().into() {
            Some(userdata)
        } else {
            None
        }
    }
}

impl AsLuaPointer for UserData {
    fn as_lua_pointer(&self) -> usize {
        ref_to_pointer(self.inner.as_ref())
    }
}

impl UserData {
    /// Creates new userdata from specified value. Metatable is retrieved from [`UserDataValue::userdata_metatable`]
    pub fn new<T: UserDataValue + 'static>(value: T, lua_vm: &mut LuaVM) -> UserData {
        UserData {
            inner: Rc::new(UserDataImpl { metatable: T::userdata_metatable(lua_vm), type_name: T::TYPE_NAME, value: Box::new(value) })
        }
    }

    /// Returns a reference to this userdata's metatable, if one is set
    pub fn metatable(&self) -> Option<&LuaTable> {
        self.inner.metatable.as_ref()
    }

    /// Returns a reference to this userdata's rust value
    pub fn value(&self) -> &dyn Any {
        &self.inner.value
    }

    /// Attempts to downcast rust value to specified type, raising a Lua error if the wrong type is specified
    pub fn downcast<T: UserDataValue + 'static>(&self) -> Result<&T, InvalidValueError> {
        (*self.inner.value).downcast_ref::<T>().ok_or(InvalidValueError { expected: T::TYPE_NAME, found: self.inner.type_name })
    }

    /// Not usable; Function calls only have access to references to LuaValues, and thus cannot obtain owned Userdata without a 2nd strong Rc existing, resulting in Rc::try_unwrap failing
    #[allow(unused)]
    fn unwrap(self) -> Option<Box<dyn Any>> {
        Rc::try_unwrap(self.inner)
            .ok()
            .map(|inner| inner.value)
    }
}

impl Display for UserData {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "USERDATA[{}]@{}", self.inner.type_name, self.as_lua_pointer())
    }
}

impl PartialEq for UserData {
    fn eq(&self, other: &Self) -> bool {
        self.as_lua_pointer() == other.as_lua_pointer()
    }
}
