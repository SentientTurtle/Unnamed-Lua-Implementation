//! Module for Lua 'thread' type

use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::fmt;
use std::rc::Rc;
use crate::types::{LuaType, CoerceFrom, AsLuaPointer, ref_to_pointer};
use crate::types::value::function::LuaFunction;
use crate::types::value::LuaValue;

#[derive(Clone)]
pub struct LuaThread {
    inner: Rc<RefCell<ThreadImpl>>
}

impl LuaThread {
    pub fn new(function: LuaFunction) -> LuaThread {
        LuaThread {
            inner: Rc::new(RefCell::new(ThreadImpl::NotStarted(function)))
        }
    }

    pub fn has_started(&self) -> bool {
        if let ThreadImpl::NotStarted(_) = *self.inner.borrow() {
            true
        } else {
            false
        }
    }

    pub fn is_dead(&self) -> bool {
        if let ThreadImpl::Dead = *self.inner.borrow() {
            true
        } else {
            false
        }
    }
}

enum ThreadImpl {
    NotStarted(LuaFunction),
    InStack(usize),
    Dead
}

impl LuaType for LuaThread {
    const TYPE_NAME: &'static str = "thread";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaThread {
    fn coerce_opt(value: &T) -> Option<Self> {
        if let LuaValue::THREAD(thread) = value.clone().into() {
            Some(thread)
        } else {
            None
        }
    }
}

impl AsLuaPointer for LuaThread {
    fn as_lua_pointer(&self) -> usize {
        ref_to_pointer(self.inner.as_ref())
    }
}

impl Debug for LuaThread {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "table:{:X}", self.as_lua_pointer())
    }
}

impl Display for LuaThread {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "table:{:X}", self.as_lua_pointer())
    }
}

impl PartialEq for LuaThread {
    fn eq(&self, other: &Self) -> bool {
        self.as_lua_pointer() == other.as_lua_pointer()
    }
}