//! Module for Lua upvalues
//!
//! See <https://www.lua.org/manual/5.4/manual.html#3.5> for details on upvalues

use std::cell::RefCell;
use std::rc::Rc;
use crate::types::value::LuaValue;
use std::fmt::{Display, Formatter, Debug};
use std::fmt;

/// Upvalue description
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct UpvalueDesc {
    /// Denotes whether upvalue is in stack or not
    instack: u8,
    /// Register index of upvalue
    idx: u8,
    kind: u8
}

impl UpvalueDesc {
    pub fn new(instack: u8, idx: u8, kind: u8) -> Self {
        UpvalueDesc { instack, idx, kind }
    }

    pub fn index(&self) -> usize {
        self.idx as usize
    }

    pub fn in_stack(&self) -> bool {
        self.instack != 0
    }

    pub fn bytes(&self) -> [u8; 3] {
        [self.instack, self.idx, self.kind]
    }
}

impl Display for UpvalueDesc {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "instack: {}\tidx:{}", self.instack, self.idx)
    }
}

#[derive(Debug, Clone)]
pub enum UpvalueImpl {
    Open { frame: usize, register: usize },
    Closed(LuaValue),
}

/// Lua upvalues; The non-local variables of a closure
///
/// Each upvalue may either be "open"; Referring to a register in the stack, or closed; containing the value directly
///
/// Upvalues are refcounted and may freely be copied
#[derive(Clone)]
pub struct Upvalue { inner: Rc<RefCell<UpvalueImpl>> }

impl Upvalue {
    /// Creates a new "open" upvalue, referencing value in stack
    ///
    /// # Arguments
    ///
    /// * `register`: Index of register within stackframe
    /// * `stack_index`: Index of stackframe within stack
    ///
    /// returns: Upvalue
    pub fn new_open(register: usize, stack_index: usize) -> Upvalue {
        Upvalue {
            inner: Rc::from(RefCell::from(UpvalueImpl::Open { frame: stack_index, register }))
        }
    }

    /// Creates a new "closed" upvalue, directly containing value
    ///
    /// # Arguments
    ///
    /// * `value`: Value of this upvalue
    ///
    /// returns: Upvalue
    pub fn new_closed(value: LuaValue) -> Upvalue {
        Upvalue {
            inner: Rc::new(RefCell::from(UpvalueImpl::Closed(value)))
        }
    }

    /// Retrieve inner UpvalueImpl
    pub(crate) fn get(&self) -> UpvalueImpl {
        self.inner.borrow().clone()
    }

    /// Close this upvalue, replacing it's reference with specified value
    pub(crate) fn close(&self, value: LuaValue) {
        self.inner.replace(UpvalueImpl::Closed(value));
    }
}

impl Debug for Upvalue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &*self.inner.borrow() {
            UpvalueImpl::Open { frame, register } => {
                f.debug_struct("Upvalue")
                    .field("frame", frame)
                    .field("register", register)
                    .finish()
            }
            UpvalueImpl::Closed(value) => {
                f.debug_tuple("Upvalue")
                    .field(value)
                    .finish()
            }
        }
    }
}