//! Module for LuaVM debug functions
//!
//! See [`crate::stdlib::debug`] for debug functions accessible to lua scripts

use std::cell::RefCell;
use crate::types::value::string::LuaString;
use crate::vm::LuaVM;

/// Extension trait for [`LuaVM`] containing debug functions
pub trait VMDebug {
    /// Retrieves debug information for a specified stackframe
    ///
    /// # Arguments
    ///
    /// * `inverse_stack_index`: Index of stackframe for which to retrieve debug info, starting from the top of the stack (i.e. 0 is top of stack, 1 is the previous frame, N is bottom of stack)
    ///
    /// returns: Option<DebugInfo>
    fn get_debug_info(&self, inverse_stack_index: usize) -> Option<DebugInfo>;
}
impl VMDebug for LuaVM {
    #[inline(always)]   // Inline-hint; Creating the full DebugInfo struct requires copying a fair number of values, inlining optimizes unused fields out.
    fn get_debug_info(&self, inverse_stack_index: usize) -> Option<DebugInfo> {
        self.stack.iter().rev().skip(inverse_stack_index).next()
            .map(|frame| {
                // frame.closure
                // DebugInfo {
                //
                // }
                todo!()
            })
    }
}

/// Debug information for a stackframe
pub struct DebugInfo {
    function: (),
    current_line: usize,
    name: Option<LuaString>,
    name_type: (),
    source: Option<LuaString>,
    short_source: (),
    first_line_defined: usize,
    last_line_defined: usize,
    is_tailcall: bool,
}


/// When called in a rust LuaFunction, returns the program counter of the Lua function calling it
///
/// # Arguments
///
/// * `lua_vm`: Lua VM used by callee
///
/// returns: Option<usize>
pub fn callee_pc(lua_vm: &mut LuaVM) -> Option<usize> {
    lua_vm.stack.last().map(|frame| {
        frame.prev_pc
    })
}

/// When called in a rust LuaFunction, returns the current program line of the Lua function calling it
///
/// # Arguments
///
/// * `lua_vm`: Lua VM used by callee
///
/// returns: Option<usize>
pub fn callee_line(lua_vm: &mut LuaVM) -> Option<usize> {
    lua_vm.stack.last().and_then(|frame| {
        let pc = frame.prev_pc;
        frame.closure.prototype().get_line(pc)
    })
}

/// When called in a rust LuaFunction, returns the name of the Lua function calling it
///
/// # Arguments
///
/// * `lua_vm`: Lua VM used by callee
///
/// returns: Option<usize>
pub fn callee_name(lua_vm: &mut LuaVM) -> Option<LuaString> {
    lua_vm.stack.last().and_then(|frame| {
        frame.closure.prototype().origin_string.clone()
    })
}