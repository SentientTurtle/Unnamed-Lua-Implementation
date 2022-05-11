//! Module for LuaVM

mod fetch;
#[allow(unused)]
pub mod debug;
// pub(crate) mod fetch;

use crate::constants::opcodes;
use std::rc::Rc;
use self::fetch::*;
use std::cmp::Ordering;
use crate::error::{ByteCodeError, CannotCompareError, LuaError, GenericError};
use crate::constants::types::{LUA_INT, LUA_FLOAT, LUA_INT_UNSIGNED, UnpackedInstruction};
use std::collections::HashMap;
use std::io::{Read, Write};
use crate::types::value::table::LuaTable;
use crate::types::value::{LuaValue, TypeMetatables};
use crate::types::value::function::{Prototype, LuaFunction, LuaClosure};
use crate::types::value::number::LuaNumber;
use crate::types::upvalue::Upvalue;
use crate::types::varargs::Varargs;
use crate::types::{LuaType, CoerceFrom};
use std::time::Instant;
use crate::types::value::thread::LuaThread;

/// Lua VM
///
/// This struct contains all the global state variables for the Lua virtual machine
pub struct LuaVM {
    /// Lua call stack
    stack: Vec<StackFrame>,
    /// Metatables for lua types; Where tables and userdata have a metatable per value, other types have a static metatable per type
    pub metatables: TypeMetatables,
    /// Global environment, containing global variables. Generally set as first upvalue in closures
    pub global_env: LuaTable,
    /// Loaded modules
    ///
    /// Dynamically loaded models (See [`crate::stdlib::package::require`]) are added automatically. Modules loaded through rust must manually be added to this map
    pub modules: HashMap<&'static str, LuaTable>,
    /// General purpose store for global variables that are hidden from lua scripts
    ///
    /// See: <https://www.lua.org/manual/5.4/manual.html#4.3>
    pub registry: LuaTable,
    /// Whether or not warnings from Lua scripts are enabled
    ///
    /// See [`crate::stdlib::basic::warn`]
    ///
    /// NOTE: Lua scripts with access to the warn function can also enable this
    pub warnings_enabled: bool,
    /// Log of tracing info, or None if tracing is disabled
    // TODO: Profile, and make cargo feature if needed
    tracing_log: Option<Vec<(Rc<Prototype>, Vec<usize>)>>,
    /// Creation time of this LuaVM
    start_instant: Instant,
    /// Limit on script size that can be loaded
    max_script_size: usize,
    /// Standard input for lua scripts running in this vm
    stdin: Box<dyn Read>,
    /// Standard output for lua scripts running in this vm
    stdout: Box<dyn Write>,
    /// Standard error for lua scripts running in this vm
    stderr: Box<dyn Write>
}

impl LuaVM {
    /// Creates new Lua VM with default settings:
    ///
    /// * Empty global environment
    /// * No type metatables (string type metatable can be loaded from string module)
    /// * No loaded modules
    /// * Empty registry
    /// * Tracing disabled
    /// * Start time set to [`Instant::now`] during this call
    /// * Unlimited max script size
    /// * STDIO inherited from rust ([`std::io::stdin`], [`std::io::stdout`], [`std::io::stderr`])
    pub fn new() -> LuaVM {
        let global_env = LuaTable::empty();
        global_env.raw_set("_G", global_env.clone());
        LuaVM {
            stack: vec![],
            metatables: TypeMetatables {
                boolean: None,
                number: None,
                string: None,
                function: None,
                thread: None,
            },
            global_env,
            modules: HashMap::new(),
            registry: LuaTable::empty(),
            warnings_enabled: false,
            tracing_log: None,
            start_instant: Instant::now(),
            max_script_size: usize::MAX, // TODO: Replace with a more sensible default, or force manual selection
            stdin: Box::new(std::io::stdin()),
            stdout: Box::new(std::io::stdout()),
            stderr: Box::new(std::io::stderr())
        }
    }

    /// Returns maximum script size set for this Lua VM
    pub fn max_script_size(&self) -> usize {
        self.max_script_size
    }

    /// Returns start instant of this Lua VM
    pub fn start_instant(&self) -> Instant {
        self.start_instant
    }

    /// Returns current stdin of Lua VM
    pub fn stdin(&mut self) -> &mut dyn Read {
        &mut *self.stdin
    }

    /// Returns current stdout of Lua VM
    pub fn stdout(&mut self) -> &mut dyn Write {
        &mut *self.stdout
    }

    /// Returns current stderr of Lua VM
    pub fn stderr(&mut self) -> &mut dyn Write {
        &mut *self.stderr
    }

    /// Returns true if warnings are enabled
    pub fn warnings_enabled(&self) -> bool {
        self.warnings_enabled
    }

    /// Sets whether warnings are enabled
    ///
    /// NOTE: Lua scripts with access to the warn function ([`crate::stdlib::basic::warn`]) can also enable this
    pub fn set_warnings(&mut self, enabled: bool) {
        self.warnings_enabled = enabled;
    }

    /// Enables tracing for this Lua VM
    ///
    /// No-op if tracing was already enabled
    pub fn enable_tracing(mut self) -> Self {   // TODO: Move to builder struct
        if let None = self.tracing_log {
            self.tracing_log = Some(Vec::new())
        }
        self
    }

    /// Destroy this LuaVM and return it's stack
    #[cfg(test)]
    pub(crate) fn into_stack(self) -> Vec<StackFrame> {
        self.stack
    }

    /// Returns trace of this Lua VM
    pub fn get_trace(&self) -> &Option<Vec<(Rc<Prototype>, Vec<usize>)>> {
        &self.tracing_log
    }

    /// Closes specified coroutine
    ///
    /// See [`crate::stdlib::coroutine::close`]
    pub(crate) fn close_coroutine(&self, _coroutine: &LuaThread) -> Result<(), LuaError> {
        todo!()
    }

    /// Yields current coroutine, starting another
    ///
    /// See [`crate::stdlib::coroutine::yield`]
    pub(crate) fn yield_current_coroutine(&self, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
        todo!()
    }

    /// Returns current coroutine
    ///
    /// See [`crate::stdlib::coroutine]
    pub(crate) fn current_coroutine(&self) -> LuaThread {
        todo!()
    }

    /// True if specified coroutine can currently be yielded
    ///
    /// See [`crate::stdlib::coroutine::isyieldable`]
    pub(crate) fn is_coroutine_yieldable(&self, _coroutine: &LuaThread) -> bool {
        todo!()
    }

    /// Resumes specified coroutine
    ///
    /// See [`crate::stdlib::coroutine::resume`]
    pub(crate) fn resume_coroutine(&self, _coroutine: &LuaThread, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
        todo!()
    }

    /// Returns true if specified coroutine is "main thread"
    ///
    /// See [`crate::stdlib::coroutine::running`]
    pub(crate) fn is_coroutine_main(&self, _coroutine: &LuaThread) -> bool {
        todo!()
    }
}

/// Returns for a Lua function call
#[derive(Debug, Copy, Clone)]
enum FrameReturn {
    /// Return values out of [`execute_closure`] call
    Out,
    /// Return values upstack
    UpStack {
        from_index: usize,
        amount: usize,
    },
    /// Jump previous stackframe
    Jump {
        /// Condition, tested against first return value
        condition: bool,
        destination: usize
    }
}

/// Single Lua stackframe
///
/// TERMINOLOGY NOTE: This is the callstack, Lua value stacks (<https://www.lua.org/manual/5.4/manual.html#4.1>) are called "registers" [`StackFrame::registers`]
#[derive(Debug)]
pub(crate) struct StackFrame {
    /// Program counter for next instruction
    pub(crate) pc: usize,
    /// Program counter for current instruction, used for stack-tracing
    pub(crate) prev_pc: usize,
    /// True if this stackframe is a tailcall (i.e. callee's stackframe has been dropped)
    is_tailcall: bool,
    /// Closure for this stackframe
    pub(crate) closure: LuaClosure,
    /// How values will be returned by this stackframe
    frame_return: FrameReturn,
    /// Registers of this frame
    registers: Vec<LuaValue>,
    /// "Top" index of registers
    registers_top: usize,   // TODO: Registers-top should always be equivalent to registers::len, verification and bugfixing needed
    /// List of upvalues referencing this frame, these Upvalues will be closed when this stackframe is dropped
    upvalues_referencing_this_frame: Vec<Option<Upvalue>>,
}

impl StackFrame {
    fn new(closure: LuaClosure, register_capacity: usize, is_tailcall: bool, frame_return: FrameReturn) -> StackFrame {
        StackFrame {
            registers: Vec::with_capacity(register_capacity),
            pc: 0,
            prev_pc: 0,
            is_tailcall,
            closure,
            upvalues_referencing_this_frame: Vec::with_capacity(register_capacity),
            frame_return,
            registers_top: register_capacity,
        }
    }
}

impl Drop for StackFrame {
    /// Close upvalues on drop
    fn drop(&mut self) {
        for i in 0..self.upvalues_referencing_this_frame.len() {
            if let Some(upvalue) = &self.upvalues_referencing_this_frame[i] {
                upvalue.close(self.registers.get_mut(i).unwrap().clone())
            }
        }
    }
}

/// Fully executes single LuaClosure
///
/// # Arguments
///
/// * `closure`: Closure to execute
/// * `lua_vm`: Lua VM for closure
/// * `parameters`: Parameters passed to closure call
///
/// returns: Result<Varargs, LuaError>
#[allow(unused)]
pub(crate) fn execute_closure(closure: LuaClosure, lua_vm: &mut LuaVM, parameters: &[LuaValue]) -> Result<Varargs, LuaError> {
    let mut parameter_buffer = Vec::new();  // Buffer to keep parameters for any subcalls
    let mut parameters = parameters;    // Move parameters into a variable to reduce it's lifetime
    let register_capacity = usize::max(closure.prototype().max_stack_size as usize, parameters.len());

    let mut new_frame = StackFrame::new(closure, register_capacity, false, FrameReturn::Out);


    new_frame.registers.extend_from_slice(parameters);
    new_frame.registers.resize_with(register_capacity, Default::default);
    new_frame.upvalues_referencing_this_frame.resize_with(register_capacity, Option::default);

    lua_vm.stack.push(new_frame);
    loop {
        let mut result = execute_top_stackframe(lua_vm, parameters);

        match result {
            CallResult::Ok(values) => {
                match lua_vm.stack.last().unwrap().frame_return {
                    FrameReturn::Out => {
                        lua_vm.stack.truncate(lua_vm.stack.len() - 1);
                        return Ok(values);
                    }
                    FrameReturn::UpStack { from_index, amount } => {
                        lua_vm.stack.truncate(lua_vm.stack.len() - 1);
                        if amount > 0 {
                            let current_frame = lua_vm.stack.last_mut().unwrap();

                            let result_count = if amount == usize::MAX {
                                values.count()
                            } else {
                                amount
                            };

                            if from_index + result_count > current_frame.registers.len() {
                                current_frame.registers.resize_with(from_index + result_count, Default::default);
                                current_frame.upvalues_referencing_this_frame.resize_with(from_index + result_count, Default::default);
                            }

                            for i in from_index..from_index + result_count {
                                set_reg(&mut current_frame.registers, i, values.n(i - from_index).clone())?;
                            }
                            current_frame.registers_top = from_index + result_count;
                        }
                    }
                    FrameReturn::Jump { condition, destination } => {
                        lua_vm.stack.truncate(lua_vm.stack.len() - 1);
                        if values.into_first().is_truthy() == condition {
                            let current_frame = lua_vm.stack.last_mut().unwrap();
                            current_frame.pc = destination;
                        }
                    }
                }
            }
            CallResult::NewCall { function, parameters: new_parameters, frame_return } => {
                match function {
                    LuaFunction::LUA_CLOSURE(new_closure) => {
                        let register_capacity = usize::max(new_closure.prototype().max_stack_size as usize, new_parameters.len());

                        let mut new_frame = StackFrame::new(
                            new_closure,
                            register_capacity,
                            false,
                            frame_return,
                        );
                        new_frame.registers.extend_from_slice(&new_parameters[..]);
                        new_frame.registers.resize_with(register_capacity, Default::default);
                        new_frame.upvalues_referencing_this_frame.resize_with(register_capacity, Default::default);

                        parameter_buffer = new_parameters;
                        parameters = &parameter_buffer[..];

                        lua_vm.stack.push(new_frame);
                        continue;
                    }
                    function @ LuaFunction::RUST_FUNCTION(_) | function @ LuaFunction::RUST_CLOSURE(_) => {
                        match function.call(lua_vm, &new_parameters) {
                            Ok(values) => {
                                match frame_return {
                                    FrameReturn::Out => {
                                        return Ok(values);
                                    }
                                    FrameReturn::UpStack { from_index, amount } => {
                                        if amount > 0 {
                                            let current_frame = lua_vm.stack.last_mut().unwrap();

                                            let result_count = if amount == usize::MAX {
                                                values.count()
                                            } else {
                                                amount
                                            };

                                            if from_index + result_count > current_frame.registers.len() {
                                                current_frame.registers.resize_with(from_index + result_count, Default::default);
                                                current_frame.upvalues_referencing_this_frame.resize_with(from_index + result_count, Default::default);
                                            }

                                            for i in from_index..from_index + result_count {
                                                set_reg(&mut current_frame.registers, i, values.n(i - from_index).clone())?;
                                            }
                                            current_frame.registers_top = from_index + result_count;
                                        }
                                        continue;
                                    }
                                    FrameReturn::Jump { condition, destination } => {
                                        if values.into_first().is_truthy() == condition {
                                            let current_frame = lua_vm.stack.last_mut().unwrap();
                                            current_frame.pc = destination;
                                        }
                                    }
                                }
                            }
                            Err(mut err) => {
                                let mut new_len = lua_vm.stack.len() - 1;
                                while let FrameReturn::UpStack { .. } = lua_vm.stack[new_len].frame_return {   // Truncate stack up to and including the frame that returns out of this method
                                    new_len -= 1;
                                }
                                for frame in lua_vm.stack[new_len..lua_vm.stack.len()].iter().rev() {
                                    err = err.trace_lua(frame.prev_pc, frame.closure.clone_prototype());
                                    if frame.is_tailcall {
                                        err = err.trace_tail_call()
                                    }
                                }
                                lua_vm.stack.truncate(new_len);

                                return Err(err);
                            }
                        }
                    }
                };
            }
            CallResult::TailCall { function, parameters: new_parameters } => {
                match function {
                    LuaFunction::LUA_CLOSURE(new_closure) => {
                        let current_frame = lua_vm.stack.last_mut().unwrap();

                        let register_capacity = usize::max(new_closure.prototype().max_stack_size as usize, new_parameters.len());

                        let mut new_frame = StackFrame::new(new_closure, register_capacity, true, current_frame.frame_return);

                        new_frame.registers.extend_from_slice(&new_parameters[..]);
                        new_frame.registers.resize_with(register_capacity, Default::default);
                        new_frame.upvalues_referencing_this_frame.resize_with(register_capacity, Default::default);

                        parameter_buffer = new_parameters;
                        parameters = &parameter_buffer[..];

                        std::mem::replace(current_frame, new_frame);
                        continue;
                    }
                    function @ LuaFunction::RUST_FUNCTION(_) | function @ LuaFunction::RUST_CLOSURE(_) => {
                        match function.call(lua_vm, &new_parameters) {
                            Ok(values) => {
                                match lua_vm.stack.last().unwrap().frame_return {
                                    FrameReturn::Out => {
                                        lua_vm.stack.truncate(lua_vm.stack.len() - 1);
                                        return Ok(values);
                                    }
                                    FrameReturn::UpStack { from_index: return_register, amount: return_value_amount } => {
                                        lua_vm.stack.truncate(lua_vm.stack.len() - 1);
                                        if return_value_amount > 0 {
                                            let current_frame = lua_vm.stack.last_mut().unwrap();

                                            let result_count = if return_value_amount == usize::MAX {
                                                values.count()
                                            } else {
                                                return_value_amount
                                            };

                                            if return_register + result_count > current_frame.registers.len() {
                                                current_frame.registers.resize_with(return_register + result_count, Default::default);
                                                current_frame.upvalues_referencing_this_frame.resize_with(return_register + result_count, Default::default);
                                            }

                                            for i in return_register..return_register + result_count {
                                                set_reg(&mut current_frame.registers, i, values.n(i - return_register).clone())?;
                                            }
                                            current_frame.registers_top = return_register + result_count;
                                        }
                                        continue;
                                    }
                                    FrameReturn::Jump { condition, destination } => {
                                        if values.into_first().is_truthy() == condition {
                                            let current_frame = lua_vm.stack.last_mut().unwrap();
                                            current_frame.pc = destination;
                                        }
                                    }
                                }
                            }
                            Err(mut err) => {
                                let mut new_len = lua_vm.stack.len() - 1;
                                while let FrameReturn::UpStack { .. } = lua_vm.stack[new_len].frame_return {   // Truncate stack up to and including the frame that returns out of this method
                                    new_len -= 1;
                                }
                                for frame in lua_vm.stack[new_len..lua_vm.stack.len()].iter().rev() {
                                    err = err.trace_lua(frame.prev_pc, frame.closure.clone_prototype());
                                    if frame.is_tailcall {
                                        err = err.trace_tail_call()
                                    }
                                }
                                lua_vm.stack.truncate(new_len);

                                return Err(err);
                            }
                        }
                    }
                }
            }
            CallResult::Err(mut err) => {
                debug_assert!(lua_vm.stack.len() >= 1);
                // We know the first entry in the stack always has FrameReturn::Out so direct indexing on the stack is safe.
                let mut new_len = lua_vm.stack.len() - 1;  // -1; We always remove the stackframe that just threw this error
                while let FrameReturn::UpStack { .. } = lua_vm.stack[new_len].frame_return {   // Truncate stack up to and including the frame that returns out of this method
                    new_len -= 1;
                }
                for frame in lua_vm.stack[new_len..lua_vm.stack.len()].iter().rev() {
                    err = err.trace_lua(frame.prev_pc, frame.closure.clone_prototype());
                    if frame.is_tailcall {
                        err = err.trace_tail_call()
                    }
                }
                lua_vm.stack.truncate(new_len);

                return Err(err);
            }
        }
    }
}

/// Result from a Lua closure call (See [`execute_top_stackframe`]
enum CallResult {
    /// Call success, returned values
    Ok(Varargs),
    /// Call failure, raised error
    Err(LuaError),
    /// Start new call, retaining current call
    NewCall { function: LuaFunction, parameters: Vec<LuaValue>, frame_return: FrameReturn },
    /// Start tailcall, completing current call
    TailCall { function: LuaFunction, parameters: Vec<LuaValue> },
}

macro_rules! math_binary_op {
    (REG $op:tt, $metamethod:expr, $error:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $luaVM:expr, $frame:ident, $registers:ident, $pc:ident, $proto:ident) => {{
        // let registers = &mut $luaVM.stack.last_mut().unwrap().registers;
        let lhs = get_reg($registers, $b)?.clone();
        let rhs = get_reg($registers, $c)?.clone();
        match (
            lhs.index_metatable($metamethod, &$luaVM.metatables).not_nil(),
            rhs.index_metatable($metamethod, &$luaVM.metatables).not_nil()
        ) {
            (None, None) => {
                set_reg($registers, $a, LuaValue::from((&lhs $op &rhs).map_err(|_| {
                    if let LuaValue::NUMBER(_) = lhs {
                        GenericError::String(format!($error, lhs.type_name()))
                    } else {
                        GenericError::String(format!($error, rhs.type_name()))
                    }
                })?))?;
            }
            (Some(func), _) | (_, Some(func)) => {
                let (function, mut parameter_prefix) = func.prep_call_with_metatable(&$luaVM.metatables)?;
                parameter_prefix.extend([lhs, rhs]);
                return CallResult::NewCall {
                    function,
                    parameters: parameter_prefix,
                    frame_return: FrameReturn::UpStack { from_index: $a, amount: 1 }
                };
            }
        }
        Ok(())
    }};
    (REG func: $op:ident, $metamethod:expr, $error:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $luaVM:expr, $frame:ident, $registers:ident, $pc:ident, $proto:ident) => {{
        // let registers = &mut $luaVM.stack.last_mut().unwrap().registers;
        let lhs = get_reg($registers, $b)?.clone();
        let rhs = get_reg($registers, $c)?.clone();

        match (
            lhs.index_metatable($metamethod, &$luaVM.metatables).not_nil(),
            rhs.index_metatable($metamethod, &$luaVM.metatables).not_nil()
        ) {
            (None, None) => {
                set_reg($registers, $a, LuaValue::from(lhs.$op(&rhs).map_err(|_| {
                    if let LuaValue::NUMBER(_) = lhs {
                        GenericError::String(format!($error, lhs.type_name()))
                    } else {
                        GenericError::String(format!($error, rhs.type_name()))
                    }
                })?))?;
            }
            (Some(func), _) | (_, Some(func)) => {
                let (function, mut parameter_prefix) = func.prep_call_with_metatable(&$luaVM.metatables)?;
                parameter_prefix.extend([lhs, rhs]);
                return CallResult::NewCall {
                    function,
                    parameters: parameter_prefix,
                    frame_return: FrameReturn::UpStack { from_index: $a, amount: 1 }
                };
            }
        }
        Ok(())
    }};
    (CONST $op:tt, $metamethod:expr, $error:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $luaVM:expr, $frame:ident, $registers:ident, $pc:ident, $proto:ident) => {{
        // let registers = &mut $luaVM.stack.last_mut().unwrap().registers;
        let lhs = get_reg($registers, $b)?.clone();
        let rhs = get_const($proto, $c)?.clone();
        match (
            lhs.index_metatable($metamethod, &$luaVM.metatables).not_nil(),
            rhs.index_metatable($metamethod, &$luaVM.metatables).not_nil()
        ) {
            (None, None) => {
                set_reg($registers, $a, LuaValue::from((&lhs $op &rhs).map_err(|_| {
                    if let LuaValue::NUMBER(_) = lhs {
                        GenericError::String(format!($error, lhs.type_name()))
                    } else {
                        GenericError::String(format!($error, rhs.type_name()))
                    }
                })?))?;
            }
            (Some(func), _) | (_, Some(func)) => {
                let (function, mut parameter_prefix) = func.prep_call_with_metatable(&$luaVM.metatables)?;
                parameter_prefix.extend([lhs, rhs]);
                return CallResult::NewCall {
                    function,
                    parameters: parameter_prefix,
                    frame_return: FrameReturn::UpStack { from_index: $a, amount: 1 }
                };
            }
        }
        Ok(())
    }};
    (CONST func: $op:ident, $metamethod:expr, $error:expr, $a:expr, $b:expr, $c:expr, $closure:expr, $luaVM:expr, $frame:ident, $registers:ident, $pc:ident, $proto:ident) => {{
        // let registers = &mut $luaVM.stack.last_mut().unwrap().registers;
        let lhs = get_reg($registers, $b)?.clone();
        let rhs = get_const($proto, $c)?.clone();

        match (
            lhs.index_metatable($metamethod, &$luaVM.metatables).not_nil(),
            rhs.index_metatable($metamethod, &$luaVM.metatables).not_nil()
        ) {
            (None, None) => {
                set_reg($registers, $a, LuaValue::from(lhs.$op(&rhs).map_err(|_| {
                    if let LuaValue::NUMBER(_) = lhs {
                        GenericError::String(format!($error, lhs.type_name()))
                    } else {
                        GenericError::String(format!($error, rhs.type_name()))
                    }
                })?))?;
            }
            (Some(func), _) | (_, Some(func)) => {
                let (function, mut parameter_prefix) = func.prep_call_with_metatable(&$luaVM.metatables)?;
                parameter_prefix.extend([lhs, rhs]);
                return CallResult::NewCall {
                    function,
                    parameters: parameter_prefix,
                    frame_return: FrameReturn::UpStack { from_index: $a, amount: 1 }
                };
            }
        }
        Ok(())
    }};
}


macro_rules! math_unary_op {
    ($op:tt, $metamethod:expr, $error:expr, $a:expr, $b:expr, $closure:expr, $luaVM:expr, $frame:ident, $registers:ident, $pc:ident, $proto:ident) => {{
        // let registers = &mut $luaVM.stack.last_mut().unwrap().registers;
        let lhs = get_reg($registers, $b)?.clone();

        match lhs.index_metatable($metamethod, &$luaVM.metatables).not_nil() {
            None => {
                set_reg($registers, $a, LuaValue::from(($op &lhs).map_err(|_| GenericError::String(format!($error, lhs.type_name())))?))?;
            }
            Some(func) => {
                let (function, mut parameter_prefix) = func.prep_call_with_metatable(&$luaVM.metatables)?;
                parameter_prefix.push(lhs);
                return CallResult::NewCall {
                    function,
                    parameters: parameter_prefix,
                    frame_return: FrameReturn::UpStack { from_index: $a, amount: 1 }
                };
            }
        }
        Ok(())
    }};
    (func: $op:ident, $metamethod:expr, $error:expr, $a:expr, $b:expr, $closure:expr, $luaVM:expr, $frame:ident, $registers:ident, $pc:ident, $proto:ident) => {{
        // let registers = &mut $luaVM.stack.last_mut().unwrap().registers;
        let lhs = get_reg($registers, $b)?.clone();

        match lhs.index_metatable($metamethod, &$luaVM.metatables).not_nil() {
            None => {
                set_reg($registers, $a, LuaValue::from(lhs.$op().map_err(|_| GenericError::String(format!($error, lhs.type_name())))?))?;
            }
            Some(func) => {
                let (function, mut parameter_prefix) = func.prep_call_with_metatable(&$luaVM.metatables)?;
                parameter_prefix.push(lhs);
                return CallResult::NewCall {
                    function,
                    parameters: parameter_prefix,
                    frame_return: FrameReturn::UpStack { from_index: $a, amount: 1 }
                };
            }
        }
        Ok(())
    }};
}

/// Execute closure at top of stack in specified LuaVM
///
/// Separate function from [`execute_closure`] for scoping; Ensures refcell borrows do not outlive the call for which they are needed
///
/// # Arguments
///
/// * `lua_vm`: LuaVM whose stack to call
/// * `parameters`: Parameters for this call
///
/// returns: CallResult
//noinspection DuplicatedCode
// Inner function to allow Try usage; Outer function is execute_closure and handles stack push/pop
fn execute_top_stackframe(lua_vm: &mut LuaVM, parameters: &[LuaValue]) -> CallResult {
    debug_assert!(lua_vm.stack.len() > 0); // Stack must contain at least one frame
    let frame_index = lua_vm.stack.len() - 1;
    let (frame, substack) = lua_vm.stack.split_last_mut().unwrap();
    let closure = &frame.closure;
    let proto_rc = closure.clone_prototype();
    let mut upvalues = closure.upvalues_mut();

    loop {
        // let frame = luaVM.stack.last_mut().unwrap();
        let registers = &mut frame.registers;
        let top = &mut frame.registers_top;
        let pc = &mut frame.pc;
        frame.prev_pc = *pc;    // The program counter is mutated immediately by instruction-fetch/jumps/calls, we keep a copy of the "current" VM cycle's program counter for building the stacktrace

        debug_assert_eq!(frame.upvalues_referencing_this_frame.len(), registers.len());

        let result: Result<(), LuaError> = try {
            if let Some(tracing) = &mut lua_vm.tracing_log {
                match tracing.last_mut() {
                    Some((last_proto, vec)) if Rc::ptr_eq(last_proto, &proto_rc) => vec.push(*pc),
                    _ => tracing.push((closure.clone_prototype(), vec![*pc]))
                }
            }
            let proto: &Prototype = closure.prototype();

            let instruction = next_op(proto, pc)?;

            let UnpackedInstruction {
                opcode,
                a,
                b,
                sb,
                c,
                sc,
                bx,
                sbx,
                ax: _ax,
                sj,
                k
            } = instruction.unpack();

            match opcode {
                opcodes::MOVE => {
                    let value = get_reg_mut(registers, b)?.clone();
                    set_reg(registers, a, value)
                }
                opcodes::LOADI => set_reg(registers, a, sbx as LUA_INT),
                opcodes::LOADF => set_reg(registers, a, sbx as LUA_FLOAT),
                opcodes::LOADK => {
                    let val = get_const(proto, bx)?.clone();
                    set_reg(registers, a, val)
                }
                opcodes::LOADKX => {
                    let extra_arg_op = next_op(proto, pc)?;
                    if extra_arg_op.opcode() == opcodes::EXTRAARG {
                        set_reg(registers, a, get_const(proto, extra_arg_op.unpack().ax)?.clone())
                    } else {
                        Err(ByteCodeError::ExpectedOpcode { expected: opcodes::EXTRAARG, found: opcode })
                    }
                }
                opcodes::LOADFALSE => set_reg(registers, a, false),
                opcodes::LOADFALSESKIP => {
                    set_reg(registers, a, false)?;
                    *pc += 1;
                    Ok(())
                }
                opcodes::LOADTRUE => set_reg(registers, a, true),
                opcodes::LOADNIL => {
                    for index in a..=a + b {
                        set_reg(registers, index, LuaValue::NIL)?;
                    }
                    Ok(())
                }
                opcodes::GETUPVAL => {
                    let upval = get_upvalue(&*upvalues, b, substack)?;
                    set_reg(registers, a, upval)
                }
                opcodes::SETUPVAL => {
                    let value = get_reg(registers, a)?.clone();
                    set_upvalue(&mut *upvalues, b, substack, value).map(|old| drop(old))
                }
                opcodes::GETTABUP => {
                    let upvalue = get_upvalue(&*upvalues, b, substack)?;
                    set_reg(registers, a, upvalue.index(get_const(proto, c)?, &lua_vm.metatables)?.clone())
                }
                opcodes::GETTABLE => set_reg(registers, a, get_reg(registers, b)?.index(get_reg(registers, c)?, &lua_vm.metatables)?.clone()),
                opcodes::GETI => set_reg(registers, a, get_reg(registers, b)?.index(&LuaValue::from(c as LUA_INT), &lua_vm.metatables)?.clone()),
                opcodes::GETFIELD => set_reg(registers, a, get_reg(registers, b)?.index(get_const(proto, c)?, &lua_vm.metatables)?.clone()),
                opcodes::SETTABUP => {
                    let upvalue = get_upvalue(&*upvalues, a, substack)?;
                    let table = LuaTable::coerce_from(&upvalue)?;
                    let key = get_const(proto, b)?.clone();
                    let value = get_rk(proto, registers, k, c)?.clone();

                    if let Some(function) = table.index_metatable("__newindex").not_nil() {
                        let (function, mut parameter_prefix) = function.prep_call_with_metatable(&lua_vm.metatables)?;
                        parameter_prefix.push(upvalue);
                        parameter_prefix.push(value);
                        return CallResult::NewCall {
                            function,
                            parameters: parameter_prefix,
                            frame_return: FrameReturn::UpStack { from_index: 0, amount: 0 }
                        }
                    } else {
                        table.raw_set(key.try_key()?, value);
                    }
                    Ok(())
                }
                opcodes::SETTABLE => {
                    let key = get_reg(registers, b)?.clone();
                    let value = get_rk(proto, registers, k, c)?.clone();
                    let table = LuaTable::coerce_from(get_reg_mut(registers, a)?)?;
                    if let Some(function) = table.index_metatable("__newindex").not_nil() {
                        let (function, mut parameter_prefix) = function.prep_call_with_metatable(&lua_vm.metatables)?;
                        parameter_prefix.extend([table.into(), key, value]);
                        return CallResult::NewCall {
                            function,
                            parameters: parameter_prefix,
                            frame_return: FrameReturn::UpStack { from_index: 0, amount: 0 }
                        };
                    } else {
                        table.raw_set(key.try_key()?, value);
                    }
                    Ok(())
                }
                opcodes::SETI => {
                    let key = LuaValue::from(b as LUA_INT);
                    let value = get_rk(proto, registers, k, c)?.clone();
                    let table = LuaTable::coerce_from(get_reg_mut(registers, a)?)?;
                    if let Some(function) = table.index_metatable("__newindex").not_nil() {
                        let (function, mut parameter_prefix) = function.prep_call_with_metatable(&lua_vm.metatables)?;
                        parameter_prefix.extend([table.into(), key, value]);
                        return CallResult::NewCall {
                            function,
                            parameters: parameter_prefix,
                            frame_return: FrameReturn::UpStack { from_index: 0, amount: 0 }
                        };
                    } else {
                        table.raw_set(key.try_key()?, value);
                    }
                    Ok(())
                }
                opcodes::SETFIELD => {
                    let key = get_const(proto, b)?.clone();
                    let value = get_rk(proto, registers, k, c)?.clone();
                    let table = LuaTable::coerce_from(get_reg_mut(registers, a)?)?;
                    if let Some(function) = table.index_metatable("__newindex").not_nil() {
                        let (function, mut parameter_prefix) = function.prep_call_with_metatable(&lua_vm.metatables)?;
                        parameter_prefix.extend([table.into(), key, value]);
                        return CallResult::NewCall {
                            function,
                            parameters: parameter_prefix,
                            frame_return: FrameReturn::UpStack { from_index: 0, amount: 0 }
                        };
                    } else {
                        table.raw_set(key.try_key()?, value);
                    }
                    Ok(())
                }
                opcodes::NEWTABLE => {
                    let extra_arg_op = next_op(proto, pc)?;
                    let array_capacity = if extra_arg_op.opcode() == opcodes::EXTRAARG {
                        if k != 0 {
                            (extra_arg_op.unpack().ax << 8) | c
                        } else {
                            c
                        }
                    } else {
                        Err(ByteCodeError::ExpectedOpcode { expected: opcodes::EXTRAARG, found: opcode })?
                    };
                    let hash_capacity = if b != 0 { 2usize.pow(b as u32) + 1 } else { 0 };

                    set_reg(registers, a, LuaTable::with_capacity(array_capacity, hash_capacity))?;
                    Ok(())
                }
                opcodes::SELF => {
                    let table = get_reg(registers, b)?.clone();
                    set_reg(registers, a, table.index(get_rk(proto, registers, k, c)?, &lua_vm.metatables)?)?;
                    set_reg(registers, a + 1, table)
                }
                opcodes::ADDI => {
                    let value = get_reg(registers, b)?;
                    let addition = (value + &LuaValue::from(sc as LUA_INT))
                        .map_err(|_| GenericError::String(format!("attempt to perform arithmetic on a {} value", value.type_name())));
                    set_reg(registers, a, addition?)
                }
                opcodes::ADDK => math_binary_op!(CONST +, "__add", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::SUBK => math_binary_op!(CONST -, "__sub", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::MULK => math_binary_op!(CONST *, "__mul", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::MODK => math_binary_op!(CONST %, "__mod", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::POWK => math_binary_op!(CONST func: pow, "__pow", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::DIVK => math_binary_op!(CONST /, "__div", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::IDIVK => math_binary_op!(CONST func: idiv, "__idiv", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),

                opcodes::BANDK => math_binary_op!(CONST &, "__band", "attempt to perform bitwise operation on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::BORK => math_binary_op!(CONST |, "__bor", "attempt to perform bitwise operation on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::BXORK => math_binary_op!(CONST ^, "__bxor", "attempt to perform bitwise operation on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),

                opcodes::SHLI => {
                    let lhs = LuaValue::from((c as isize - ((2isize.pow(8) / 2) - 1)) as LUA_INT);
                    let rhs = get_reg(registers, b)?.clone();
                    match rhs.index_metatable("__shl", &lua_vm.metatables).not_nil() {
                        None => {
                            set_reg(registers, a, LuaValue::from((&lhs << &rhs).map_err(|_| {
                                if let LuaValue::NUMBER(_) = lhs {
                                    GenericError::String(format!("attempt to perform arithmetic on a {} value", lhs.type_name()))
                                } else {
                                    GenericError::String(format!("attempt to perform arithmetic on a {} value", rhs.type_name()))
                                }
                            })?))?;
                        }
                        Some(func) => {
                            let (function, mut parameter_prefix) = func.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.push(lhs);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::UpStack { from_index: a, amount: 1 }
                            };
                        }
                    }
                    Ok(())
                }
                opcodes::SHRI => {
                    let lhs = get_reg(registers, b)?.clone();
                    let rhs = LuaValue::from((c as isize - ((2isize.pow(8) / 2) - 1)) as LUA_INT);
                    match rhs.index_metatable("__shr", &lua_vm.metatables).not_nil() {
                        None => {
                            set_reg(registers, a, LuaValue::from((&lhs >> &rhs).map_err(|_| {
                                if let LuaValue::NUMBER(_) = lhs {
                                    GenericError::String(format!("attempt to perform arithmetic on a {} value", lhs.type_name()))
                                } else {
                                    GenericError::String(format!("attempt to perform arithmetic on a {} value", rhs.type_name()))
                                }
                            })?))?;
                        }
                        Some(func) => {
                            let (function, mut parameter_prefix) = func.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.extend([lhs, rhs]);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::UpStack { from_index: a, amount: 1 }
                            };
                        }
                    }
                    Ok(())
                }

                opcodes::ADD => math_binary_op!(REG +, "__add", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::SUB => math_binary_op!(REG -, "__sub", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::MUL => math_binary_op!(REG *, "__mul", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::MOD => math_binary_op!(REG %, "__mod", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::POW => math_binary_op!(REG func: pow, "__pow", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::DIV => math_binary_op!(REG /, "__div", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::IDIV => math_binary_op!(REG func: idiv, "__idiv", "attempt to perform arithmetic on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),

                opcodes::BAND => math_binary_op!(REG &, "__band", "attempt to perform bitwise operation on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::BOR => math_binary_op!(REG |, "__bor", "attempt to perform bitwise operation on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),
                opcodes::BXOR => math_binary_op!(REG ^, "__bxor", "attempt to perform bitwise operation on a {} value", a, b, c, closure, lua_vm, frame, registers, pc, proto),

                opcodes::SHL => {
                    let lhs = get_reg(registers, b)?.clone();
                    let rhs = get_reg(registers, c)?.clone();
                    match (
                        lhs.index_metatable("__shl", &lua_vm.metatables).not_nil(),
                        rhs.index_metatable("__shl", &lua_vm.metatables).not_nil()
                    ) {
                        (None, None) => {
                            set_reg(registers, a, LuaValue::from((&lhs << &rhs).map_err(|_| {
                                if let LuaValue::NUMBER(_) = lhs {
                                    GenericError::String(format!("attempt to perform arithmetic on a {} value", lhs.type_name()))
                                } else {
                                    GenericError::String(format!("attempt to perform arithmetic on a {} value", rhs.type_name()))
                                }
                            })?))?;
                        }
                        (Some(func), _) | (_, Some(func)) => {
                            let (function, mut parameter_prefix) = func.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.extend([lhs, rhs]);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::UpStack { from_index: a, amount: 1 }
                            };
                        }
                    }
                    Ok(())
                }
                opcodes::SHR => {
                    let lhs = get_reg(registers, b)?.clone();
                    let rhs = get_reg(registers, c)?.clone();
                    match (
                        lhs.index_metatable("__shr", &lua_vm.metatables).not_nil(),
                        rhs.index_metatable("__shr", &lua_vm.metatables).not_nil()
                    ) {
                        (None, None) => {
                            set_reg(registers, a, LuaValue::from((&lhs >> &rhs).map_err(|_| {
                                if let LuaValue::NUMBER(_) = lhs {
                                    GenericError::String(format!("attempt to perform arithmetic on a {} value", lhs.type_name()))
                                } else {
                                    GenericError::String(format!("attempt to perform arithmetic on a {} value", rhs.type_name()))
                                }
                            })?))?;
                        }
                        (Some(func), _) | (_, Some(func)) => {
                            let (function, mut parameter_prefix) = func.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.extend([lhs, rhs]);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::UpStack { from_index: a, amount: 1 }
                            };
                        }
                    }
                    Ok(())
                }

                opcodes::MMBIN => {
                    Ok(())
                }
                opcodes::MMBINI => {
                    Ok(())
                }
                opcodes::MMBINK => {
                    Ok(())
                }

                opcodes::UNM => math_unary_op!(-, "__unm", "attempt to perform arithmetic on a {} value", a, b, closure, lua_vm, frame, registers, pc, proto),
                opcodes::BNOT => math_unary_op!(func: bnot, "__bnot", "attempt to perform arithmetic on a {} value", a, b, closure, lua_vm, frame, registers, pc, proto),
                opcodes::NOT => set_reg(registers, a, (!get_reg(registers, b)?)?),
                opcodes::LEN => math_unary_op!(func: len, "__len", "attempt to perform arithmetic on a {} value", a, b, closure, lua_vm, frame, registers, pc, proto),

                opcodes::CONCAT => {
                    let mut buffer = Vec::with_capacity((a + b).saturating_sub(a + 1));
                    for index in a..=(a + b).saturating_sub(1) {
                        buffer.push(get_reg(registers, index)?);
                    }
                    let value = LuaValue::concat(&buffer[..])?;
                    set_reg(registers, a, value)
                }

                opcodes::CLOSE => {
                    Ok(())  // Close is handled by Drop of the stackframe
                }
                opcodes::TBC => {
                    Err(LuaError::new("Opcode not implemented!"))?
                }

                opcodes::JMP => {
                    *pc = (*pc as isize + sj) as usize;
                    Ok(())
                }
                opcodes::EQ => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = get_reg(registers, b)?.clone();
                    match (
                        lhs.index_metatable("__eq", &lua_vm.metatables).not_nil(),
                        rhs.index_metatable("__eq", &lua_vm.metatables).not_nil()
                    ) {
                        (Some(lhs_metamethod), Some(rhs_metamethod)) if lhs_metamethod == rhs_metamethod => {   // If both have the same metamethod, call that
                            let (function, mut parameter_prefix) = lhs_metamethod.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.extend([lhs, rhs]);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::Jump { condition: k != 0, destination: *pc + 1 }
                            };
                        }
                        _ => {  // else, compare values for equality based on cmp::Eq implementation
                            if (lhs == rhs) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::LT => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = get_reg(registers, b)?.clone();
                    match (
                        lhs.index_metatable("__lt", &lua_vm.metatables).not_nil(),
                        rhs.index_metatable("__lt", &lua_vm.metatables).not_nil()
                    ) {
                        (Some(lhs_metamethod), Some(rhs_metamethod)) if lhs_metamethod == rhs_metamethod => {
                            let (function, mut parameter_prefix) = lhs_metamethod.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.extend([lhs, rhs]);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::Jump { condition: k != 0, destination: *pc + 1 }
                            };
                        }
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(CannotCompareError { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? == Ordering::Less) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::LE => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = get_reg(registers, b)?.clone();
                    match (
                        lhs.index_metatable("__le", &lua_vm.metatables).not_nil(),
                        rhs.index_metatable("__le", &lua_vm.metatables).not_nil()
                    ) {
                        (Some(lhs_metamethod), Some(rhs_metamethod)) if lhs_metamethod == rhs_metamethod => {
                            let (function, mut parameter_prefix) = lhs_metamethod.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.extend([lhs, rhs]);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::Jump { condition: k != 0, destination: *pc + 1 }
                            };
                        }
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(CannotCompareError { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? != Ordering::Greater) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::EQK => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = get_const(proto, b)?.clone();
                    match (
                        lhs.index_metatable("__eq", &lua_vm.metatables).not_nil(),
                        rhs.index_metatable("__eq", &lua_vm.metatables).not_nil()
                    ) {
                        (Some(lhs_metamethod), Some(rhs_metamethod)) if lhs_metamethod == rhs_metamethod => {   // If both have the same metamethod, call that
                            let (function, mut parameter_prefix) = lhs_metamethod.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.extend([lhs, rhs]);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::Jump { condition: k != 0, destination: *pc + 1 }
                            };
                        }
                        _ => {  // else, compare values for equality based on cmp::Eq implementation
                            if (lhs == rhs) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::EQI => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = LuaValue::from(sb as LUA_INT);
                    match lhs.index_metatable("__eq", &lua_vm.metatables).not_nil() {
                        Some(metamethod) => {   // If metamethod, call that
                            let (function, mut parameter_prefix) = metamethod.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.extend([lhs, rhs]);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::Jump { condition: k != 0, destination: *pc + 1 }
                            };
                        }
                        _ => {  // else, compare values for equality based on cmp::Eq implementation
                            if (lhs == rhs) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::LTI => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = LuaValue::from(sb as LUA_INT);
                    match lhs.index_metatable("__lt", &lua_vm.metatables).not_nil() {
                        Some(metamethod) => {
                            let (function, mut parameter_prefix) = metamethod.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.extend([lhs, rhs]);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::Jump { condition: k != 0, destination: *pc + 1 }
                            };
                        }
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(CannotCompareError { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? == Ordering::Less) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::LEI => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = LuaValue::from(sb as LUA_INT);
                    match lhs.index_metatable("__le", &lua_vm.metatables).not_nil() {
                        Some(metamethod) => {
                            let (function, mut parameter_prefix) = metamethod.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.extend([lhs, rhs]);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::Jump { condition: k != 0, destination: *pc + 1 }
                            };
                        }
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(CannotCompareError { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? != Ordering::Greater) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::GTI => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = LuaValue::from(sb as LUA_INT);
                    match lhs.index_metatable("__le", &lua_vm.metatables).not_nil() {
                        Some(metamethod) => {
                            let (function, mut parameter_prefix) = metamethod.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.extend([lhs, rhs]);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::Jump { condition: k != 0, destination: *pc + 1 }
                            };
                        }
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(CannotCompareError { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? == Ordering::Greater) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::GEI => {
                    let lhs = get_reg(registers, a)?.clone();
                    let rhs = LuaValue::from(sb as LUA_INT);
                    match lhs.index_metatable("__lt", &lua_vm.metatables).not_nil(){
                        Some(metamethod) => {
                            let (function, mut parameter_prefix) = metamethod.prep_call_with_metatable(&lua_vm.metatables)?;
                            parameter_prefix.extend([lhs, rhs]);
                            return CallResult::NewCall {
                                function,
                                parameters: parameter_prefix,
                                frame_return: FrameReturn::Jump { condition: k != 0, destination: *pc + 1 }
                            };
                        }
                        _ => {
                            if (lhs.partial_cmp(&rhs).ok_or(CannotCompareError { lhs_type: lhs.type_name(), rhs_type: rhs.type_name() })? != Ordering::Less) != (k != 0) {
                                *pc = *pc + 1
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::TEST => {
                    let condition = bool::coerce_from(get_reg(registers, a)?)?;
                    if (if condition { 1 } else { 0 }) != k {
                        *pc = *pc + 1
                    }
                    Ok(())
                }
                opcodes::TESTSET => {
                    let condition = bool::coerce_from(get_reg(registers, b)?)?;
                    if if condition { 1 } else { 0 } != k {
                        *pc = *pc + 1;
                    } else {
                        set_reg(registers, a, get_reg(registers, b)?.clone())?;
                    }
                    Ok(())
                }
                opcodes::CALL => {
                    let (function, param_prefix) = get_reg(registers, a)?
                        .clone()
                        .prep_call_with_metatable(&lua_vm.metatables)?;

                    let param_range = if b == 0 {
                        a + 1..*top
                    } else {
                        a + 1..a + b
                    };

                    let param_count = if param_range.start + 1 >= param_range.end {
                        0
                    } else {
                        param_range.end - param_range.start - 1
                    };

                    let mut new_parameters = Vec::with_capacity(param_count);    // TODO: Verify that this doesn't lead to oversized allocations
                    if param_prefix.len() > 0 {
                        new_parameters.extend(param_prefix);
                    }
                    for i in param_range {
                        new_parameters.push(get_reg(registers, i)?.clone());
                    }

                    return CallResult::NewCall {
                        function,
                        parameters: new_parameters,
                        frame_return: FrameReturn::UpStack { from_index: a, amount: c.wrapping_sub(1) } // Intentional wrapping to max_value if 0
                    };
                }
                opcodes::TAILCALL => {
                    let function_value = get_reg(registers, a)?;
                    let (function, param_prefix) = function_value.clone().prep_call_with_metatable(&lua_vm.metatables)?;

                    let param_range = if b == 0 {
                        a + 1..*top
                    } else {
                        a + 1..a + b
                    };
                    let param_count = if param_range.start + 1 >= param_range.end {
                        0
                    } else {
                        param_range.end - param_range.start - 1
                    };

                    let mut param_vec = Vec::with_capacity(param_count);    // TODO: Verify that this doesn't lead to oversized allocations
                    if param_prefix.len() > 0 {
                        param_vec.extend(param_prefix);
                    }
                    for i in param_range {
                        param_vec.push(get_reg(registers, i)?.clone());
                    }

                    return CallResult::TailCall { function, parameters: param_vec }
                }
                opcodes::RETURN => {
                    let result_range = match b {
                        0 => a..registers.len(),
                        1 => a..a,
                        b => a..a + b - 1
                    };
                    let result_count = if result_range.start + 1 > result_range.end {
                        0
                    } else {
                        result_range.end - result_range.start - 1
                    };

                    let mut results = Vec::with_capacity(result_count);
                    for i in result_range {
                        results.push(get_reg(registers, i)?.clone())
                    }
                    return CallResult::Ok(results.into());
                }
                opcodes::RETURN0 => {
                    return CallResult::Ok(Varargs::empty());
                }
                opcodes::RETURN1 => {
                    return CallResult::Ok(Varargs::from(get_reg(registers, a)?.clone()));
                }
                opcodes::FORLOOP => {
                    match LuaNumber::coerce_from(get_reg(registers, a + 2)?)? {
                        LuaNumber::INT(step) => {
                            let count = LUA_INT::coerce_from(get_reg(registers, a + 1)?)? as LUA_INT_UNSIGNED;
                            if count > 0 {
                                let index = LUA_INT::coerce_from(get_reg(registers, a)?)?;

                                set_reg(registers, a + 1, count - 1)?;
                                set_reg(registers, a, index + step)?;
                                set_reg(registers, a + 3, index + step)?;

                                *pc = *pc - bx;
                            }
                        }
                        LuaNumber::FLOAT(step) => {
                            let limit = LUA_FLOAT::coerce_from(get_reg(registers, a + 1)?)?;
                            let index = LUA_FLOAT::coerce_from(get_reg(registers, a)?)?;
                            if (0.0 < step && index <= limit) || (0.0 >= step && limit <= index) {
                                set_reg(registers, a, index + step)?;
                                set_reg(registers, a + 3, index + step)?;

                                *pc = *pc - bx;
                            }
                        }
                    }
                    Ok(())
                }
                opcodes::FORPREP => {   // TODO: Provide better errors on coercions
                    let init = get_reg(registers, a)?.clone();  // Early clone to release borrow on 'registers' before the cloned value is moved.
                    let limit = get_reg(registers, a + 1)?;
                    let step = get_reg(registers, a + 2)?;

                    // Weird pattern matching gets around a borrowing edge case: if-let with tuples moves the values, which we want to avoid here. (If-let-chains still experimental at time of writing)
                    let init_opt = if let LuaValue::NUMBER(LuaNumber::INT(init_int)) = init { Some(init_int) } else { None };
                    let step_opt = if let LuaValue::NUMBER(LuaNumber::INT(step_int)) = *step { Some(step_int) } else { None };
                    if let (Some(init_int), Some(step_int)) = (init_opt, step_opt) {
                        if step_int == 0 {
                            Err(LuaError::new("Loop with step of 0!"))?;
                        }

                        // Convert limit to integer
                        let mut skip = false;
                        let limit_int = if let Some(limit_int) = LUA_INT::coerce_opt(limit) {
                            limit_int
                        } else {
                            let limit_float = LUA_FLOAT::coerce_from(limit)?;

                            let limit_int = if 0.0 > limit_float {
                                if step_int < 0 {
                                    skip = true;
                                    0
                                } else {
                                    LUA_INT::MAX
                                }
                            } else {
                                if step_int > 0 {
                                    skip = true;
                                    0
                                } else {
                                    LUA_INT::MIN
                                }
                            };

                            limit_int
                        };

                        skip |= if step_int > 0 {
                            init_int > limit_int
                        } else {
                            init_int < limit_int
                        };

                        if skip {
                            *pc += bx + 1;
                        } else {
                            let counter = if step_int > 0 {
                                (limit_int as LUA_INT_UNSIGNED).wrapping_sub(init_int as LUA_INT_UNSIGNED) / (step_int as LUA_INT_UNSIGNED)
                            } else {
                                (init_int as LUA_INT_UNSIGNED).wrapping_sub(limit_int as LUA_INT_UNSIGNED) / (((-(step_int + 1)) as LUA_INT_UNSIGNED) + 1)
                            };
                            set_reg(registers, a + 1, counter as LUA_INT)?;
                        }

                        set_reg(registers, a + 3, init)?;
                    } else {
                        let limit_float = LUA_FLOAT::coerce_from(limit)?;
                        let step_float = LUA_FLOAT::coerce_from(step)?;
                        let init_float = LUA_FLOAT::coerce_from(&init)?;

                        if step_float == 0.0 as LUA_FLOAT {
                            Err(LuaError::new("Loop with step of 0!"))?;
                        } else {
                            if (0.0 < step_float && limit_float < init_float) || (0.0 >= step_float && init_float < limit_float) {
                                *pc += bx + 1;
                            } else {
                                set_reg(registers, a, init_float)?;
                                set_reg(registers, a + 1, limit_float)?;
                                set_reg(registers, a + 2, step_float)?;
                                set_reg(registers, a + 3, init_float)?;
                            }
                        }
                    }

                    // set_reg(registers, a, (get_reg(registers, a)? - get_reg(registers, a + 2)?)?)?;
                    Ok(())
                }
                opcodes::TFORPREP => {
                    *pc += bx;
                    Ok(())
                }
                opcodes::TFORCALL => {
                    let function = get_reg(registers, a)?.clone();
                    let (function, mut parameter_prefix) = function.prep_call_with_metatable(&lua_vm.metatables)?;
                    parameter_prefix.extend([get_reg(registers, a + 1)?.clone(), get_reg(registers, a + 2)?.clone()]);
                    return CallResult::NewCall {
                        function,
                        parameters: parameter_prefix,
                        frame_return: FrameReturn::UpStack {
                            from_index: a + 4,
                            amount: c
                        }
                    };
                }
                opcodes::TFORLOOP => {
                    if get_reg(registers, a + 4)? != &LuaValue::NIL {
                        set_reg(registers, a + 2, get_reg(registers, a + 4)?.clone())?;
                        *pc -= bx;
                    }
                    Ok(())
                }
                opcodes::SETLIST => {
                    let table = LuaTable::coerce_from(get_reg(registers, a)?)?;
                    let start_index = if k == 1 {
                        let extra_arg_op = next_op(proto, pc)?;
                        if extra_arg_op.opcode() == opcodes::EXTRAARG {
                            (extra_arg_op.unpack().ax << 8) | c
                        } else {
                            Err(ByteCodeError::ExpectedOpcode { expected: opcodes::EXTRAARG, found: opcode })?
                        }
                    } else {
                        c
                    };
                    let element_count = if b == 0 {
                        top.checked_sub(a + 1).ok_or(ByteCodeError::SetlistUnderflow)?
                    } else {
                        b
                    };
                    for i in 1..=element_count {
                        let value = get_reg(registers, a + i)?;
                        table.raw_set(LuaNumber::from(start_index + i), value.clone());
                    }
                    Ok(())
                }
                opcodes::CLOSURE => {
                    let new_proto = proto.functions.get(bx).ok_or(ByteCodeError::PrototypeIndexOutOfBounds { prototype_index: bx, prototype_len: proto.functions.len() })?.clone();

                    let mut new_upvalues: Vec<Upvalue> = Vec::with_capacity(new_proto.upvalue_descriptors.len());    // TODO: set _ENV
                    for desc in &new_proto.upvalue_descriptors {
                        let upvalue;
                        if desc.in_stack() {
                            debug_assert_eq!(frame.upvalues_referencing_this_frame.len(), registers.len());
                            let option = frame.upvalues_referencing_this_frame.get_mut(desc.index())
                                .ok_or(ByteCodeError::RegisterIndexOutOfBounds { index: desc.index(), registers_length: registers.len() })?;

                            upvalue = match option {
                                None => {
                                    let upval = Upvalue::new_open(desc.index(), frame_index);
                                    option.replace(upval.clone());
                                    upval
                                }
                                Some(upval) => upval.clone(),
                            };
                        } else {
                            upvalue = upvalues.get(desc.index()).ok_or(ByteCodeError::UpvalueIndexOutOfBounds { upvalue_index: desc.index(), upvalues_length: upvalues.len() })?.clone();
                        }
                        new_upvalues.push(upvalue);
                    }
                    set_reg(registers, a, LuaFunction::new_lua(new_proto, new_upvalues))?;
                    Ok(())
                }
                opcodes::VARARG => {
                    let vararg_len = match c {
                        0 => parameters.len().saturating_sub(proto.param_count as usize),
                        c => c - 1
                    };

                    // Variadic functions have registers' length set to the minimum value, so we need to upsize when copying parameters
                    for _ in registers.len()..(a + vararg_len) {
                        registers.push(LuaValue::NIL);
                    }
                    for _ in frame.upvalues_referencing_this_frame.len()..(a + vararg_len) {
                        frame.upvalues_referencing_this_frame.push(None);
                    }

                    for i in 0..vararg_len {
                        set_reg(registers, a + i, parameters.get(proto.param_count as usize + i).map(LuaValue::clone).unwrap_or(LuaValue::NIL))?
                    }
                    *top = a + vararg_len;

                    let upvalues = &mut frame.upvalues_referencing_this_frame;
                    for _ in upvalues.len()..(a + vararg_len) {
                        upvalues.push(None);
                    }
                    // Err(LuaError::of_value(parameters.len()))?
                    Ok(())
                }
                opcodes::VARARGPREP => {
                    Ok(()) // No-op in our implementation
                }
                opcodes::EXTRAARG => Err(ByteCodeError::AttemptToExecuteExtraArg),
                _ => Err(ByteCodeError::UnknownOpcode { opcode })
            }?
        };
        if let Err(err) = result {
            return CallResult::Err(err);
        }
    }
}
