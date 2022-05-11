//! Module for Lua 'function' type
//!
//! The concrete type for all kinds of Lua function is [`LuaFunction`]
//!
//! Rust functions that can be called from Lua have the signature `fn(&mut LuaVM, &[LuaValue]) -> Result<Varargs, LuaError>`

use std::fmt::{Formatter, Debug, Display};
use std::fmt;
use crate::types::value::string::LuaString;
use crate::constants::types::{HOST_INT, LUA_INSTRUCTION, LUA_FLOAT, UnpackedInstruction};
use crate::types::value::LuaValue;
use crate::types::upvalue::{UpvalueDesc, Upvalue};
use std::rc::{Rc, Weak};
use std::cell::{Ref, RefCell, RefMut};
use crate::vm::LuaVM;
use crate::types::varargs::Varargs;
use crate::types::{AsLuaPointer, ref_to_pointer, LuaType, CoerceFrom};
use crate::constants::opcodes;
use crate::error::LuaError;


/// Debug information for local variables
#[derive(Debug, PartialEq)]
pub struct LocalVariableInfo {
    /// Name of local variable
    pub name: Option<LuaString>,
    /// Start of scope of variable
    pub startpc: HOST_INT,
    /// End of scope of variable
    pub endpc: HOST_INT,
}

/// Struct for Lua function prototype
pub struct Prototype {
    /// Upvalue size value, used for round trip dumping of bytecode
    /// Use [`Prototype::upvalue_descriptors`]'s length instead
    pub(crate) upvalue_size: u8,
    /// Origin of this prototype; An optional string describing where this prototype was loaded from.
    pub origin_string: Option<LuaString>,
    pub first_line_defined: HOST_INT,
    pub last_line_defined: HOST_INT,
    pub param_count: u8,
    pub is_vararg: u8,
    pub max_stack_size: u8,
    pub code: Vec<LUA_INSTRUCTION>,
    pub constants: Vec<LuaValue>,
    pub upvalue_descriptors: Vec<UpvalueDesc>,
    pub functions: Vec<Rc<Prototype>>,
    pub lineinfo: Vec<i8>,
    pub abslineinfo: Vec<(HOST_INT, HOST_INT)>,
    pub localvariableinfo: Vec<LocalVariableInfo>,
    pub upvaluenames: Vec<Option<LuaString>>,
    pub parent: RefCell<Option<Weak<Prototype>>>
}

impl Prototype {
    fn get_abs_lineinfo(&self, pc: usize) -> (i32, i32) {
        if self.abslineinfo.len() == 0 || pc < self.abslineinfo[0].0 as usize {
            (-1, self.first_line_defined)
        } else {
            let mut i = (pc / 128) as isize - 1;
            assert!(i < 0 || (i < self.abslineinfo.len() as isize && self.abslineinfo[i as usize].0 as usize <= pc));
            while i + 1 < self.abslineinfo.len() as isize && pc >= self.abslineinfo[(i + 1) as usize].0 as usize {
                i += 1
            }
            return self.abslineinfo[i as usize];
        }
    }

    /// Returns line for a given program counter index, if available
    ///
    /// # Arguments
    ///
    /// * `pc`: Instruction index to look up
    ///
    /// returns: Option<usize>
    pub fn get_line(&self, pc: usize) -> Option<usize> {
        if self.lineinfo.is_empty() {
            return None;
        } else {
            let (mut basepc, mut baseline) = self.get_abs_lineinfo(pc);
            while basepc < (pc as HOST_INT) && (basepc < (self.lineinfo.len() as i32 - 1)) {
                basepc += 1;
                baseline += self.lineinfo[basepc as usize] as i32;
            }
            Some(baseline as usize)
        }
    }
}

/// Utility struct, packs all information needed to Display a LUA_INSTRUCTION
pub struct InstructionDisplay<'a> {
    pub(crate) proto: &'a Prototype,
    pub(crate) index: usize,
    pub(crate) instruction: LUA_INSTRUCTION,
}

impl<'a> Display for InstructionDisplay<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let InstructionDisplay { proto, index, instruction } = *self;

        let UnpackedInstruction {
            opcode,
            a,
            b,
            sb,
            c,
            sc,
            bx,
            sbx,
            ax,
            sj,
            k
        } = instruction.unpack();

        match opcode {
            opcodes::MOVE => write!(f, "MOVE\tR[{}] = R[{}]", a, b),

            opcodes::LOADI => write!(f, "LOADI\tR[{}] = {}", a, sbx),
            opcodes::LOADF => write!(f, "LOADF\tR[{}] = {}", a, sbx as LUA_FLOAT),
            opcodes::LOADK => write!(f, "LOADK\tR[{}] = `{}`", a, proto.constants.get(bx).unwrap_or(&LuaValue::NIL)),
            opcodes::LOADKX => write!(f, "LOADKX\tR[{}] = K(EXTRA_ARG)", a),
            opcodes::LOADFALSE => write!(f, "LOADFALSE\tR[{}] = false", a),
            opcodes::LOADFALSESKIP => write!(f, "LOADFALSESKIP\tR[{}] = false; pc++", a),
            opcodes::LOADTRUE => write!(f, "LOADTRUE\tR[{}] = true", a),
            opcodes::LOADNIL => write!(f, "LOADNIL\tR({}..={}) = NIL", a + 1, a + b),

            opcodes::GETUPVAL => write!(f, "GETUPVAL\tR[{}] = Upval[{}]", a, b),
            opcodes::SETUPVAL => write!(f, "SETUPVAL\tUpval[{}] = R[{}]", b, a),

            opcodes::GETTABUP => write!(f, "GETTABUP\tR[{}] = Upval[{}][`{}`]", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::GETTABLE => write!(f, "GETTABLE\tR[{}] = R[{}][R[{}]]", a, b, c),
            opcodes::GETI => write!(f, "GETI\tR[{}] = R[{}][{}]", a, b, c),
            opcodes::GETFIELD => write!(f, "GETFIELD\tR[{}] = R[{}][`{}`]", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),

            opcodes::SETTABUP => {
                if k == 1 {
                    write!(f, "SETTABUP\tUpval[{}][`{}`] = `{}`", a, proto.constants.get(b).unwrap_or(&LuaValue::NIL), proto.constants.get(c).unwrap_or(&LuaValue::NIL))
                } else {
                    write!(f, "SETTABUP\tUpval[{}][`{}`] = R[{}]", a, proto.constants.get(b).unwrap_or(&LuaValue::NIL), c)
                }
            }
            opcodes::SETTABLE => {
                if k == 1 {
                    write!(f, "SETTABLE\tR[{}][R[{}]] = `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL))
                } else {
                    write!(f, "SETTABLE\tR[{}][R[{}]] = R[{}]", a, b, c)
                }
            }
            opcodes::SETI => {
                if k == 1 {
                    write!(f, "SETI\tR[{}][{}] = `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL))
                } else {
                    write!(f, "SETI\tR[{}][{}] = R[{}]", a, b, c)
                }
            }
            opcodes::SETFIELD => {
                if k == 1 {
                    write!(f, "SETFIELD\tR[{}][`{}`] = `{}`", a, proto.constants.get(b).unwrap_or(&LuaValue::NIL), proto.constants.get(c).unwrap_or(&LuaValue::NIL))
                } else {
                    write!(f, "SETFIELD\tR[{}][`{}`] = R[{}]", a, proto.constants.get(b).unwrap_or(&LuaValue::NIL), c)
                }
            }

            opcodes::NEWTABLE => write!(f, "NEWTABLE\tR[{}] = {{}}", a),

            opcodes::SELF => write!(f, "SELF\tR[{}] = R[{}]; R[{}] = R[{}][`{}`]", a + 1, b, a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),

            opcodes::ADDI => write!(f, "ADDI\tR[{}] = R[{}] + {}", a, b, sc),
            opcodes::ADDK => write!(f, "ADDK\tR[{}] = R[{}] + `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::SUBK => write!(f, "SUBK\tR[{}] = R[{}] - `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::MULK => write!(f, "MULK\tR[{}] = R[{}] * `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::MODK => write!(f, "MODK\tR[{}] = R[{}] % `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::POWK => write!(f, "POWK\tR[{}] = R[{}] ^ `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::DIVK => write!(f, "DIVK\tR[{}] = R[{}] / `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::IDIVK => write!(f, "IDIVK\tR[{}] = R[{}] // `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::BANDK => write!(f, "BAND\tR[{}] = R[{}] & `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::BORK => write!(f, "BORK\tR[{}] = R[{}] | `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::BXORK => write!(f, "BXORK\tR[{}] = R[{}] ~ `{}`", a, b, proto.constants.get(c).unwrap_or(&LuaValue::NIL)),
            opcodes::SHRI => write!(f, "SHRI\tR[{}] = R[{}] >> {}", a, b, sc),
            opcodes::SHLI => write!(f, "SHLI\tR[{}] = {} << R[{}]", a, sc, b),

            opcodes::ADD => write!(f, "ADD\tR[{}] = R[{}] + R[{}]", a, b, c),
            opcodes::SUB => write!(f, "SUB\tR[{}] = R[{}] - R[{}]", a, b, c),
            opcodes::MUL => write!(f, "MUL\tR[{}] = R[{}] * R[{}]", a, b, c),
            opcodes::MOD => write!(f, "MOD\tR[{}] = R[{}] % R[{}]", a, b, c),
            opcodes::POW => write!(f, "POW\tR[{}] = R[{}] ^ R[{}]", a, b, c),
            opcodes::DIV => write!(f, "DIV\tR[{}] = R[{}] / R[{}]", a, b, c),
            opcodes::IDIV => write!(f, "IDIV\tR[{}] = R[{}] // R[{}]", a, b, c),
            opcodes::BAND => write!(f, "BAND\tR[{}] = R[{}] & R[{}]", a, b, c),
            opcodes::BOR => write!(f, "BOR\tR[{}] = R[{}] | R[{}]", a, b, c),
            opcodes::BXOR => write!(f, "BXOR\tR[{}] = R[{}] ~ R[{}]", a, b, c),
            opcodes::SHL => write!(f, "SHL\tR[{}] = R[{}] << R[{}]", a, b, c),
            opcodes::SHR => write!(f, "SHR\tR[{}] = R[{}] >> R[{}]", a, b, c),

            opcodes::MMBIN => write!(f, "MMBIN\t{}(R[{}], R[{}])", c, a, b),
            opcodes::MMBINI => write!(f, "MMBINI\t{}(R[{}], {})", c, a, sb),
            opcodes::MMBINK => write!(f, "MMBINK\t{}(R[{}], `{}`)", c, a, proto.constants.get(b).unwrap_or(&LuaValue::NIL)),

            opcodes::UNM => write!(f, "UNM\tR[{}] = -R[{}]", a, b),
            opcodes::BNOT => write!(f, "BNOT\tR[{}] = ~R[{}]", a, b),
            opcodes::NOT => write!(f, "NOT\tR[{}] = not R[{}]", a, b),
            opcodes::LEN => write!(f, "LEN\tR[{}] = #R[{}]", a, b),

            opcodes::CONCAT => write!(f, "CONCAT\tR({}) = R({}) .. ... .. R({})", a, a, a + b + 1),

            opcodes::CLOSE => write!(f, "CLOSE\tR[{}..]", a),
            opcodes::TBC => write!(f, "TBC\tMark for close R[{}]", a),

            opcodes::JMP => write!(f, "JMP\tto {} ({:+})", (index as isize) + sj + 2, sj), // Offset the target index by 2; One as the display indices start at 1 whereas the opcode-array starts at 0, and one to accommodate the fact that lua-jumps occur after the program counter has been incremented.
            opcodes::EQ => {
                if k == 0 {
                    write!(f, "EQ\tR[{}] == R[{}]", a, b)
                } else {
                    write!(f, "EQ\tR[{}] != R[{}]", a, b)
                }
            }
            opcodes::LT => {
                if k == 0 {
                    write!(f, "LT\tR[{}] < R[{}]", a, b)
                } else {
                    write!(f, "LT\tR[{}] >= R[{}]", a, b)
                }
            }
            opcodes::LE => {
                if k == 0 {
                    write!(f, "LE\tR[{}] <= R[{}]", a, b)
                } else {
                    write!(f, "LE\tR[{}] > R[{}]", a, b)
                }
            }

            opcodes::EQK => {
                if k == 0 {
                    write!(f, "EQK\tR[{}] == `{}`", a, proto.constants.get(b).unwrap_or(&LuaValue::NIL))
                } else {
                    write!(f, "EQK\tR[{}] != `{}`", a, proto.constants.get(b).unwrap_or(&LuaValue::NIL))
                }
            }
            opcodes::EQI => {
                if k == 0 {
                    write!(f, "EQI\tR[{}] == {}", a, sb)
                } else {
                    write!(f, "EQI\tR[{}] != {}", a, sb)
                }
            }
            opcodes::LTI => {
                if k == 0 {
                    write!(f, "LTI\tR[{}] < {}", a, sb)
                } else {
                    write!(f, "LTI\tR[{}] >= {}", a, sb)
                }
            }
            opcodes::LEI => {
                if k == 0 {
                    write!(f, "LEI\tR[{}] <= {}", a, sb)
                } else {
                    write!(f, "LEI\tR[{}] > {}", a, sb)
                }
            }
            opcodes::GTI => {
                if k == 0 {
                    write!(f, "GTI\tR[{}] > {}", a, sb)
                } else {
                    write!(f, "GTI\tR[{}] <= {}", a, sb)
                }
            }
            opcodes::GEI => {
                if k == 0 {
                    write!(f, "GEI\tR[{}] >= {}", a, sb)
                } else {
                    write!(f, "GEI\tR[{}] < {}", a, sb)
                }
            }

            opcodes::TEST => {
                if k == 0 {
                    write!(f, "TEST\tif R[{}]", a)
                } else {
                    write!(f, "TEST\tif not R[{}]", a)
                }
            }
            opcodes::TESTSET => {
                if k == 0 {
                    write!(f, "TEST\tif R[{}] else R[{}] = R[{}]", b, a, b)
                } else {
                    write!(f, "TEST\tif not R[{}] else R[{}] = R[{}]", b, a, b)
                }
            }

            opcodes::CALL => {
                if b == 0 {
                    if c == 0 {
                        write!(f, "CALL\tR[{}..all] = R[{}](R[{}..=top])", a, a, a + 1)
                    } else {
                        write!(f, "CALL\tR[{}..{}] = R[{}](R[{}..=top])", a, a + c - 1, a, a + 1)
                    }
                } else {
                    if c == 0 {
                        write!(f, "CALL\tR[{}..all] = R[{}](R[{}..={}])", a, a, a + 1, a + b - 1)
                    } else {
                        write!(f, "CALL\tR[{}..{}] = R[{}](R[{}..={}])", a, a + c - 1, a, a + 1, a + b - 1)
                    }
                }
            }
            opcodes::TAILCALL => {
                if b == 0 {
                    write!(f, "TAILCALL\treturn R[{}](R[{}..=top])", a, a + 1)
                } else {
                    write!(f, "TAILCALL\treturn R[{}](R[{}..={}])", a, a + 1, a + b - 1)
                }
            }

            opcodes::RETURN => {
                if b == 0 {
                    write!(f, "RETURN\treturn R[{}..=top]", a)
                } else {
                    write!(f, "RETURN\treturn R[{}..={}]", a, ((a + b) as isize) - 2)
                }
            }
            opcodes::RETURN0 => write!(f, "RETURN\treturn"),
            opcodes::RETURN1 => write!(f, "RETURN\treturn R[{}]", a),

            opcodes::FORLOOP => write!(f, "FORLOOP\tloop -{}", bx),
            opcodes::FORPREP => write!(f, "FORPREP\tprep {} +{}", a, bx + 1),

            opcodes::TFORPREP => write!(f, "TFORPREP\tprep +{}", bx),
            opcodes::TFORCALL => write!(f, "TFORCALL\tR[{}..={}] = R[{}](R[{}], R[{}])", a + 4, a + 3 + c, a, a + 1, a + 2),
            opcodes::TFORLOOP => write!(f, "TFORLOOP\tif R[{}] != nil then R[{}] = R[{}]; jump to {} (-{})", a + 4, a + 2, a + 4, (index as isize) - (bx as isize) + 2, bx), // See #JMP for why this is incremented by 2

            opcodes::SETLIST => {
                let start = if k == 1 {
                    (
                        proto.code.get(index + 1)
                            .copied()
                            .map(LUA_INSTRUCTION::unpack)
                            .map(|i| i.ax << 8)
                            .unwrap_or(0)
                    ) | c
                } else {
                    c
                };
                if b == 0 {
                    write!(f, "SETLIST\tR[{}][{}..=top] = R[{}..=top]", a, start + 1, a + 1)
                } else {
                    write!(f, "SETLIST\tR[{}][{}..={}] = R[{}..={}]", a, start + 1, c + b, a + 1, a + b)
                }
            }

            opcodes::CLOSURE => write!(f, "CLOSURE\tR[{}] = closure({})", a, bx),

            opcodes::VARARG => {
                match c {
                    0 => write!(f, "VARARG\tR[{}..] = ...", a),
                    c => write!(f, "VARARG\tR[{}..={}] = ...", a, a + c - 1)
                }
            }

            opcodes::VARARGPREP => write!(f, "VARARGPREP"),

            opcodes::EXTRAARG => write!(f, "EXTRAARG\t{}", ax),
            _ => write!(f, "[UNKNOWN OPCODE] {}", opcode)
        }
    }
}

//noinspection ALL
impl Display for Prototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (index, instruction) in self.code.iter().enumerate() {
            match self.get_line(index) {
                Some(line) => {
                    write!(f, "{}\t[{}]\t{}\n", index, line, InstructionDisplay {
                        proto: self,
                        index,
                        instruction: *instruction,
                    })?;
                }
                None => {
                    write!(f, "{}\t{}\n", index, InstructionDisplay {
                        proto: self,
                        index,
                        instruction: *instruction,
                    })?;
                }
            }
        }
        write!(f, "CONSTANTS: {}\n", self.constants.len())?;
        for (index, constant) in self.constants.iter().enumerate() {
            write!(f, "{}\t{}\n", index, constant)?;
        }

        write!(f, "UPVALUES: {}\n", self.upvalue_descriptors.len())?;
        for (index, upvalue) in self.upvalue_descriptors.iter().enumerate() {
            let unnamed = LuaString::UNICODE(Rc::from("Unnamed upvalue"));
            let name = &match match self.upvaluenames.get(index) {
                None => &None,
                Some(opt) => opt,
            } {
                Some(name) => name,
                None => &unnamed,
            };
            write!(f, "{}\t{}\t{}\n", name, index, upvalue)?;
        }

        write!(f, "FUNCTIONS: {}\n", self.functions.len())?;
        for (index, function) in self.functions.iter().enumerate() {
            write!(f, "Function-{}\n{}", index, function)?;
        }
        Ok(())
    }
}

impl Debug for Prototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        struct LinesDefined(HOST_INT, HOST_INT);
        impl Debug for LinesDefined {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                write!(f, "{}-{}", self.0, self.1)
            }
        }

        if let Some(name) = &self.origin_string {
            let mut partial_debug = f.debug_struct("Prototype");
            if let Some(string) = name.as_str() {
                partial_debug.field("source", &string);
            } else {
                partial_debug.field("source", name);
            };
            if self.first_line_defined != 0 || self.last_line_defined != 0 {
                partial_debug.field("lines", &LinesDefined(self.first_line_defined, self.last_line_defined)).finish()
            } else {
                partial_debug.finish()
            }
        } else {
            f.debug_tuple("Prototype").field(&self.origin_string).finish()
        }
    }
}

/// Struct wrapper for rust function pointers, attaches a name to function
///
/// See [`RustClosure`] for equivalent that takes closures
#[derive(Copy, Clone)]
pub struct RustFunction {
    name: &'static str,
    function: fn(&mut LuaVM, &[LuaValue]) -> Result<Varargs, LuaError>,
}

impl RustFunction {
    pub fn from_parts(name: &'static str, inner: fn(&mut LuaVM, &[LuaValue]) -> Result<Varargs, LuaError>) -> RustFunction {
        RustFunction {
            name,
            function: inner,
        }
    }

    pub fn ptr(&self) -> fn(&mut LuaVM, &[LuaValue]) -> Result<Varargs, LuaError> {
        self.function
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    #[inline(always)]
    pub fn call(&self, lua_vm: &mut LuaVM, parameters: &[LuaValue]) -> Result<Varargs, LuaError> {
        (self.function)(lua_vm, parameters)
    }
}

impl LuaType for RustFunction {
    const TYPE_NAME: &'static str = "rust function";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for RustFunction {
    fn coerce_opt(value: &T) -> Option<Self> {
        if let LuaValue::FUNCTION(LuaFunction::RUST_FUNCTION(func)) = value.clone().into() {
            Some(func)
        } else {
            None
        }
    }
}


/// Struct wrapper for rust closures, attaches a name to function
///
/// See [`RustFunction`] for equivalent that takes function pointers
#[derive(Clone)]
pub struct RustClosure {
    name: &'static str,
    closure: Rc<RefCell<dyn FnMut(&mut LuaVM, &[LuaValue]) -> Result<Varargs, LuaError>>>,
}

impl RustClosure {
    pub fn new<T: 'static + FnMut(&mut LuaVM, &[LuaValue]) -> Result<Varargs, LuaError>>(name: &'static str, closure: T) -> RustClosure {
        RustClosure {
            name,
            closure: Rc::new(RefCell::new(closure)),
        }
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn call(&self, lua_vm: &mut LuaVM, parameters: &[LuaValue]) -> Result<Varargs, LuaError> {
        self.closure.borrow_mut()(lua_vm, parameters)   // TODO: This will break on recursive calls
    }
}

impl LuaType for RustClosure {
    const TYPE_NAME: &'static str = "rust closure";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for RustClosure {
    fn coerce_opt(value: &T) -> Option<Self> {
        if let LuaValue::FUNCTION(LuaFunction::RUST_CLOSURE(func)) = value.clone().into() {
            Some(func)
        } else {
            None
        }
    }
}

/// Wrapper struct for prototypes; Packaging prototype and it's upvalues
#[derive(Clone)]
pub struct LuaClosure {
    prototype: Rc<Prototype>,
    upvalues: Rc<RefCell<Vec<Upvalue>>>, // Refcell is used as "identity" pointer for this object
}

impl LuaType for LuaClosure {
    const TYPE_NAME: &'static str = "lua closure";
}

impl LuaClosure {
    // TODO: Create helper function
    /// Creates new LuaClosure, for proper execution upvalues must match those expected by prototype.
    ///
    /// Mismatched/missing upvalues will result in a LuaError thrown during execution
    ///
    /// Top-level scripts expect only the _ENV upvalue
    ///
    /// # Arguments
    ///
    /// * `prototype`: Prototype for this closure
    /// * `upvalues`: Upvalues for prototype
    ///
    /// returns: LuaClosure
    pub fn new(prototype: Rc<Prototype>, upvalues: Vec<Upvalue>) -> LuaClosure {
        LuaClosure {
            prototype,
            upvalues: Rc::new(RefCell::new(upvalues)),
        }
    }

    #[inline(always)]
    pub fn prototype(&self) -> &Prototype {
        &*self.prototype
    }

    #[inline(always)]
    pub fn upvalues(&self) -> Ref<'_, Vec<Upvalue>> {
        self.upvalues.borrow()
    }

    #[inline(always)]
    pub fn upvalues_mut(&self) -> RefMut<'_, Vec<Upvalue>> {
        self.upvalues.borrow_mut()
    }

    #[inline(always)]
    pub fn clone_prototype(&self) -> Rc<Prototype> {
        self.prototype.clone()
    }
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaClosure {
    fn coerce_opt(value: &T) -> Option<Self> {
        if let LuaValue::FUNCTION(LuaFunction::LUA_CLOSURE(func)) = value.clone().into() {
            Some(func)
        } else {
            None
        }
    }
}

impl Debug for LuaClosure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Ok(inner) = self.upvalues.try_borrow() {
            f.debug_struct("LuaClosure")
                .field("prototype", &*self.prototype)
                .field("upvalues", &inner)
                .finish()
        } else {
            f.debug_struct("LuaClosure")
                .field("prototype", &*self.prototype)
                .field("upvalues", &"<borrowed>")
                .finish()
        }
    }
}

/// Top level type for Lua functions
#[allow(non_camel_case_types)]
#[derive(Clone)]
pub enum LuaFunction {
    LUA_CLOSURE(LuaClosure),
    RUST_FUNCTION(RustFunction),
    RUST_CLOSURE(RustClosure),
}

impl LuaFunction {
    pub fn new_rust(name: &'static str, function: fn(&mut LuaVM, &[LuaValue]) -> Result<Varargs, LuaError>) -> LuaFunction {
        LuaFunction::RUST_FUNCTION(RustFunction { name, function })
    }

    pub fn new_lua(prototype: Rc<Prototype>, upvalues: Vec<Upvalue>) -> LuaFunction {
        LuaFunction::LUA_CLOSURE(
            LuaClosure::new(prototype, upvalues)
        )
    }


    /// Calls this LuaFunction
    ///
    /// NOTE: This is only equivalent to [`LuaValue::call`] for the [`LuaValue::FUNCTION`] variant. LuaValue::call should be used when calling parameters or other values of unknown type.
    ///
    /// # Arguments
    ///
    /// * `lua_vm`: LuaVM for this call
    /// * `params`: Parameters of this call
    ///
    /// returns: Result<Varargs, LuaError>
    pub fn call(&self, lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
        match self {
            LuaFunction::LUA_CLOSURE(closure) => {
                crate::vm::execute_closure(closure.clone(), lua_vm, params)
                    .map_err(|e| e)
            }
            LuaFunction::RUST_FUNCTION(function) => {
                function.ptr()(lua_vm, params)
                    .map_err(|e| e.trace_native(function.clone()))
            }
            LuaFunction::RUST_CLOSURE(closure) => {
                use std::ops::DerefMut;
                closure.closure.borrow_mut().deref_mut()(lua_vm, params)
                    .map_err(|e| e.trace_closure(closure.name))
            }
        }
    }
}

impl LuaType for LuaFunction {
    const TYPE_NAME: &'static str = "function";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaFunction {
    fn coerce_opt(value: &T) -> Option<Self> {
        if let LuaValue::FUNCTION(func) = value.clone().into() {
            Some(func)
        } else {
            None
        }
    }
}

impl Debug for LuaFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LuaFunction::LUA_CLOSURE(c) => {
                if let Ok(upvalues) = c.upvalues.try_borrow() {
                    f.debug_tuple("LuaFunction::LUA_CLOSURE").field(&*c.prototype).field(&*upvalues).finish()
                } else {
                    f.debug_tuple("LuaFunction::LUA_CLOSURE").field(&*c.prototype).field(&"<upvalues borrowed>").finish()
                }
            }
            LuaFunction::RUST_FUNCTION(func) => f.debug_tuple("LuaFunction::RUST_FUNCTION").field(&func.name).field(&(func.function as *const ())).finish(),          // Deref function pointer into raw pointer
            LuaFunction::RUST_CLOSURE(c) => f.debug_tuple("LuaFunction::RUST_CLOSURE").field(&c.name).field(&(c as *const RustClosure)).finish()
        }
    }
}

impl Display for LuaFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        <Self as Debug>::fmt(self, f)
    }
}

impl AsLuaPointer for LuaFunction {
    fn as_lua_pointer(&self) -> usize {
        match self {
            LuaFunction::LUA_CLOSURE(c) => ref_to_pointer(&*c.upvalues),  // Pointer to refcell, as refcell has exclusive ownership of the closure we don't need to enter and get a ref to it's contents it here
            LuaFunction::RUST_FUNCTION(f) => f.function as *const () as usize,      // Deref function pointer into raw pointer
            LuaFunction::RUST_CLOSURE(c) => ref_to_pointer(c.closure.as_ref()), // Ditto
        }
    }
}

impl Eq for LuaFunction {}

impl PartialEq for LuaFunction {
    fn eq(&self, other: &Self) -> bool {
        self.as_lua_pointer() == other.as_lua_pointer()
    }
}
