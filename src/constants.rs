//! Module for definition of most constants used in this VM
//!
//! Default values assume an x86 64bit architecture, and may need to be changed when compiling targeting a significantly different architecture

use types::{LUA_FLOAT, LUA_INT};

/// Types used for numerical values
///
/// Sizes
#[allow(non_camel_case_types)]
pub mod types {
    use std::fmt::{Debug, Formatter};

    /// A single byte on the host architecture
    pub type HOST_BYTE = u8;
    /// Signed equivalent of [`HOST_BYTE`]
    pub type HOST_SIGNED_BYTE = i8;
    /// C 'int' type equivalent, usually 32-bits
    pub type HOST_INT = i32;
    /// Object size on host architecture, alias for usize
    pub type HOST_OBJECT_SIZE = usize;
    /// Representation of Lua integers
    pub type LUA_INT = i64;
    /// Unsigned equivalent of [`LUA_INT`], must have equal size as LUA_INT in memory
    pub type LUA_INT_UNSIGNED = u64;
    /// Representation of Lua floats, must have equal size as LUA_INT in memory
    pub type LUA_FLOAT = f64;


    /// Lua instruction type, newtype around u32
    #[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
    pub struct LUA_INSTRUCTION {
        pub(crate) inner: u32,
    }

    /// Unpacked instruction, each field of the instruction separated out
    ///
    /// NOTE: While each argument is determined for every opcode, their values may not be valid or applicable
    /// See: <http://www.lua.org/source/5.4/lopcodes.h.html> for which opcodes use which arguments
    pub struct UnpackedInstruction {
        pub opcode: u8,
        pub a: usize,
        pub b: usize,
        pub sb: isize,
        pub c: usize,
        pub sc: isize,
        pub bx: usize,
        pub sbx: isize,
        pub ax: usize,
        pub sj: isize,
        pub k: u8,
    }

    impl LUA_INSTRUCTION {
        /// Returns a guaranteed-invalid instruction, the returned value is not guaranteed to remain invalid in future versions and accordingly should not be persisted
        pub fn invalid() -> Self {
            LUA_INSTRUCTION { inner: u32::MAX }
        }

        /// Returns opcode for this instruction
        pub fn opcode(self) -> u8 {
            self.unpack().opcode
        }

        pub fn opcode_name(self) -> &'static str {
            use crate::constants::opcodes;
            opcodes::opcode_name(self.unpack().opcode)
        }

        /// Unpacks a [`LUA_INSTRUCTION`] into a [`UnpackedInstruction`]; Making it's fields directly accessible.
        #[inline(always)]   // This function calculates every argument, inlining enables compiler to optimize out all unused arguments
        pub fn unpack(self) -> UnpackedInstruction {
            let opcode: u8 = (self.inner & 0b111_1111) as u8;
            let a = (self.inner >> 7) as usize & 0b1111_1111;
            let b = (self.inner >> (7 + 8 + 1)) as usize & 0b1111_1111;
            let sb = ((self.inner >> (7 + 8 + 1)) as usize & 0b1111_1111) as isize - ((2isize.pow(8) / 2) - 1);
            let c = (self.inner >> (7 + 8 + 8 + 1)) as usize & 0b1111_1111;
            let sc = ((self.inner >> (7 + 8 + 8 + 1)) as usize & 0b1111_1111) as isize - ((2isize.pow(8) / 2) - 1);
            let bx = (self.inner >> (7 + 8)) as usize & !(!0 << 17); // 17 bits
            let sbx = bx as isize - ((2isize.pow(17) / 2) - 1);
            let ax = (self.inner >> 7) as usize & !(!0 << 25); // 25 bits
            let sj = ax as isize - ((2isize.pow(25) / 2) - 1);

            let k = (self.inner >> (7 + 8)) as u8 & 0b1;

            UnpackedInstruction {
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
                k,
            }
        }
    }

    impl Debug for LUA_INSTRUCTION {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.debug_tuple("LuaInstruction").field(&self.opcode_name()).finish()
        }
    }

    // Compile-time assertions
    static _ASSERTIONS: () = {
        if std::mem::size_of::<LUA_INT>() != std::mem::size_of::<LUA_INT_UNSIGNED>() {
            panic!("Signed and Unsigned LUA_INT must have equal size in memory!")
        };
        if std::mem::size_of::<LUA_FLOAT>() != std::mem::size_of::<LUA_INT>() {
            panic!("LUA_FLOAT and LUA_INT must have equal size in memory!")
        };
    };
}

/// Lua type tags
#[allow(unused)]
pub mod typetag {
    pub const TNIL: u8 = 0;
    pub const TBOOLEAN: u8 = 1;
    pub const TLIGHTUSERDATA: u8 = 2;
    pub const TNUMBER: u8 = 3;
    pub const TSTRING: u8 = 4;
    pub const TTABLE: u8 = 5;
    pub const TFUNCTION: u8 = 6;
    pub const TUSERDATA: u8 = 7;
    pub const TTHREAD: u8 = 8;

    pub const VFALSE: u8 = TBOOLEAN | (0 << 4);
    pub const VTRUE: u8 = TBOOLEAN | (1 << 4);

    pub const VFLOAT: u8 = TNUMBER | (1 << 4);
    pub const VINTEGER: u8 = TNUMBER | (0 << 4);

    pub const VSHORTSTRING: u8 = TSTRING | (0 << 4);
    pub const VLONGSTRING: u8 = TSTRING | (1 << 4);
}

/// Lua version for this implementation
///
/// NOTE: Bytecode loaders/dumpers may differ from this
pub const LUA_VERSION: u8 = 0x54;

/// Lua bytecode format for this implementation
///
/// NOTE: Bytecode loaders/dumpers may differ from this
pub const LUA_FORMAT: u8 = 0x00;

/// Lua bytecode signature
///
/// NOTE: Bytecode loaders/dumpers may differ from this
pub const LUA_SIGNATURE: &[u8; 4] = b"\x1bLua";

/// Lua bytecode conversion data
///
/// Used to check against data mangling
///
/// NOTE: Bytecode loaders/dumpers may differ from this
pub const LUA_CONV_DATA: &[u8; 6] = b"\x19\x93\r\n\x1a\n";


/// Lua bytecode check integer
///
/// Used to verify integer parsing yields correct value
///
/// NOTE: Bytecode loaders/dumpers may differ from this
pub const LUA_CHECK_INTEGER: LUA_INT = 0x5678;

/// Lua bytecode check float
///
/// Used to verify float parsing yields correct value
///
/// NOTE: Bytecode loaders/dumpers may differ from this
pub const LUA_CHECK_FLOAT: LUA_FLOAT = 370.5;

/// Lua short string length limit
///
/// NOTE: This value is not required to be identifical to dump/load valid bytecode, it is only required to be identical for round-trip parsing
/// NOTE: Bytecode loaders/dumpers may differ from this
pub const SHORT_STRING_LENGTH: usize = 40;

macro_rules! def_opcodes {
    ($($name:ident = $id:expr);+;) => {
        /// Lua opcodes
        ///
        /// See: <http://www.lua.org/source/5.4/lopcodes.h.html>
        pub mod opcodes {
            $(pub const $name: u8 = $id;)+

            pub fn opcode_name(code: u8) -> &'static str {
                match code {
                    $($id => stringify!($name),)+
                    _ => "[UNKNOWN OPCODE]"
                }
            }
        }
    };
}

def_opcodes!(
    MOVE = 0;
    LOADI = 1;
    LOADF = 2;
    LOADK = 3;
    LOADKX = 4;
    LOADFALSE = 5;
    LOADFALSESKIP = 6;
    LOADTRUE = 7;
    LOADNIL = 8;

    GETUPVAL = 9;
    SETUPVAL = 10;

    GETTABUP = 11;
    GETTABLE = 12;
    GETI = 13;
    GETFIELD = 14;

    SETTABUP = 15;
    SETTABLE = 16;
    SETI = 17;
    SETFIELD = 18;

    NEWTABLE = 19;

    SELF = 20;

    ADDI = 21;
    ADDK = 22;
    SUBK = 23;
    MULK = 24;
    MODK = 25;
    POWK = 26;
    DIVK = 27;
    IDIVK = 28;
    BANDK = 29;
    BORK = 30;
    BXORK = 31;
    SHRI = 32;
    SHLI = 33;

    ADD = 34;
    SUB = 35;
    MUL = 36;
    MOD = 37;
    POW = 38;
    DIV = 39;
    IDIV = 40;
    BAND = 41;
    BOR = 42;
    BXOR = 43;
    SHL = 44;
    SHR = 45;

    MMBIN = 46;
    MMBINI = 47;
    MMBINK = 48;

    UNM = 49;
    BNOT = 50;
    NOT = 51;
    LEN = 52;

    CONCAT = 53;

    CLOSE = 54;
    TBC = 55;
    JMP = 56;
    EQ = 57;
    LT = 58;
    LE = 59;

    EQK = 60;
    EQI = 61;
    LTI = 62;
    LEI = 63;
    GTI = 64;
    GEI = 65;

    TEST = 66;
    TESTSET = 67;

    CALL = 68;
    TAILCALL = 69;

    RETURN = 70;
    RETURN0 = 71;
    RETURN1 = 72;

    FORLOOP = 73;
    FORPREP = 74;

    TFORPREP = 75;
    TFORCALL = 76;
    TFORLOOP = 77;

    SETLIST = 78;

    CLOSURE = 79;

    VARARG = 80;

    VARARGPREP = 81;

    EXTRAARG = 82;
);