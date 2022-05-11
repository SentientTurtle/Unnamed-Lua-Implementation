//! Module for bytecode loading
//!
//! Provided implementations support only Lua 5.4 bytecode

use std::io::{Cursor, Read};
use core::mem;
use std::{fmt, io};
use std::cell::RefCell;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use crate::constants;
use crate::constants::types::{LUA_INT, LUA_FLOAT, LUA_INSTRUCTION, HOST_INT, HOST_SIGNED_BYTE};
use std::rc::Rc;
use crate::types::value::number::LuaNumber;
use crate::types::value::string::LuaString;
use crate::types::value::LuaValue;
use crate::types::upvalue::UpvalueDesc;
use crate::types::value::function::{LocalVariableInfo, Prototype};

/// Error type for bytecode loading
#[derive(Debug)]
pub enum LoadError {
    /// An IO error occurred
    IO(io::Error),
    /// Missing or invalid Lua signature at start of bytecode
    InvalidLuaSignature { found: Vec<u8>, expected: &'static [u8] },
    /// Invalid Conversion Data; Generally indicates mangled bytecode, such as changing file from LF-newlines to CRLF-newlines
    ConversionDataCorrupt { found: Vec<u8>, expected: &'static [u8] },
    /// This loader does not support the system parameters this bytecode was dumped for
    IncompatibleSystemParam { found: Vec<u8>, expected: &'static [u8] },
    /// The bytecode contains values that cannot be represented in the current system's architecture. (E.g. integers greater than i32::MAX on 32-bit systems)
    /// Loader implementations may coerce such values to floating-point instead of raising this error
    CannotRepresentValue,
    /// Check integer value does not match expected value. Found value is passed in error
    CorruptCheckInt(LUA_INT),
    /// Check float value does not match expected value. Found value is passed in error
    CorruptCheckFloat(LUA_FLOAT),
    /// The bytecode's Lua version is not compatible with this loader. Found value is passed in error
    InvalidVersion(u8),
    /// The bytecode's Lua format is not compatible with this loader. Found value is passed in error
    InvalidFormat(u8),
    /// A boolean constant had a binary value that is not 0 or 1. Found value is passed in error
    NonBinaryBoolean(u8),
    /// An unsupported constant type tag was found. Found value is passed in error
    UnsupportedConstantTypeTag(u8),
    /// A null-string was found in the constant field
    NullStringInConstant,
    /// The bytecode's debug information is inconsistent with it's contents; Bytecode loading may succeed with debug information turned off.
    InvalidDebugInfo,
    /// Limit on bytes-to-read was reached during loading
    ReadLimitReached { requested: usize, available: usize }
}

impl LoadError {
    /// Utility function that provides a lua value representation of this error
    pub fn lua_message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl std::error::Error for LoadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        if let LoadError::IO(err) = self {
            Some(err)
        } else {
            None
        }
    }
}

impl From<io::Error> for LoadError {
    fn from(err: io::Error) -> Self {
        LoadError::IO(err)
    }
}

impl Display for LoadError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LoadError::IO(err) => write!(f, "Decode IO error: {}", err),
            LoadError::InvalidLuaSignature { found, expected } => write!(f, "invalid lua signature, found {:X?} expected {:X?}", found, expected),
            LoadError::ConversionDataCorrupt { found, expected } => write!(f, "invalid conversion data, found {:X?} expected {:X?}", found, expected),
            LoadError::IncompatibleSystemParam { found, expected } => write!(f, "invalid system parameter, found {:X?} expected {:X?}", found, expected),
            LoadError::CorruptCheckInt(int) => write!(f, "invalid check int: {}", int),
            LoadError::CorruptCheckFloat(float) => write!(f, "invalid check float: {}", float),
            LoadError::UnsupportedConstantTypeTag(tag) => write!(f, "unsupported constant type tag: {}", tag),
            LoadError::NullStringInConstant => write!(f, "null string in constant"),
            LoadError::NonBinaryBoolean(byte) => write!(f, "non-binary boolean byte: {:X}", byte),
            LoadError::InvalidVersion(version) => write!(f, "invalid version: {}", version),
            LoadError::InvalidFormat(format) => write!(f, "invalid format: {}", format),
            LoadError::InvalidDebugInfo => write!(f, "invalid debug info"),
            LoadError::CannotRepresentValue => write!(f, "cannot represent loader in current architecture"),
            LoadError::ReadLimitReached { requested, available } => write!(f, "memory limit reached, requested {}, available {}", requested, available)
        }
    }
}

/// Limit on amount of bytes that will be read by a Loader.
///
/// Malformed bytecode may include very large byte string literals, limit on total amount of bytes that will be read prevents out-of-memory errors/attacks
pub struct ReadLimit(pub usize);
impl ReadLimit {
    /// Reduces read limit by the specified amount, or throws an error if the limit is too low
    ///
    /// # Arguments
    ///
    /// * `amount`: Amount to take from the limit
    ///
    /// returns: Result<(), LoadError>
    pub fn take(&mut self, amount: usize) -> Result<(), LoadError> {
        if self.0 < amount {
            panic!("{}", LoadError::ReadLimitReached { requested: amount, available: self.0 });
        } else {
            self.0 -= amount;
            Ok(())
        }
    }

    /// Checks if read limit is higher-or-equal than the specified amount, or throws an error otherwise
    ///
    /// # Arguments
    ///
    /// * `amount`: Amount to take from the limit
    ///
    /// returns: Result<(), LoadError>
    pub fn peek(&mut self, amount: usize) -> Result<(), LoadError> {
        if self.0 < amount {
            panic!("{}", LoadError::ReadLimitReached { requested: amount, available: self.0 });
        } else {
            Ok(())
        }
    }
}

/// Trait for bytecode loaders
pub trait Loader {
    /// Binary signature for Lua bytecode; First few bytes expected in loaded chunk. Defaults to [`constants::LUA_SIGNATURE`] ("&lt;esc&gt;Lua")
    const LUA_SIGNATURE: &'static [u8] = constants::LUA_SIGNATURE;
    /// Lua version expected in header. Defaults to [`constants::LUA_VERSION`] (0x54)
    const LUA_VERSION: &'static [u8] = &[constants::LUA_VERSION];
    /// Lua format expected in header. Defaults to [`constants::LUA_FORMAT`] (0x00)
    const LUA_FORMAT: &'static [u8] = &[constants::LUA_FORMAT];
    /// Lua conversion test data. Defaults to [`constants::LUA_CONV_DATA`] ("\x19\x93\r\n\x1a\n")
    const LUA_CONV_DATA: &'static [u8] = constants::LUA_CONV_DATA;
    /// Lua system parameters. 3 bytes containing the size of LUA_INSTRUCTION, LUA_INT and LUA_FLOAT as parsed by this Loader
    const LUA_SYSTEM_PARAMETER: &'static [u8];
    /// Lua conversion test integer. Defaults to [`constants::LUA_CHECK_INTEGER`] (0x5678)
    const LUA_CHECK_INTEGER: LUA_INT = constants::LUA_CHECK_INTEGER;
    /// Lua conversion test float. Defaults to [`constants::LUA_CHECK_FLOAT`] (370.5)
    const LUA_CHECK_FLOAT: LUA_FLOAT = constants::LUA_CHECK_FLOAT;


    /// Reads a single usize
    ///
    /// A default implementation is provided as size values have a dynamic length in bytecode
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<usize, LoadError>
    ///
    fn read_host_usize<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<usize, LoadError> {
        let mut value: usize = 0;
        loop {
            let byte = Self::read_byte(reader, read_limit)?;
            value = value.checked_shl(7).ok_or(LoadError::CannotRepresentValue)?;
            value |= byte as usize & 0x7f;
            if byte & 0x80 != 0 { break; };
        }
        Ok(value)
    }

    /// Reads a single 'host int'; Usually 32-bit. (C's `int` type)
    ///
    /// As implementation varies per architecture, no default implementation is provided
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<HOST_INT, LoadError>
    ///
    fn read_host_int<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<HOST_INT, LoadError>;

    /// Reads a single Lua integer ([`LUA_INT`])
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<LUA_INT, LoadError>
    ///
    fn read_lua_int<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<LUA_INT, LoadError>;

    /// Reads a single Lua float ([`LUA_FLOAT`])
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<LUA_FLOAT, LoadError>
    ///
    fn read_lua_float<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<LUA_FLOAT, LoadError>;

    /// Reads a single Lua instruction ([`LUA_INSTRUCTION`])
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<LUA_INSTRUCTION, LoadError>
    ///
    fn read_lua_instruction<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<LUA_INSTRUCTION, LoadError>;

    /// Reads a single byte, checking memory limit before doing so
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<u8, LoadError>
    ///
    fn read_byte<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<u8, LoadError> {
        read_limit.take(1)?;
        let mut byte_buf = [0u8];
        reader.read_exact(&mut byte_buf)?;
        Ok(byte_buf[0])
    }

    /// Reads a single boolean, checking memory limit before doing so
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<bool, LoadError>
    ///
    fn read_boolean<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<bool, LoadError> {
        match Self::read_byte(reader, read_limit)? {
            0 => Ok(false),
            1 => Ok(true),
            byte => Err(LoadError::NonBinaryBoolean(byte))
        }
    }
    
    /// Reads a single 'nullable' [`LuaString`], read memory limit before doing so.
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<Option<LuaString>, LoadError>
    ///
    fn read_string<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<Option<LuaString>, LoadError> {
        let size = Self::read_host_usize(reader, read_limit)?;
        if size == 0 {
            Ok(None)
        } else {
            read_limit.take(size - 1)?;
            let mut str_buffer: Vec<u8> = vec![0u8; size - 1];
            reader.read_exact(&mut str_buffer[..])?;
            Ok(Some(LuaString::from(str_buffer)))
        }
    }
    
    /// Reads a single constant [`LuaValue`], checking read limit before doing so.
    ///
    /// Note: Not all LuaValue types can be loaded as constants (In default implementation: Userdata, Function, Thread or, Table cannot be loaded as a constant)
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<LuaValue, LoadError>
    ///
    fn read_constant<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<LuaValue, LoadError> {
        let constant_type = Self::read_byte(reader, read_limit)?;
        match constant_type {
            constants::typetag::TNIL => Ok(LuaValue::NIL),
            constants::typetag::VFALSE => Ok(LuaValue::BOOLEAN(false)),
            constants::typetag::VTRUE => Ok(LuaValue::BOOLEAN(true)),
            constants::typetag::VFLOAT => Ok(LuaValue::NUMBER(LuaNumber::FLOAT(Self::read_lua_float(reader, read_limit)?))),
            constants::typetag::VINTEGER => Ok(LuaValue::NUMBER(LuaNumber::INT(Self::read_lua_int(reader, read_limit)?))),
            constants::typetag::TSTRING | constants::typetag::VLONGSTRING => Ok(LuaValue::STRING(Self::read_string(reader, read_limit)?.ok_or(LoadError::NullStringInConstant)?)),
            _ => Err(LoadError::UnsupportedConstantTypeTag(constant_type))
        }
    }

    /// Reads a single upvalue description (See: [`UpvalueDesc`]), checking read limit before doing so.
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<UpvalueDesc, LoadError>
    ///
    fn read_upvalue_description<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<UpvalueDesc, LoadError> {
        Ok(UpvalueDesc::new(Self::read_byte(reader, read_limit)?, Self::read_byte(reader, read_limit)?, Self::read_byte(reader, read_limit)?))
    }

    /// Reads a single local variable description (See: [`LocalVariableInfo`]), checking read limit before doing so.
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<LocalVariableInfo, LoadError>
    ///
    fn read_local_variable<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<LocalVariableInfo, LoadError> {
        Ok(LocalVariableInfo {
            name: Self::read_string(reader, read_limit)?,
            startpc: Self::read_host_int(reader, read_limit)?,
            endpc: Self::read_host_int(reader, read_limit)?
        })
    }

    /// Reads a function and it's children, checking read limit before doing so.
    /// NOTE: Unless you are implementing [`Loader`], you likely want to use [`Loader::load_chunk`] instead
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<Rc<Prototype>, LoadError>
    ///
    fn read_prototype<R: Read>(reader: &mut R, upvalue_size: u8, name_override: Option<LuaString>, read_limit: &mut ReadLimit) -> Result<Rc<Prototype>, LoadError> {
        // Internal utility function
        #[inline(always)]
        fn read_vec<T, R: Read, F: Fn(&mut R, &mut ReadLimit) -> Result<T, LoadError>>(reader: &mut R, count: usize, read_fn: F, read_limit: &mut ReadLimit) -> Result<Vec<T>, LoadError> {
            let mut buf = Vec::new();   // We have the information to pre-allocate, but we cannot validate how many read bytes would be needed to fill the array and so malicious bytecode could cause a memory shortage TODO: Alternative safety limit?
            for _ in 0..count {
                buf.push(read_fn(reader, read_limit)?)
            }
            Ok(buf)
        }

        let source_string = Self::read_string(reader, read_limit)?;
        let first_line_defined = Self::read_host_int(reader, read_limit)?;
        let last_line_defined = Self::read_host_int(reader, read_limit)?;
        let param_count = Self::read_byte(reader, read_limit)?;
        let is_vararg = Self::read_byte(reader, read_limit)?;
        let max_stack_size = Self::read_byte(reader, read_limit)?;

        let code_count = Self::read_host_usize(reader, read_limit)?;
        let code = read_vec(reader, code_count, Self::read_lua_instruction, read_limit)?;

        let constant_count = Self::read_host_usize(reader, read_limit)?;
        let constants = read_vec(reader, constant_count, Self::read_constant, read_limit)?;

        let upvalue_count = Self::read_host_usize(reader, read_limit)?;
        let upvalue_descriptors = read_vec(reader, upvalue_count, Self::read_upvalue_description, read_limit)?;

        let function_count = Self::read_host_usize(reader, read_limit)?;
        let functions = read_vec(reader, function_count, |reader: &mut R, read_limit| {
            Self::read_prototype(reader, 0,None, read_limit)
        }, read_limit)?;

        // Debug info
        let lineinfo_count = Self::read_host_usize(reader, read_limit)?;
        let lineinfo = read_vec(reader, lineinfo_count, |reader: &mut R, read_limit| Self::read_byte(reader, read_limit).map(|byte| byte as HOST_SIGNED_BYTE), read_limit)?;

        let abslineinfo_count = Self::read_host_usize(reader, read_limit)?;
        let abslineinfo = read_vec(reader, abslineinfo_count, |reader, read_limit| Ok((Self::read_host_int(reader, read_limit)?, Self::read_host_int(reader, read_limit)?)), read_limit)?;

        let locvars_count = Self::read_host_usize(reader, read_limit)?;
        let localvariableinfo = read_vec(reader, locvars_count, Self::read_local_variable, read_limit)?;

        let debug_upvalue_count = Self::read_host_usize(reader, read_limit)?;
        if debug_upvalue_count != upvalue_count {
            return Err(LoadError::InvalidDebugInfo);
        }
        let upvaluenames = read_vec(reader, debug_upvalue_count, Self::read_string, read_limit)?;

        let proto = Rc::new(Prototype {    // TODO: Parent
            upvalue_size,
            origin_string: name_override.or(source_string),
            first_line_defined,
            last_line_defined,
            param_count,
            is_vararg,
            max_stack_size,
            code,
            constants,
            upvalue_descriptors,
            functions,
            lineinfo,
            abslineinfo,
            localvariableinfo,
            upvaluenames,
            parent: RefCell::new(None)
        });
        for child in &proto.functions {
            child.parent.borrow_mut().replace(Rc::downgrade(&proto));
        };
        Ok(proto)
    }

    /// Reads and checks the Lua bytecode header
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<(), LoadError>
    ///
    fn read_header<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<(), LoadError> {
        fn check_array<R: Read>(reader: &mut R, array: &[u8], error_mapper: fn(Vec<u8>) -> LoadError, read_limit: &mut ReadLimit) -> Result<(), LoadError> {
            read_limit.take(array.len())?;
            let mut buffer = vec![0u8; array.len()];
            reader.read_exact(&mut buffer)?;
            if &buffer != array {
                Err(error_mapper(buffer))
            } else {
                Ok(())
            }
        }

        check_array(reader, Self::LUA_SIGNATURE, |found| LoadError::InvalidLuaSignature { found, expected: Self::LUA_SIGNATURE }, read_limit)?;

        let version = Self::read_byte(reader, read_limit)?;
        let format = Self::read_byte(reader, read_limit)?;
        if version != constants::LUA_VERSION {
            return Err(LoadError::InvalidVersion(version));
        }
        if format != constants::LUA_FORMAT {
            return Err(LoadError::InvalidFormat(format));
        }

        check_array(reader, Self::LUA_CONV_DATA, |found| LoadError::ConversionDataCorrupt { found, expected: Self::LUA_CONV_DATA }, read_limit)?;
        check_array(reader, Self::LUA_SYSTEM_PARAMETER, |found| LoadError::IncompatibleSystemParam { found, expected: Self::LUA_SYSTEM_PARAMETER }, read_limit)?;

        let integer = Self::read_lua_int(reader, read_limit)?;
        if integer != constants::LUA_CHECK_INTEGER {
            return Err(LoadError::CorruptCheckInt(integer));
        }

        let float = Self::read_lua_float(reader, read_limit)?;
        if float != constants::LUA_CHECK_FLOAT {
            return Err(LoadError::CorruptCheckFloat(float));
        }

        Ok(())
    }

    /// Reads a full Lua bytecode chunk/file, returning it as a function
    ///
    /// # Arguments
    ///
    /// * `reader`: Reader to use
    /// * `read_limit`: Limit on total amount of bytes to read
    ///
    /// returns: Result<Rc<Prototype>, LoadError>
    ///
    fn load_chunk<R: Read>(reader: &mut R, name_override: Option<LuaString>, read_limit: usize) -> Result<Rc<Prototype>, LoadError> {
        let mut read_limit = ReadLimit(read_limit);
        Self::read_header(reader, &mut read_limit)?;
        let upvalue_size = Self::read_byte(reader, &mut read_limit)?;
        let function = Self::read_prototype(reader, upvalue_size, name_override: Option<LuaString>, &mut read_limit)?;
        Ok(function)
    }
}

/// Struct providing default implementation for [`Loader`] which is compatible with Lua 5.4 on little-endian 64-bit architectures. (x86/ARM64)
///
/// Uses i64 for LUA_INT, f64 for LUA_FLOAT, and u32 for LUA_INSTRUCTION
///
pub struct LE64Loader {}
impl Loader for LE64Loader {
    const LUA_SYSTEM_PARAMETER: &'static [u8] = &[
        mem::size_of::<u32>() as u8,    // Instruction size in bytes
        mem::size_of::<i64>() as u8,    // Integer size in bytes
        mem::size_of::<f64>() as u8     // Float size in bytes
    ];

    /// Wrapper around read_host_usize
    fn read_host_int<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<HOST_INT, LoadError> {
        HOST_INT::try_from(Self::read_host_usize(reader, read_limit)? as isize).map_err(|_| LoadError::CannotRepresentValue)
    }

    fn read_lua_int<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<LUA_INT, LoadError> {
        read_limit.take( mem::size_of::<i64>())?;
        let mut int_buf = [0u8; mem::size_of::<i64>()];
        reader.read_exact(&mut int_buf)?;
        let integer = LUA_INT::try_from(i64::from_le_bytes(int_buf)).map_err(|_| LoadError::CannotRepresentValue)?;
        Ok(integer)
    }

    fn read_lua_float<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<LUA_FLOAT, LoadError> {
        read_limit.take( mem::size_of::<f64>())?;
        let mut float_buf = [0u8; mem::size_of::<f64>()];
        reader.read_exact(&mut float_buf)?;
        let float = LUA_FLOAT::try_from(f64::from_le_bytes(float_buf)).map_err(|_| LoadError::CannotRepresentValue)?;
        Ok(float)
    }

    fn read_lua_instruction<R: Read>(reader: &mut R, read_limit: &mut ReadLimit) -> Result<LUA_INSTRUCTION, LoadError> {
        read_limit.take( mem::size_of::<u32>())?;
        let mut integer_buf = [0u8; mem::size_of::<u32>()];
        reader.read_exact(&mut integer_buf)?;
        let instruction = LUA_INSTRUCTION { inner: u32::from_le_bytes(integer_buf) };
        Ok(instruction)
    }
}

impl LE64Loader {
    /// Explains bytecode parsing. Intended for debugging/testing [`LE64Loader`] compatible bytecode.
    ///
    /// Returns Vector of (index of start of byte slice, kind of object/value read, slice representing object, value of object read)
    /// # Arguments
    ///
    /// * `bytecode`: Bytecode to explain
    ///
    /// returns: Result<Vec<(usize, &str, &[u8], String), Global>, Error>
    ///
    #[cfg(debug_assertions)]
    pub fn explain(bytecode: &[u8]) -> Result<Vec<(usize, &'static str, &[u8], String)>, io::Error> {
        let read_limit = &mut ReadLimit(bytecode.len());

        let mut index = 0;
        let mut explanation = Vec::new();
        let mut cursor = Cursor::new(bytecode);

        macro_rules! explain_call {
            ($call:expr, $message:expr) => {
                match $call {
                    Ok(val) => {
                        explanation.push((index, $message, &bytecode[index..cursor.position() as usize], format!("{:?}", val)));
                        index = cursor.position() as usize;
                        val
                    },
                    Err(LoadError::IO(io_error)) => { return Err(io_error) }
                    Err(_) => {
                        explanation.push((index, concat!("Expected ", $message), &bytecode[index..cursor.position() as usize], "N/A".to_string()));
                        return Ok(explanation);
                    }
                }
            };
        }

        fn check_array<R: Read>(reader: &mut R, array: &[u8], error_mapper: fn(Vec<u8>) -> LoadError, read_limit: &mut ReadLimit) -> Result<(), LoadError> {
            let mut buffer = vec![0u8; array.len()];
            reader.read_exact(&mut buffer)?;
            if &buffer != array {
                Err(error_mapper(buffer))
            } else {
                Ok(())
            }
        }

        explain_call!(check_array(&mut cursor, Self::LUA_SIGNATURE, |found| LoadError::InvalidLuaSignature { found, expected: Self::LUA_SIGNATURE }, read_limit), "Lua Signature");
        explain_call!(Self::read_byte(&mut cursor, read_limit).and_then(|version| if version != constants::LUA_VERSION { Err(LoadError::InvalidVersion(version)) } else { Ok(()) }), "Lua version");
        explain_call!(Self::read_byte(&mut cursor, read_limit).and_then(|format| if format != constants::LUA_FORMAT { Err(LoadError::InvalidFormat(format)) } else { Ok(()) }), "Lua format");
        explain_call!(check_array(&mut cursor, Self::LUA_CONV_DATA, |found| LoadError::ConversionDataCorrupt { found, expected: Self::LUA_CONV_DATA }, read_limit), "Conversion data");
        explain_call!(check_array(&mut cursor, Self::LUA_SYSTEM_PARAMETER, |found| LoadError::IncompatibleSystemParam { found, expected: Self::LUA_SYSTEM_PARAMETER }, read_limit), "System parameters");
        explain_call!(Self::read_lua_int(&mut cursor, read_limit).and_then(|integer| if integer != constants::LUA_CHECK_INTEGER { Err(LoadError::CorruptCheckInt(integer)) } else { Ok(()) }), "Check integer");
        explain_call!(Self::read_lua_float(&mut cursor, read_limit).and_then(|float| if float != constants::LUA_CHECK_FLOAT{ Err(LoadError::CorruptCheckFloat(float)) } else { Ok(()) }), "Check integer");
        explain_call!(Self::read_byte(&mut cursor, read_limit), "Upvalue size");

        macro_rules! explain_vec {
            ($reader:expr, $count:expr, $read_fn:expr, $message:expr, $mem_limit:expr) => {
                for _ in 0..$count {
                    match ($read_fn($reader, $mem_limit)) {
                        Ok(val) => {
                            explanation.push((index, concat!( "\t", $message ), &bytecode[index..cursor.position() as usize], format!("{:?}", val)));
                            index = cursor.position() as usize;
                        }
                        Err(LoadError::IO(io_error)) => { return Err(io_error); }
                        Err(_) => {
                            explanation.push((index, concat!( "\tExpected ", $message ), &bytecode[index..cursor.position() as usize], "N/A".to_string()));
                            return Ok(explanation);
                        }
                    }
                }
            }
        }

        explain_call!(Self::read_string(&mut cursor, read_limit), "Source string");
        explain_call!(Self::read_host_int(&mut cursor, read_limit), "First line defined");
        explain_call!(Self::read_host_int(&mut cursor, read_limit), "Last line defined");
        explain_call!(Self::read_byte(&mut cursor, read_limit), "Parameter count");
        explain_call!(Self::read_byte(&mut cursor, read_limit), "Is vararg");
        explain_call!(Self::read_byte(&mut cursor, read_limit), "Max stacksize");

        let code_count = explain_call!(Self::read_host_usize(&mut cursor, read_limit), "Code count");
        explain_vec!(&mut cursor, code_count, Self::read_lua_instruction, "Instruction", read_limit);

        let constant_count = explain_call!(Self::read_host_usize(&mut cursor, read_limit), "Constant count");
        explain_vec!(&mut cursor, constant_count, Self::read_constant, "Constant", read_limit);

        let upvalue_count = explain_call!(Self::read_host_usize(&mut cursor, read_limit), "Upvalue description count");
        explain_vec!(&mut cursor, upvalue_count, Self::read_upvalue_description, "Upvalue description", read_limit);

        let function_count = explain_call!(Self::read_host_usize(&mut cursor, read_limit), "Function count");
        explain_vec!(&mut cursor, function_count, |cursor, read_limit| Self::read_prototype(cursor, 0, None, read_limit), "Function", read_limit); // TODO: Nested functions

        let lineinfo_count = explain_call!(Self::read_host_usize(&mut cursor, read_limit), "Lineinfo count");
        explain_vec!(&mut cursor, lineinfo_count, Self::read_byte, "Lineinfo", read_limit);

        let abslineinfo_count = explain_call!(Self::read_host_usize(&mut cursor, read_limit), "Absolute lineinfo count");
        explain_vec!(&mut cursor, abslineinfo_count, |mut cursor, read_limit: &mut ReadLimit| {
            Ok((Self::read_host_int(&mut cursor, &mut *read_limit)?, Self::read_host_int(&mut cursor, &mut *read_limit)?))
        }, "Absolute lineinfo", read_limit);

        let locvars_count = explain_call!(Self::read_host_usize(&mut cursor, read_limit), "Local variable count");
        explain_vec!(&mut cursor, locvars_count, Self::read_local_variable, "Local variable", read_limit);

        let debug_upvalue_count = explain_call!(Self::read_host_usize(&mut cursor, read_limit), "Upvalue debug name count");
        explain_vec!(&mut cursor, debug_upvalue_count, Self::read_string, "Upvalue debug name", read_limit);

        Ok(explanation)
    }
}