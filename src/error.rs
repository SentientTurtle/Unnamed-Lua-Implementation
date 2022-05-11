//! Module containing main Error types

use crate::constants::types::{LUA_INT};
use std::rc::Rc;
use std::fmt::{Formatter, Debug, Display};
use std::{fmt, io};
use std::error::Error;
use crate::bytecode::loader::LoadError;
use crate::types::value::LuaValue;
use crate::types::value::function::{Prototype, RustFunction};
use crate::constants;
use crate::types::value::string::LuaString;

/// Stacktrace entry
pub enum TraceEntry {
    Lua { program_counter: usize, prototype: Rc<Prototype> },
    Rust(RustFunction),
    RustClosure(&'static str),
    /// Tail call stacktrace entries keep track of the amount of tail calls made; One entry is made for multiple repeated tail calls to prevent stacktrace overflow with heavy tailcall recursion
    TailCall(usize),
}

impl Display for TraceEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TraceEntry::Lua { program_counter, prototype} => {
                match (&prototype.origin_string, prototype.get_line(*program_counter), prototype.first_line_defined, prototype.last_line_defined) {
                    (Some(name), Some(line), 0, 0) => {
                        write!(f, "Lua \"{}\" @ ln {}", name, line)
                    },
                    (Some(name), None, 0, 0) => {
                        write!(f, "Lua \"{}\"", name)
                    },
                    (Some(name), Some(line), fld, lld) => {
                        write!(f, "Lua \"{}\" ({}-{}) @ ln {}", name, fld, lld, line)
                    },
                    (Some(name), None, fld, lld) => {
                        write!(f, "Lua \"{}\" ({}-{})", name, fld, lld)
                    },
                    (None, Some(line), _, _) => {
                        write!(f, "Anonymous Lua function @ ln {}", line)
                    }
                    (None, None, _, _) => {
                        write!(f, "Anonymous Lua function")
                    }
                }
            }
            TraceEntry::Rust(func) => write!(f, "Rust \"{}\"", func.name()),
            TraceEntry::RustClosure(name) => write!(f, "Rust closure \"{}\"", name),
            TraceEntry::TailCall(amount) => write!(f, "Tailcall({} times)", amount),
        }
    }
}

impl Debug for TraceEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TraceEntry::Lua { program_counter, prototype} => {
                let mut debug = f.debug_struct("TraceElement::Lua");
                if let Some(line) = prototype.get_line(*program_counter) {
                    debug.field("line", &line);
                }
                debug.field("program_counter", program_counter);
                if let Some(instruction) = prototype.code.get(*program_counter) {
                    debug.field("opcode", &instruction.opcode_name());
                }
                debug.field("prototype", prototype)
                    .finish()
            }
            TraceEntry::Rust(func) => f.debug_tuple("TraceElement::Rust").field(&func.name()).field(&(func as *const RustFunction)).finish(),
            TraceEntry::RustClosure(name) => f.debug_tuple("TraceElement::RustClosure").field(name).finish(),
            TraceEntry::TailCall(amount) => f.debug_tuple("TraceElement::TailCall").field(amount).finish(),
        }
    }
}

/// Top level type for Lua Errors, wrapping an error and stack trace (built on stack-unwind)
///
/// If the cargo feature "os-env-exit" is disabled, Lua's os.exit throws a "Exit Process" quasi-error. This error should not be caught and dropped. [`LuaError::is_exit`] returns true for this error type
///
/// When catching and throwing a different error, [`LuaError::map`] should be used to retain stacktrace
///
/// Copying this error is expensive with large stack traces, and should be avoided
#[derive(Debug)]    // TODO: Add clone once basic interpreter implementation is complete
pub struct LuaError {
    error: LuaErrorKind,
    stacktrace: Vec<TraceEntry>
}
impl LuaError {
    /// Creates new LuaError wrapping a [`GenericError`] with a &'static str message
    ///
    /// # Arguments
    ///
    /// * `message`: Message of error
    ///
    /// returns: LuaError
    pub fn new(message: &'static str) -> Self {
        LuaError {
            error: LuaErrorKind::GenericError(GenericError::Str(message)),
            stacktrace: Vec::new()
        }
    }

    /// Creates new LuaError wrapping a [`GenericError`] with a String message
    ///
    /// # Arguments
    ///
    /// * `message`: Message of error
    ///
    /// returns: LuaError
    pub fn with_string(message: String) -> LuaError {
        LuaError {
            error: LuaErrorKind::GenericError(GenericError::String(message)),
            stacktrace: Vec::new()
        }
    }

    /// Creates new LuaError wrapping a [`GenericError`] with a LuaValue message
    ///
    /// # Arguments
    ///
    /// * `message`: Message of error
    ///
    /// returns: LuaError
    pub fn with_message<T: Into<LuaValue>>(message: T) -> Self {
        LuaError {
            error: LuaErrorKind::GenericError(GenericError::Value(message.into())),
            stacktrace: Vec::new()
        }
    }

    /// Replaces the cause of this LuaError without changing it's stacktrace
    ///
    /// # Arguments
    ///
    /// * `new_error`: New error for this LuaError
    ///
    /// returns: LuaError
    pub fn map<F: FnOnce(LuaValue) -> T, T: Into<LuaErrorKind>>(mut self, new_error: F) -> Self {
        self.error = new_error(self.error.message()).into();
        self
    }

    /// Creates "Exit Process" quasi-error, signalling that the Lua VM's shutdown is requested
    ///
    /// This error is not caught by (x)pcall, though it is not guaranteed the VM or host process will shut down if this error is thrown.
    ///
    /// # Arguments
    ///
    /// * `exit_code`: Exit code to be shown if process is shut down
    ///
    /// returns: LuaError
    pub fn exit_process(exit_code: LUA_INT) -> LuaError {
        LuaError {
            error: LuaErrorKind::ExitProcess { exit_code },
            stacktrace: Vec::new()
        }
    }

    /// Returns true if this error is the "Exit Process" quasi-error
    pub fn is_exit(&self) -> bool {
        match self.error {
            LuaErrorKind::ExitProcess { .. } => true,
            _ => false
        }
    }

    /// Returns message of this error, as LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] instead
    pub fn message(self) -> LuaValue {
        self.error.message()
    }

    /// Appends a rust function call to the stacktrace
    ///
    /// # Arguments
    ///
    /// * `caller`: Rust function which has errored
    ///
    /// returns: LuaError
    pub fn trace_native(mut self, caller: RustFunction) -> LuaError {
        self.stacktrace.push(TraceEntry::Rust(caller));
        self
    }

    /// Appends a rust closure call to the stacktrace
    ///
    /// # Arguments
    ///
    /// * `caller`: Name of rust closure which has errored
    ///
    /// returns: LuaError
    pub fn trace_closure(mut self, caller: &'static str) -> LuaError {
        self.stacktrace.push(TraceEntry::RustClosure(caller));
        self
    }

    /// Appends a Lua function call to stack trace
    ///
    /// # Arguments
    ///
    /// * `program_counter`: Program counter at which Lua function errored
    /// * `prototype`: Lua function that errored
    ///
    /// returns: LuaError
    pub fn trace_lua(mut self, program_counter: usize, prototype: Rc<Prototype>) -> LuaError {
        self.stacktrace.push(TraceEntry::Lua { program_counter, prototype });
        self
    }

    /// Appends a lua tailcall to the stacktrace
    ///
    /// If the previous entry on the stacktrace was also a tailcall, increments it's call count
    ///
    /// returns: LuaError
    pub fn trace_tail_call(mut self) -> LuaError {
        match self.stacktrace.last_mut() {
            Some(TraceEntry::TailCall(count)) => *count = *count + 1,
            Some(_) | None => self.stacktrace.push(TraceEntry::TailCall(1)),
        }
        self
    }

    /// Returns a struct that implements Display for this error's stack trace
    pub fn display_stacktrace(&self) -> LuaErrorStackTraceDisplay {
        LuaErrorStackTraceDisplay {
            inner: self
        }
    }
}

impl Error for LuaError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.error {
            LuaErrorKind::ArgumentError(err) => Some(err),
            LuaErrorKind::ByteCodeError(err) => Some(err),
            LuaErrorKind::CompileError(err) => Some(err),
            LuaErrorKind::DecodeError(err) => Some(err),
            LuaErrorKind::GenericError(err) => Some(err),
            LuaErrorKind::CannotCoerce(err) => Some(err),
            LuaErrorKind::CannotCompare(err) => Some(err),
            LuaErrorKind::InvalidValue(err) => Some(err),
            LuaErrorKind::InvalidTableKey(err) => Some(err),
            LuaErrorKind::CannotIndexType(err) => Some(err),
            LuaErrorKind::StringIndexOutOfRange(err) => Some(err),
            LuaErrorKind::AttemptToCallNonFunction(err) => Some(err),
            LuaErrorKind::InvalidConcatenation(err) => Some(err),
            LuaErrorKind::ExitProcess { .. } => None
        }
    }
}

impl Display for LuaError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.error)
    }
}

/// Struct that implements display for a LuaError's stacktrace
pub struct LuaErrorStackTraceDisplay<'a> {
    inner: &'a LuaError
}

impl<'a> Display for LuaErrorStackTraceDisplay<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner.error)?;
        for entry in &self.inner.stacktrace {
            write!(f, "\n\t{}", entry)?;
        }
        Ok(())
    }
}

impl<T: Into<LuaErrorKind>> From<T> for LuaError {
    fn from(error: T) -> Self {
        LuaError {
            error: error.into(),
            stacktrace: Vec::with_capacity(0)
        }
    }
}

/// Errorkind enum for LuaError, see documentation on [`LuaError`] and specific error types for details
#[derive(Debug)]
pub enum LuaErrorKind {
    ArgumentError(ArgumentError),
    ByteCodeError(ByteCodeError),
    CompileError(CompileError),
    DecodeError(LoadError),
    GenericError(GenericError),
    CannotCoerce(CannotCoerceError),
    CannotCompare(CannotCompareError),
    InvalidValue(InvalidValueError),
    InvalidTableKey(InvalidKeyError),
    CannotIndexType(CannotIndexTypeError),
    StringIndexOutOfRange(StringIndexOutOfRangeError),
    AttemptToCallNonFunction(AttemptToCallNonFunctionError),
    InvalidConcatenation(InvalidConcatenationError),
    /// Exit Process "quasi-error"; Signals that shutdown of LuaVM/host process was requested
    ExitProcess { exit_code: LUA_INT }
}

impl LuaErrorKind {
    /// Returns message of this error, as LuaValue
    ///
    /// Implementation for [`LuaError::message`]
    fn message(self) -> LuaValue {
        match self {
            LuaErrorKind::ArgumentError(err) => err.message(),
            LuaErrorKind::ByteCodeError(err) => err.message(),
            LuaErrorKind::CompileError(err) => err.message(),
            LuaErrorKind::DecodeError(err) => err.lua_message(),
            LuaErrorKind::GenericError(err) => err.message(),
            LuaErrorKind::CannotCoerce(err) => err.message(),
            LuaErrorKind::CannotCompare(err) => err.message(),
            LuaErrorKind::InvalidValue(err) => err.message(),
            LuaErrorKind::InvalidTableKey(err) => err.message(),
            LuaErrorKind::CannotIndexType(err) => err.message(),
            LuaErrorKind::AttemptToCallNonFunction(err) => err.message(),
            LuaErrorKind::InvalidConcatenation(err) => err.message(),
            LuaErrorKind::StringIndexOutOfRange(err) => err.message(),
            LuaErrorKind::ExitProcess { exit_code } => LuaValue::from(format!("Attempt to exit process ({})", exit_code))
        }
    }
}

impl Display for LuaErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LuaErrorKind::ArgumentError(error) => write!(f, "{}", error),
            LuaErrorKind::ByteCodeError(error) => write!(f, "{}", error),
            LuaErrorKind::CompileError(error) => write!(f, "{}", error),
            LuaErrorKind::DecodeError(error) => write!(f, "{}", error),
            LuaErrorKind::GenericError(error) => write!(f, "{}", error),
            LuaErrorKind::CannotCoerce(error) => write!(f, "{}", error),
            LuaErrorKind::CannotCompare(error) => write!(f, "{}", error),
            LuaErrorKind::InvalidValue(error) => write!(f, "{}", error),
            LuaErrorKind::InvalidTableKey(error) => write!(f, "{}", error),
            LuaErrorKind::CannotIndexType(error) => write!(f, "{}", error),
            LuaErrorKind::AttemptToCallNonFunction(error) => write!(f, "{}", error),
            LuaErrorKind::InvalidConcatenation(error) => write!(f, "{}", error),
            LuaErrorKind::StringIndexOutOfRange(error) => write!(f, "{}", error),
            LuaErrorKind::ExitProcess { exit_code }  => write!(f, "Attempt to exit process ({})", exit_code)
        }
    }
}

impl From<ArgumentError> for LuaErrorKind {
    fn from(e: ArgumentError) -> Self { LuaErrorKind::ArgumentError(e) }
}

impl From<ByteCodeError> for LuaErrorKind {
    fn from(e: ByteCodeError) -> Self { LuaErrorKind::ByteCodeError(e) }
}

impl From<CompileError> for LuaErrorKind {
    fn from(e: CompileError) -> Self {
        LuaErrorKind::CompileError(e)
    }
}

impl From<LoadError> for LuaErrorKind {
    fn from(e: LoadError) -> Self {
        LuaErrorKind::DecodeError(e)
    }
}

impl From<GenericError> for LuaErrorKind {
    fn from(e: GenericError) -> Self {
        LuaErrorKind::GenericError(e)
    }
}

impl From<CannotCoerceError> for LuaErrorKind {
    fn from(e: CannotCoerceError) -> Self {
        LuaErrorKind::CannotCoerce(e)
    }
}

impl From<CannotCompareError> for LuaErrorKind {
    fn from(e: CannotCompareError) -> Self {
        LuaErrorKind::CannotCompare(e)
    }
}

impl From<InvalidValueError> for LuaErrorKind {
    fn from(e: InvalidValueError) -> Self {
        LuaErrorKind::InvalidValue(e)
    }
}

impl From<InvalidIndexationError> for LuaErrorKind {
    fn from(e: InvalidIndexationError) -> Self {
        match e {
            InvalidIndexationError::InvalidKey(error) => LuaErrorKind::InvalidTableKey(error),
            InvalidIndexationError::CannotIndexType(error) => LuaErrorKind::CannotIndexType(error)
        }
    }
}

impl From<CannotIndexTypeError> for LuaErrorKind {
    fn from(e: CannotIndexTypeError) -> Self {
        LuaErrorKind::CannotIndexType(e)
    }
}

impl From<InvalidKeyError> for LuaErrorKind {
    fn from(e: InvalidKeyError) -> Self {
        LuaErrorKind::InvalidTableKey(e)
    }
}

impl From<StringIndexOutOfRangeError> for LuaErrorKind {
    fn from(e: StringIndexOutOfRangeError) -> Self {
        LuaErrorKind::StringIndexOutOfRange(e)
    }
}

impl From<AttemptToCallNonFunctionError> for LuaErrorKind {
    fn from(e: AttemptToCallNonFunctionError) -> Self {
        LuaErrorKind::AttemptToCallNonFunction(e)
    }
}

impl From<InvalidConcatenationError> for LuaErrorKind {
    fn from(e: InvalidConcatenationError) -> Self {
        LuaErrorKind::InvalidConcatenation(e)
    }
}

/// Argument-related error; Wraps another error kind and an argument index (starting at 0)
///
/// For adding argument index information to existing errors, the [`IntoArgumentError`] extension trait is provided
#[derive(Debug)]
pub enum ArgumentError {
    InvalidArgument { message: &'static str, argument_index: usize },
    GenericError { error: GenericError, argument_index: usize },
    CannotCoerce { error: CannotCoerceError, argument_index: usize },
    CannotCompare { error: CannotCompareError, argument_index: usize },
    InvalidValue { error: InvalidValueError, argument_index: usize },
    InvalidTableKey { error: InvalidKeyError, argument_index: usize },
    CannotIndexType { error: CannotIndexTypeError, argument_index: usize },
    StringIndexOutOfRange { error: StringIndexOutOfRangeError, argument_index: usize },
    AttemptToCallNonFunction { error: AttemptToCallNonFunctionError, argument_index: usize },
    InvalidConcatenation { error: InvalidConcatenationError, argument_index: usize },
}

impl ArgumentError {
    /// Wraps an error with argument information
    ///
    /// # Arguments
    ///
    /// * `error`: Error to wrap
    /// * `argument_index`: Argument index (starting at 0) which caused the error
    ///
    /// returns: ArgumentError
    pub fn new<T: IntoArgumentError<Out = ArgumentError>>(error: T, argument_index: usize) -> ArgumentError {
        error.error_for_argument(argument_index)
    }

    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation instead
    pub fn message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl Error for ArgumentError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ArgumentError::InvalidArgument { .. } => None,
            ArgumentError::GenericError { error, .. } => Some(error),
            ArgumentError::CannotCoerce { error, .. } => Some(error),
            ArgumentError::CannotCompare { error, .. } => Some(error),
            ArgumentError::InvalidValue { error, .. } => Some(error),
            ArgumentError::InvalidTableKey { error, .. } => Some(error),
            ArgumentError::CannotIndexType { error, .. } => Some(error),
            ArgumentError::StringIndexOutOfRange { error, .. } => Some(error),
            ArgumentError::AttemptToCallNonFunction { error, .. } => Some(error),
            ArgumentError::InvalidConcatenation { error, .. } => Some(error),
        }
    }
}

impl Display for ArgumentError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ArgumentError::InvalidArgument { message, argument_index } => write!(f, "bad argument #{} ({})", argument_index + 1, message),
            ArgumentError::GenericError { error, argument_index } => write!(f, "bad argument #{} ({})", argument_index + 1, error),
            ArgumentError::CannotCoerce { error, argument_index } => write!(f, "bad argument #{} ({})", argument_index + 1, error),
            ArgumentError::CannotCompare { error, argument_index } => write!(f, "bad argument #{} ({})", argument_index + 1, error),
            ArgumentError::InvalidValue { error, argument_index } => write!(f, "bad argument #{} ({})", argument_index + 1, error),
            ArgumentError::CannotIndexType { error, argument_index } => write!(f, "bad argument #{} ({})", argument_index + 1, error),
            ArgumentError::AttemptToCallNonFunction { error, argument_index } => write!(f, "bad argument #{} ({})", argument_index + 1, error),
            ArgumentError::InvalidConcatenation { error, argument_index } => write!(f, "bad argument #{} ({})", argument_index + 1, error),
            ArgumentError::InvalidTableKey { error, argument_index } => write!(f, "bad argument #{} ({})", argument_index + 1, error),
            ArgumentError::StringIndexOutOfRange { error, argument_index } => write!(f, "bad argument #{} ({})", argument_index + 1, error),
        }
    }
}

/// Extension trait for [`ArgumentError`] conversion
///
/// [`IntoArgumentError::error_for_argument`] may be directly called on either error types [`Result`]s containing valid error types
pub trait IntoArgumentError {
    type Out;

    /// Wraps this error with argument information
    ///
    /// # Arguments
    ///
    /// * `argument_index`: Argument index (starting at 0) which caused the error
    ///
    /// returns: ArgumentError
    fn error_for_argument(self, argument_index: usize) -> Self::Out;
}

impl<T, E: IntoArgumentError> IntoArgumentError for Result<T, E> where E: IntoArgumentError<Out = ArgumentError> {
    type Out = Result<T, ArgumentError>;

    fn error_for_argument(self, argument_index: usize) -> Result<T, ArgumentError> {
        match self {
            Ok(val) => Ok(val),
            Err(err) => Err(err.error_for_argument(argument_index))
        }
    }
}

/// Generic error type, wrapping a text message or lua value
#[derive(Debug)]
pub enum GenericError {
    String(String),
    Str(&'static str),
    Value(LuaValue)
}

impl GenericError {
    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation or matching on GenericError variants instead
    pub fn message(self) -> LuaValue {
        match self {
            GenericError::Value(value) => value,
            _ => LuaValue::STRING(LuaString::from(format!("{}", self)))
        }
    }
}

impl Error for GenericError {}

impl IntoArgumentError for GenericError {
    type Out = ArgumentError;

    fn error_for_argument(self, argument_index: usize) -> Self::Out {
        ArgumentError::GenericError { error: self, argument_index }
    }
}

impl Display for GenericError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GenericError::String(message) => write!(f, "{}", message),
            GenericError::Str(message) => write!(f, "{}", message),
            GenericError::Value(message) => write!(f, "{}", message),
        }
    }
}

/// Error denoting type coercion error
///
/// See <https://www.lua.org/manual/5.4/manual.html#3.4.3> for details on Lua coercion
#[derive(Debug)]
pub struct CannotCoerceError {
    /// Original type of coerced value
    pub from: &'static str,
    /// Target type value could not be coerced to
    pub to: &'static str,
}

impl CannotCoerceError {
    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation instead
    pub fn message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl Error for CannotCoerceError {}

impl IntoArgumentError for CannotCoerceError {
    type Out = ArgumentError;

    fn error_for_argument(self, argument_index: usize) -> Self::Out {
        ArgumentError::CannotCoerce { error: self, argument_index }
    }
}

impl Display for CannotCoerceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "cannot coerce '{}' to '{}'", self.from, self.to)
    }
}

/// Error denoting types cannot be (order-)compared to each other
///
/// NOTE: Standard equality-comparison (==, ~=) between all types is valid and will not throw this error,
/// however, metatables may provide custom comparison overloading that use this error type.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#3.4.4> for details on Lua comparisons
#[derive(Debug)]
pub struct CannotCompareError {
    /// Type of left-hand-side value in comparison
    pub lhs_type: &'static str,
    /// Type of right-hand-side value in comparison
    pub rhs_type: &'static str,
}

impl CannotCompareError {
    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation instead
    pub fn message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl Error for CannotCompareError {}

impl IntoArgumentError for CannotCompareError {
    type Out = ArgumentError;

    fn error_for_argument(self, argument_index: usize) -> Self::Out {
        ArgumentError::CannotCompare { error: self, argument_index }
    }
}

impl Display for CannotCompareError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "cannot compare '{}' to '{}'", self.lhs_type, self.rhs_type)
    }
}

/// Error denoting invalid value was passed
///
/// NOTE: Most functions will attempt to coerce the passed value to the expected type, and accept any value that can be coerced into the required type
#[derive(Debug)]
pub struct InvalidValueError {
    /// Expected value type (or other description)
    pub expected: &'static str,
    /// Found value type
    pub found: &'static str,
}

impl InvalidValueError {
    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation instead
    pub fn message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl Error for InvalidValueError {}

impl IntoArgumentError for InvalidValueError {
    type Out = ArgumentError;

    fn error_for_argument(self, argument_index: usize) -> Self::Out {
        ArgumentError::InvalidValue { error: self, argument_index }
    }
}

impl Display for InvalidValueError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "invalid type; Expected '{}' found '{}'", self.expected, self.found)
    }
}

/// Error denoting invalid type indexation or table access
#[derive(Debug)]
pub enum InvalidIndexationError {
    /// The key used for indexing is either nil or NaN
    InvalidKey(InvalidKeyError),
    /// The indexed type is not a table, string, or userdata with __index/__newindex metatable set
    CannotIndexType(CannotIndexTypeError)
}

impl InvalidIndexationError {
    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation instead
    pub fn message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl Error for InvalidIndexationError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            InvalidIndexationError::InvalidKey(err) => Some(err),
            InvalidIndexationError::CannotIndexType(err) => Some(err)
        }
    }
}

impl IntoArgumentError for InvalidIndexationError {
    type Out = ArgumentError;

    fn error_for_argument(self, argument_index: usize) -> Self::Out {
        match self {
            InvalidIndexationError::InvalidKey(error) => ArgumentError::InvalidTableKey { error, argument_index },
            InvalidIndexationError::CannotIndexType(error) => ArgumentError::CannotIndexType { error, argument_index }
        }
    }
}

impl Display for InvalidIndexationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InvalidIndexationError::InvalidKey(error) => write!(f, "{}", error),
            InvalidIndexationError::CannotIndexType(error) => write!(f, "{}", error)
        }
    }
}

impl From<InvalidKeyError> for InvalidIndexationError {
    fn from(error: InvalidKeyError) -> Self {
        InvalidIndexationError::InvalidKey(error)
    }
}

impl From<CannotIndexTypeError> for InvalidIndexationError {
    fn from(error: CannotIndexTypeError) -> Self {
        InvalidIndexationError::CannotIndexType(error)
    }
}

/// Error denoting key is invalid; nil or NaN
#[derive(Debug)]
pub enum InvalidKeyError {
    KeyIsNil,
    KeyIsNaN
}

impl InvalidKeyError {
    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation instead
    pub fn message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl Error for InvalidKeyError {}

impl IntoArgumentError for InvalidKeyError {
    type Out = ArgumentError;

    fn error_for_argument(self, argument_index: usize) -> Self::Out {
        ArgumentError::InvalidTableKey { error: self, argument_index }
    }
}

impl Display for InvalidKeyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InvalidKeyError::KeyIsNil => write!(f, "table key is nil"),
            InvalidKeyError::KeyIsNaN => write!(f, "table key is NaN"),
        }
    }
}

/// Error denoting type cannot be indexed
///
/// Lua permits tables and strings to be indexed, userdata may permit indexing via __index/__newindex metamethods
#[derive(Debug)]
pub struct CannotIndexTypeError { pub indexed_type: &'static str }

impl CannotIndexTypeError {
    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation instead
    pub fn message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl Error for CannotIndexTypeError {}

impl IntoArgumentError for CannotIndexTypeError {
    type Out = ArgumentError;

    fn error_for_argument(self, argument_index: usize) -> Self::Out {
        ArgumentError::CannotIndexType { error: self, argument_index }
    }
}

impl Display for CannotIndexTypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "attempt to index {}", self.indexed_type)
    }
}

/// Error denoting substring index is out of range
///
/// NOTE: Lua uses 1-based relative indexing, see <https://www.lua.org/manual/5.4/manual.html#pdf-string.sub>
#[derive(Debug)]
pub struct StringIndexOutOfRangeError {
    pub index: LUA_INT,
}

impl StringIndexOutOfRangeError {
    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation instead
    pub fn message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl Error for StringIndexOutOfRangeError {}

impl IntoArgumentError for StringIndexOutOfRangeError {
    type Out = ArgumentError;

    fn error_for_argument(self, argument_index: usize) -> Self::Out {
        ArgumentError::StringIndexOutOfRange { error: self, argument_index }
    }
}

impl Display for StringIndexOutOfRangeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "position out of bounds ({})", self.index)
    }
}

/// Error denoting a non-callable value was called
///
/// Callable lua values are functions and values with a __call metamethod
#[derive(Debug)]
pub struct AttemptToCallNonFunctionError { pub called_type: &'static str }

impl AttemptToCallNonFunctionError {
    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation instead
    pub fn message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl Error for AttemptToCallNonFunctionError {}

impl IntoArgumentError for AttemptToCallNonFunctionError {
    type Out = ArgumentError;

    fn error_for_argument(self, argument_index: usize) -> Self::Out {
        ArgumentError::AttemptToCallNonFunction { error: self, argument_index }
    }
}

impl Display for AttemptToCallNonFunctionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "attempt to call {}", self.called_type)
    }
}

/// Error denoting an invalid concatenation
///
/// NOTE: Concatenation is overloadable via __concat metamethod
///
/// See <https://www.lua.org/manual/5.4/manual.html#3.4.6> for details on Lua concatenation
#[derive(Debug)]
pub enum InvalidConcatenationError {
    TooLarge,   // TODO: Once concatenation length limit is implemented, refer to it in documentation of this variant
    InvalidType { concatenated_type: &'static str }
}

impl InvalidConcatenationError {
    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation instead
    pub fn message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl Error for InvalidConcatenationError {}

impl IntoArgumentError for InvalidConcatenationError {
    type Out = ArgumentError;

    fn error_for_argument(self, argument_index: usize) -> Self::Out {
        ArgumentError::InvalidConcatenation { error: self, argument_index }
    }
}

impl Display for InvalidConcatenationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InvalidConcatenationError::TooLarge => write!(f, "concatenation"),
            InvalidConcatenationError::InvalidType { concatenated_type } => write!(f, "attempt to concatenate {}", concatenated_type)
        }
    }
}

/// Error denoting malformed bytecode
#[derive(Debug)]
pub enum ByteCodeError {
    /// Program counter was >= amount of opcodes in prototype
    ///
    /// Generally caused by missing return instruction or malformed jump
    ProgramCounterOutOfBounds { counter: usize, code_length: usize },
    /// Invalid register index instruction argument
    ///
    /// Generally caused by malformed instruction
    RegisterIndexOutOfBounds { index: usize, registers_length: usize },
    /// Invalid constant index instruction argument
    ///
    /// Generally caused by malformed instruction
    ConstantIndexOutOfBounds { index: usize, constants_length: usize },
    /// Upvalue index instruction argument was out of bounds
    ///
    /// Generally caused by malformed instruction or running function expecting upvalues as main script
    UpvalueIndexOutOfBounds { upvalue_index: usize, upvalues_length: usize },
    /// Upvalue points to invalid register
    ///
    /// Generally caused by malformed instruction
    UpvalueRegisterIndexOutOfBounds { upvalue_index: usize, register_index: usize, registers_length: usize },
    /// Upvalue points to invalid stackframe
    ///
    /// Generally caused by malformed instruction
    UpvalueStackIndexOutOfBounds { upvalue_index: usize, stack_index: usize, stack_length: usize },
    /// Invalid inner-function index instruction argument
    ///
    /// Generally caused by malformed "CLOSURE" instruction
    PrototypeIndexOutOfBounds { prototype_index: usize, prototype_len: usize },
    /// Attempt to execute instruction with unknown opcode
    ///
    /// Generally caused by malformed instruction or running bytecode from different versions of Lua
    UnknownOpcode { opcode: u8 },
    /// Attempt to execute 'ExtraArg' instruction
    ///
    /// 'ExtraArg' is a data-instruction that cannot be executed
    ///
    /// Generally caused by malformed jump
    AttemptToExecuteExtraArg,
    /// Setlist instruction was set to fill until register 'top', starting at a register index that is greater than 'top'
    SetlistUnderflow,
    /// Expected a different instruction
    ///
    /// Generally caused by missing ExtraArg instruction
    ExpectedOpcode { expected: u8, found: u8 },
}

impl ByteCodeError {
    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation instead
    pub fn message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl Error for ByteCodeError {}

impl Display for ByteCodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ByteCodeError::ProgramCounterOutOfBounds { counter, code_length } => write!(f, "Program counter out of bounds at index {} with program length {}", counter, code_length),
            ByteCodeError::RegisterIndexOutOfBounds { index, registers_length } => write!(f, "Register index out of bounds at index {} with register count {}", index, registers_length),
            ByteCodeError::ConstantIndexOutOfBounds { index, constants_length } => write!(f, "Constant index out of bounds at index {} with constant count {}", index, constants_length),
            ByteCodeError::UpvalueIndexOutOfBounds { upvalue_index, upvalues_length } => write!(f, "Upvalue index out of bounds at index {} with upvalue count {}", upvalue_index, upvalues_length),
            ByteCodeError::UpvalueRegisterIndexOutOfBounds { upvalue_index, register_index, registers_length } => write!(f, "Upvalue ({}) register index out of bounds at index {} with register count {}", upvalue_index, register_index, registers_length),
            ByteCodeError::UpvalueStackIndexOutOfBounds { upvalue_index, stack_index, stack_length } => write!(f, "Upvalue ({}) stack index out of bounds at index {} with stack length {}", upvalue_index, stack_index, stack_length),
            ByteCodeError::PrototypeIndexOutOfBounds { prototype_index, prototype_len } => write!(f, "Prototype index out of bounds at index {} with protoype count {}", prototype_index, prototype_len),
            ByteCodeError::UnknownOpcode { opcode } => write!(f, "Unknown opcode: {:X}", opcode),
            ByteCodeError::AttemptToExecuteExtraArg => write!(f, "Attempt to execute ExtraArg opcode"),
            ByteCodeError::SetlistUnderflow => write!(f, "Underflow in SETLIST"),
            ByteCodeError::ExpectedOpcode { expected, found } => write!(f, "Expected opcode {}, found opcode {}", constants::opcodes::opcode_name(*expected), constants::opcodes::opcode_name(*found)),
        }
    }
}

/// Error denoting Lua script compilation failed
#[derive(Debug)]
pub enum CompileError {
    /// External IO error occured during compilation
    ///
    /// Generally caused by inability to write temporary files, or start luac subprocess
    IOError(io::Error),
    /// Script compilation failed
    CompileFailed(String),
    /// Script compilation succeeded, but resultant bytecode could not be loaded
    ///
    /// Generally caused by mismatch between compiler and loader or use of an incompatible version of `luac`
    DecodeError(LoadError),
}

impl CompileError {
    /// Returns the message of this error as a LuaValue
    ///
    /// If message is not passed to Lua script, rust callers should use [`Display`] implementation instead
    pub fn message(self) -> LuaValue {
        LuaValue::STRING(LuaString::from(format!("{}", self)))
    }
}

impl Error for CompileError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            CompileError::IOError(err) => Some(err),
            CompileError::DecodeError(err) => Some(err),
            _ => None,
        }
    }
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::IOError(error) => write!(f, "Compile io error: {}", error),
            CompileError::CompileFailed(message) => write!(f, "Compile error: {}", message.trim()),
            CompileError::DecodeError(error) => write!(f, "{}", error)
        }
    }
}

impl From<io::Error> for CompileError {
    fn from(err: io::Error) -> Self {
        CompileError::IOError(err)
    }
}

impl From<LoadError> for CompileError {
    fn from(err: LoadError) -> Self {
        CompileError::DecodeError(err)
    }
}