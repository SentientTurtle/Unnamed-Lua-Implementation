//! Module containing the Lua IO functions library
//!
//! Available functions vary depending on enabled cargo features

use std::borrow::Cow;
use crate::vm::LuaVM;
use crate::types::value::table::LuaTable;
use crate::types::value::LuaValue;
use crate::error::{LuaError, ArgumentError, IntoArgumentError, InvalidValueError, GenericError};
use std::io::{Read, Write, BufRead, BufReader, Seek, SeekFrom};
use crate::types::value::userdata::{UserDataValue, UserData};
use crate::types::{CoerceFrom, LuaType};
use crate::types::varargs::Varargs;
use crate::types::value::function::{LuaFunction, RustFunction, RustClosure};
use std::cell::RefCell;
use crate::types::value::number::LuaNumber;
use std::ffi::OsStr;
use std::fs::File;
use crate::types::parameters::LuaParameters;
use crate::types::value::string::LuaString;
use crate::constants::types::LUA_INT;

// TODO: Directory separator replacement
// TODO: __gc and __close for file handles

/// Concrete struct for Lua file handles
///
/// File handles may be constructed from [`IOStream`] using [`IOHandle::new`], or from [`Read`]/[`Write`] using the *_handle functions.
///
/// File handle Userdata should be downcast to this type
pub struct IOHandle(Box<dyn IOStream>);

impl IOHandle {
    /// Wraps an [`IOStream`] implementation into a file handle
    ///
    /// # Arguments
    ///
    /// * `stream`: Stream backing this handle
    ///
    /// returns: IOHandle
    // Constructors
    pub fn new<T: IOStream + 'static>(stream: T) -> IOHandle {
        IOHandle(Box::new(stream))
    }

    /// Creates handle from a [`BufRead`]
    ///
    /// # Arguments
    ///
    /// * `stream`: Read stream to wrap
    ///
    /// returns: IOHandle
    pub fn read_handle<T: BufRead + 'static>(stream: T) -> IOHandle {
        IOHandle(Box::new(ReadHandle(RefCell::new(Some(stream)))))
    }

    /// Creates handle from a [`BufRead`] with [`Seek`]
    ///
    /// This permits the Lua script to seek this reader. [`IOHandle::read_handle`] should be used if this is undesirable.
    ///
    /// # Arguments
    ///
    /// * `stream`: Read stream to wrap
    ///
    /// returns: IOHandle
    pub fn read_seek_handle<T: BufRead + Seek + 'static>(stream: T) -> IOHandle {
        IOHandle(Box::new(ReadSeekHandle(ReadHandle(RefCell::new(Some(stream))))))
    }

    /// Creates handle from a [`Write`]
    ///
    /// It is recommended to use a buffered writer, Lua file:write calls result in one-or-more write calls on the underlying stream.
    ///
    /// # Arguments
    ///
    /// * `stream`: Write stream to wrap
    ///
    /// returns: IOHandle
    pub fn write_handle<T: Write + 'static>(stream: T) -> IOHandle {
        IOHandle(Box::new(WriteHandle(RefCell::new(Some(stream)))))
    }

    /// Creates handle from a [`Write`] with [`Seek`]
    ///
    /// This permits the Lua script to seek this writer. [`IOHandle::write_handle`] should be used if this is undesirable.
    ///
    /// It is recommended to use a buffered writer, Lua file:write calls result in one-or-more write calls on the underlying stream.
    ///
    /// # Arguments
    ///
    /// * `stream`: Write stream to wrap
    ///
    /// returns: IOHandle
    pub fn write_seek_handle<T: Write + Seek + 'static>(stream: T) -> IOHandle {
        IOHandle(Box::new(WriteSeekHandle(WriteHandle(RefCell::new(Some(stream))))))
    }

    /// Creates a single handle from both a [`Read`] and a [`Write`], resulting handle may both be read from and written to by Lua scripts.
    ///
    /// It is recommended to use a buffered writer, Lua file:write calls result in one-or-more write calls on the underlying stream.
    ///
    /// # Arguments
    ///
    /// * `read`: Reader that handle delegates to
    /// * `write`: Writer that handle delegates to
    ///
    /// returns: IOHandle
    pub fn duplex_handle<R: BufRead + 'static, W: Write + 'static>(read: R, write: W) -> IOHandle {
        IOHandle(Box::new(DuplexHandle(
            ReadHandle(RefCell::new(Some(read))),
            WriteHandle(RefCell::new(Some(write))))
        ))
    }

    /// returns: IOHandle to LuaVM's stdin
    ///
    /// Note: This handle does not capture the set stdin, changing LuaVM's stdio streams will be change this handle as well.
    pub fn stdin() -> IOHandle {
        IOHandle(Box::new(STDIOHandle::IN))
    }


    /// returns: IOHandle to LuaVM's stdout
    ///
    /// Note: This handle does not capture the set stdin, changing LuaVM's stdio streams will be change this handle as well.
    pub fn stdout() -> IOHandle {
        IOHandle(Box::new(STDIOHandle::OUT))
    }

    /// returns: IOHandle to LuaVM's stderr
    ///
    /// Note: This handle does not capture the set stdin, changing LuaVM's stdio streams will be change this handle as well.
    pub fn stderr() -> IOHandle {
        IOHandle(Box::new(STDIOHandle::ERR))
    }

    // Methods

    /// Delegation to [`IOStream::close`]
    fn close(&self, lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> { // TODO: Promote this error type to LuaError?
        self.0.close(lua_vm)
    }

    /// Delegation to [`IOStream::flush`]
    fn flush(&self, lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> {
        self.0.flush(lua_vm)
    }

    /// Delegation to [`IOStream::read`]
    fn read(&self, lua_vm: &mut LuaVM, format: &[IOReadFormat]) -> Result<Varargs, GenericError> {
        self.0.read(lua_vm, &format)
    }

    /// Delegation to [`IOStream::seek`]
    fn seek(&self, lua_vm: &mut LuaVM, position: SeekFrom) -> Result<Varargs, GenericError> {
        self.0.seek(lua_vm, position)
    }

    /// Delegation to [`IOStream::setvbuf`]
    fn setvbuf(&self, lua_vm: &mut LuaVM, mode: BufferMode, size: usize) -> Result<Varargs, GenericError> {
        self.0.setvbuf(lua_vm, mode, size)
    }

    /// Delegation to [`IOStream::write`]
    fn write(&self, lua_vm: &mut LuaVM, values: &[&[u8]]) -> Result<bool, GenericError> {
        self.0.write(lua_vm, values)
    }

    /// Delegation to [`IOStream::is_closed`]
    fn is_closed(&self, lua_vm: &mut LuaVM) -> bool {
        self.0.is_closed(lua_vm)
    }
}

impl UserDataValue for IOHandle {
    const TYPE_NAME: &'static str = "file";

    fn userdata_metatable(lua_vm: &mut LuaVM) -> Option<LuaTable> {
        LuaTable::coerce_opt(&lua_vm.registry.raw_get("filehandle-metatable"))
    }
}

/// Read format elements
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IOReadFormat {
    /// Read a single number
    Numeral,
    /// Read all remaining bytes
    All,
    /// Read a full line, trimming trailing newline
    LineTrimmed,
    /// Read a full line, including trailing newline
    Line,
    /// Read up to the specified amount of bytes
    Bytes(usize),
}

/// BufferMode for writing. Unused by reference implementations, other implementations may opt to use them.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BufferMode {
    None,
    Full,
    Line,
}

/// Features available to an IOHandle
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IOFeature {
    READ,
    WRITE,
    SEEK,
}

/// Inner implementation trait for IO Handles
///
/// Implementers should ensure [`Drop::drop`] closes underlying stream
pub trait IOStream {

    /// Closes the underlying stream. Corresponds to Lua `file:close()`
    ///
    /// Note: This function is not called on garbage collect, implementers should instead rely on close-on-drop
    ///
    /// See: <https://www.lua.org/manual/5.4/manual.html#pdf-file:close>
    fn close(&self, lua_vm: &mut LuaVM) -> Result<Varargs, GenericError>;

    /// Flushes the underlying stream. Corresponds to Lua `file:flush()`
    ///
    /// See: <https://www.lua.org/manual/5.4/manual.html#pdf-file:flush>
    fn flush(&self, lua_vm: &mut LuaVM) -> Result<Varargs, GenericError>;

    /// Reads from the underlying stream, NO-OP on write-only streams. Corresponds to Lua `file:read()`
    ///
    /// See: <https://www.lua.org/manual/5.4/manual.html#pdf-file:read>
    ///
    /// # Arguments
    ///
    /// * `lua_vm`: LuaVM to use
    /// * `format`: Values to read, errors thrown for the first value-format that could not be read
    ///
    /// returns: Result<Varargs, GenericError>
    fn read(&self, lua_vm: &mut LuaVM, format: &[IOReadFormat]) -> Result<Varargs, GenericError>;

    /// Seeks this stream, errors if seeking is not possible in this stream. Corresponds to Lua `file:seek()`
    ///
    /// See: <https://www.lua.org/manual/5.4/manual.html#pdf-file:seek>
    ///
    /// # Arguments
    ///
    /// * `lua_vm`: LuaVM to use
    /// * `position`: Position to seek to, may be directly passed to [`Seek::seek`]
    ///
    /// returns: Result<Varargs, GenericError>
    fn seek(&self, lua_vm: &mut LuaVM, position: SeekFrom) -> Result<Varargs, GenericError>;

    /// Sets buffering mode for this stream. Implementations may ignore this, or provide platform-specific behavior Corresponds to Lua `file:setvbuf()`
    ///
    /// See: <https://www.lua.org/manual/5.4/manual.html#pdf-file:seek>
    fn setvbuf(&self, lua_vm: &mut LuaVM, mode: BufferMode, buffer_size: usize) -> Result<Varargs, GenericError>;

    /// Writes to the underlying stream, returns true if write succeeded, false if this is a read-only stream. Corresponds to Lua `file:write()`
    ///
    /// Note: This function has no return value. Lua scripts will receive the file handle as return value.
    ///
    /// See: <https://www.lua.org/manual/5.4/manual.html#pdf-file:write>
    ///
    /// # Arguments
    ///
    /// * `lua_vm`: LuaVM to use
    /// * `values`: Values to be written
    ///
    /// returns: Result<bool, GenericError>
    fn write(&self, lua_vm: &mut LuaVM, values: &[&[u8]]) -> Result<bool, GenericError>;  // TODO: Possibly trim this down to a single slice, rather than a slice of slices

    /// True if this stream is closed
    fn is_closed(&self, lua_vm: &mut LuaVM) -> bool;

    /// Details whether Reading, Writing, and/or Seeking is possible in this stream
    ///
    /// Currently not exposed to Lua scripts
    fn available_features(&self, lua_vm: &mut LuaVM) -> &'static [IOFeature];
}

/// IOStream implementation wrapper for [`Read`]
struct ReadHandle<T: Read>(RefCell<Option<T>>);

impl<T: Read> ReadHandle<T> {
    pub(crate) fn read_no_vm(&self, format: &[IOReadFormat]) -> Result<Varargs, GenericError> {
        if let Some(reader) = &mut *self.0.borrow_mut() {
            let mut results: Vec<LuaValue> = Vec::new();

            'format_loop: for f in format {
                match f {
                    IOReadFormat::Numeral => {
                        struct ReadNumber {
                            current_character: i16,
                            buffer: Vec<u8>,
                        }

                        fn read_byte<T: Read>(reader: &mut T) -> i16 {
                            let mut buf = 0u8;
                            match reader.read_exact(std::slice::from_mut(&mut buf)) {
                                Ok(_) => buf as i16,
                                Err(_) => -1
                            }
                        }

                        fn nextc<T: Read>(rn: &mut ReadNumber, reader: &mut T) -> bool {
                            if rn.buffer.len() >= MAX_NUMERAL_LENGTH {
                                rn.buffer.truncate(0);
                                false
                            } else {
                                rn.buffer.push(rn.current_character as u8);
                                rn.current_character = read_byte(reader);
                                true
                            }
                        }

                        fn test2<T: Read>(rn: &mut ReadNumber, set: [u8; 2], reader: &mut T) -> bool {
                            if rn.current_character == set[0] as i16 || rn.current_character == set[1] as i16 {
                                nextc(rn, reader)
                            } else {
                                false
                            }
                        }

                        fn read_digits<T: Read>(rn: &mut ReadNumber, hex: bool, reader: &mut T) -> usize {
                            use std::ops::RangeInclusive;   // Small hack: We can't case binary literals to i16 in the match block, we can use static ranges in the match block
                            static DECIMAL_RANGE: RangeInclusive<i16> = b'0' as i16..=b'9' as i16;
                            static LOWER_HEX_RANGE: RangeInclusive<i16> = b'a' as i16..=b'f' as i16;
                            static UPPER_HEX_RANGE: RangeInclusive<i16> = b'A' as i16..=b'F' as i16;

                            let mut count = 0;
                            while
                            (
                                DECIMAL_RANGE.contains(&rn.current_character)
                                    || (
                                    hex && (
                                        LOWER_HEX_RANGE.contains(&rn.current_character)
                                            || UPPER_HEX_RANGE.contains(&rn.current_character)
                                    )
                                )
                            ) && nextc(rn, reader) {
                                count += 1;
                            }
                            count
                        }

                        const MAX_NUMERAL_LENGTH: usize = 200;
                        let mut rn = ReadNumber {
                            current_character: 0,
                            buffer: Vec::with_capacity(MAX_NUMERAL_LENGTH),
                        };

                        let mut count = 0;
                        let mut hex = false;

                        loop {  // Skip whitespace
                            let byte = read_byte(reader);
                            rn.current_character = byte;
                            if !(byte > 0 && (byte as u8).is_ascii_whitespace()) {
                                break;
                            }
                        }
                        test2(&mut rn, *b"-+", reader);
                        if test2(&mut rn, *b"00", reader) { // Wonder if the compiler's smart enough to optimize this
                            if test2(&mut rn, *b"xX", reader) {
                                hex = true;
                            } else {
                                count = 1;
                            }
                        }
                        count += read_digits(&mut rn, hex, reader);
                        if test2(&mut rn, *b".,", reader) { // TODO: Decimal locales
                            count += read_digits(&mut rn, hex, reader);
                        }
                        if count > 0 && test2(&mut rn, if hex { *b"pP" } else { *b"eE" }, reader) {
                            test2(&mut rn, *b"-+", reader);
                            read_digits(&mut rn, false, reader);
                        }
                        match LuaNumber::coerce_opt(&LuaValue::from(rn.buffer)) {
                            None => break 'format_loop,
                            Some(number) => results.push(number.into())
                        }
                    }
                    IOReadFormat::All => {
                        let mut buf = Vec::new();
                        match reader.read_to_end(&mut buf) {
                            Ok(_) => results.push(LuaValue::from(buf)),
                            Err(_) => break 'format_loop
                        }
                    }
                    IOReadFormat::LineTrimmed | IOReadFormat::Line => {
                        let mut buf = Vec::new();
                        let mut is_error = false;
                        loop {
                            let mut byte = 0u8;

                            match reader.read_exact(std::slice::from_mut(&mut byte)) {
                                Ok(_) => buf.push(byte),
                                Err(_) => {
                                    is_error = true;
                                    break
                                },
                            }
                            if byte == b'\n' {
                                break
                            }
                        }
                        if buf.len() > 0 {
                            if let IOReadFormat::LineTrimmed = f {
                                if let Some(b'\n') = buf.last() {
                                    buf.truncate(buf.len() - 1);
                                    if let Some(b'\r') = buf.last() {
                                        buf.truncate(buf.len() - 1);
                                    }
                                }
                            }
                        }
                        if is_error {
                            break 'format_loop;
                        }
                        results.push(LuaValue::from(buf));
                    }
                    IOReadFormat::Bytes(n) => {
                        let mut buf = Vec::with_capacity(*n);
                        match reader.take(*n as u64).read_to_end(&mut buf) {
                            Ok(0) => {
                                if *n == 0 {
                                    results.push(LuaValue::from(buf));
                                } else {
                                    break 'format_loop;
                                }
                            }
                            Ok(_) => results.push(LuaValue::from(buf)),
                            Err(_) => {
                                if buf.len() > 0 {
                                    results.push(LuaValue::from(buf));
                                }
                                break 'format_loop;
                            }
                        }
                    }
                }
            }

            Ok(Varargs::from(results))
        } else {
            Err(GenericError::Str("attempt to use a closed file"))
        }
    }
}

impl<T: Read> IOStream for ReadHandle<T> {
    fn close(&self, _lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> {
        let mut option = self.0.borrow_mut();
        if option.is_some() {
            option.take();
            Ok(Varargs::from(()))    // TODO: Return process exit status / signal
        } else {
            Err(GenericError::Str("attempt to use a closed file"))
        }
    }

    fn flush(&self, _lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> { Ok(Varargs::from(true)) }    // NO-OP for Read-only streams

    fn read(&self, _lua_vm: &mut LuaVM, format: &[IOReadFormat]) -> Result<Varargs, GenericError> {
        self.read_no_vm(format)
    }

    fn seek(&self, _lua_vm: &mut LuaVM, _position: SeekFrom) -> Result<Varargs, GenericError> {
        Err(GenericError::Str("Cannot seek in this file"))
    }

    fn setvbuf(&self, _lua_vm: &mut LuaVM, _mode: BufferMode, _size: usize) -> Result<Varargs, GenericError> {
        Ok(Varargs::from(()))  // NO-OP
    }

    fn write(&self, _lua_vm: &mut LuaVM, values: &[&[u8]]) -> Result<bool, GenericError> {
        if values.len() == 0 {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn is_closed(&self, _lua_vm: &mut LuaVM) -> bool {
        self.0.borrow().is_none()
    }

    fn available_features(&self, _lua_vm: &mut LuaVM) -> &'static [IOFeature] {
        &[IOFeature::READ]
    }
}

/// IOStream implementation wrapper for [`Read`] + [`Seek`]
struct ReadSeekHandle<T: Read + Seek>(ReadHandle<T>);

impl<T: Read + Seek> IOStream for ReadSeekHandle<T> {
    fn seek(&self, _lua_vm: &mut LuaVM, position: SeekFrom) -> Result<Varargs, GenericError> {
        if let Some(file) = &mut *self.0.0.borrow_mut() {
            match file.seek(position) {
                Ok(new_position) => Ok(Varargs::from(new_position)),
                Err(err) => Ok(Varargs::from([Varargs::fail_value(), format!("{}", err).into()])),
            }
        } else {
            Err(GenericError::Str("attempt to use a closed file"))
        }
    }

    // Delegate to ReadHandle
    fn close(&self, lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> {
        self.0.close(lua_vm)
    }

    fn flush(&self, lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> {
        self.0.flush(lua_vm)
    }

    fn read(&self, lua_vm: &mut LuaVM, format: &[IOReadFormat]) -> Result<Varargs, GenericError> {
        self.0.read(lua_vm, format)
    }

    fn setvbuf(&self, lua_vm: &mut LuaVM, mode: BufferMode, size: usize) -> Result<Varargs, GenericError> {
        self.0.setvbuf(lua_vm, mode, size)
    }

    fn write(&self, lua_vm: &mut LuaVM, values: &[&[u8]]) -> Result<bool, GenericError> {
        self.0.write(lua_vm, values)
    }

    fn is_closed(&self, lua_vm: &mut LuaVM) -> bool {
        self.0.is_closed(lua_vm)
    }

    fn available_features(&self, _lua_vm: &mut LuaVM) -> &'static [IOFeature] {
        &[IOFeature::READ, IOFeature::SEEK]
    }
}

/// IOStream implementation wrapper for [`Write`]
struct WriteHandle<T: Write>(RefCell<Option<T>>);

impl<T: Write> WriteHandle<T> {
    fn flush_no_vm(&self) -> Result<Varargs, GenericError> {
        if let Some(writer) = &mut *self.0.borrow_mut() {
            writer.flush()
                .map(|_| Varargs::empty())
                .map_err(|err| GenericError::String(format!("{}", err)))
        } else {
            Err(GenericError::Str("attempt to use a closed file"))
        }
    }

    pub(crate) fn write_no_vm(&self, values: &[&[u8]]) -> Result<bool, GenericError> {
        if let Some(writer) = &mut *self.0.borrow_mut() {
            for buf in values {
                writer.write_all(*buf)
                    .map_err(|err| GenericError::String(format!("{}", err)))?
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl<T: Write> IOStream for WriteHandle<T> {
    fn close(&self, _lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> {
        let mut option = self.0.borrow_mut();
        if option.is_some() {
            option.take();
            Ok(Varargs::from(()))    // TODO: Return process exit status / signal
        } else {
            Err(GenericError::Str("attempt to use a closed file"))
        }
    }

    fn flush(&self, _lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> {
        self.flush_no_vm()
    }

    fn read(&self, _lua_vm: &mut LuaVM, _format: &[IOReadFormat]) -> Result<Varargs, GenericError> {
        Ok(Varargs::nil())  // NO-OP
    }

    fn seek(&self, _lua_vm: &mut LuaVM, _position: SeekFrom) -> Result<Varargs, GenericError> {
        Err(GenericError::Str("Cannot seek in this file"))
    }

    fn setvbuf(&self, _lua_vm: &mut LuaVM, _buffer_mode: BufferMode, _buffer_size: usize) -> Result<Varargs, GenericError> {
        Ok(Varargs::nil())  // Buffermode currently unimplemented
    }

    fn write(&self, _lua_vm: &mut LuaVM, values: &[&[u8]]) -> Result<bool, GenericError> {
        self.write_no_vm(values)
    }

    fn is_closed(&self, _lua_vm: &mut LuaVM) -> bool {
        self.0.borrow().is_none()
    }

    fn available_features(&self, _lua_vm: &mut LuaVM) -> &'static [IOFeature] {
        &[IOFeature::WRITE]
    }
}

/// IOStream implementation wrapper for [`Write`] + [`Seek`]
struct WriteSeekHandle<T: Write + Seek>(WriteHandle<T>);

impl<T: Write + Seek> IOStream for WriteSeekHandle<T> {
    fn seek(&self, _lua_vm: &mut LuaVM, position: SeekFrom) -> Result<Varargs, GenericError> {
        if let Some(file) = &mut *self.0.0.borrow_mut() {
            match file.seek(position) {
                Ok(new_position) => Ok(Varargs::from(new_position)),
                Err(err) => Ok(Varargs::from([Varargs::fail_value(), format!("{}", err).into()])),
            }
        } else {
            Err(GenericError::Str("attempt to use a closed file"))
        }
    }

    // Delegate to WriteHandle
    fn close(&self, lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> {
        self.0.close(lua_vm)
    }

    fn flush(&self, lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> {
        self.0.flush(lua_vm)
    }

    fn read(&self, lua_vm: &mut LuaVM, format: &[IOReadFormat]) -> Result<Varargs, GenericError> {
        self.0.read(lua_vm, format)
    }

    fn setvbuf(&self, lua_vm: &mut LuaVM, mode: BufferMode, size: usize) -> Result<Varargs, GenericError> {
        self.0.setvbuf(lua_vm, mode, size)
    }

    fn write(&self, lua_vm: &mut LuaVM, values: &[&[u8]]) -> Result<bool, GenericError> {
        self.0.write(lua_vm, values)
    }

    fn is_closed(&self, lua_vm: &mut LuaVM) -> bool {
        self.0.is_closed(lua_vm)
    }

    fn available_features(&self, _lua_vm: &mut LuaVM) -> &'static [IOFeature] {
        &[IOFeature::READ, IOFeature::SEEK]
    }
}

/// Duplex IOStream that wraps both a [`ReadHandle`] and [`WriteHandle`], permits both reading and writing to the same Lua file handle
struct DuplexHandle<R: BufRead, W: Write>(ReadHandle<R>, WriteHandle<W>);

impl<R: BufRead, W: Write> IOStream for DuplexHandle<R, W> {
    fn close(&self, lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> {
        match (self.0.close(lua_vm), self.1.close(lua_vm)) {
            (Ok(_read_args), Ok(write_args)) => Ok(write_args),
            (Err(read_err), Ok(_write_args)) => Err(read_err),
            (_, Err(write_err)) => Err(write_err),
        }
    }

    fn flush(&self, lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> {
        match (self.0.flush(lua_vm), self.1.flush(lua_vm)) {
            (Ok(_read_args), Ok(write_args)) => Ok(write_args),
            (Err(read_err), Ok(_write_args)) => Err(read_err),
            (_, Err(write_err)) => Err(write_err),
        }
    }

    fn read(&self, lua_vm: &mut LuaVM, format: &[IOReadFormat]) -> Result<Varargs, GenericError> {
        self.0.read(lua_vm, format)
    }

    fn seek(&self, _lua_vm: &mut LuaVM, _position: SeekFrom) -> Result<Varargs, GenericError> {
        Err(GenericError::Str("Cannot seek in this file"))
    }

    fn setvbuf(&self, lua_vm: &mut LuaVM, mode: BufferMode, size: usize) -> Result<Varargs, GenericError> {
        match (self.0.setvbuf(lua_vm, mode, size), self.1.setvbuf(lua_vm, mode, size)) {
            (Ok(_read_args), Ok(write_args)) => Ok(write_args),
            (Err(read_err), Ok(_write_args)) => Err(read_err),
            (_, Err(write_err)) => Err(write_err),
        }
    }

    fn write(&self, lua_vm: &mut LuaVM, values: &[&[u8]]) -> Result<bool, GenericError> {
        self.1.write(lua_vm, values)
    }

    fn is_closed(&self, lua_vm: &mut LuaVM) -> bool {
        debug_assert_eq!(self.0.is_closed(lua_vm), self.1.is_closed(lua_vm));   // is_closed should only change on manual closure via ::close method, which closes both handles for this type
        self.0.is_closed(lua_vm)
    }

    fn available_features(&self, _lua_vm: &mut LuaVM) -> &'static [IOFeature] {
        &[IOFeature::READ, IOFeature::WRITE]
    }
}

/// Handle that reads or writes LuaVM's stdio
enum STDIOHandle {
    IN, OUT, ERR
}
impl IOStream for STDIOHandle {
    fn close(&self, _lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> {
        Err(GenericError::Str("cannot close standard file"))
    }

    fn flush(&self, lua_vm: &mut LuaVM) -> Result<Varargs, GenericError> {
        let file = match self {
            STDIOHandle::IN => return Ok(Varargs::from(true)),
            STDIOHandle::OUT => lua_vm.stdout(),
            STDIOHandle::ERR => lua_vm.stderr()
        };

        WriteHandle(RefCell::from(Some(file)))  // Creating a refcell for a single call is hacky, but very cheap
            .flush_no_vm()
    }

    fn read(&self, lua_vm: &mut LuaVM, format: &[IOReadFormat]) -> Result<Varargs, GenericError> {
        let file = match self {
            STDIOHandle::IN => lua_vm.stdin(),
            STDIOHandle::OUT => return Ok(Varargs::empty()),
            STDIOHandle::ERR => return Ok(Varargs::empty()),
        };

        ReadHandle(RefCell::from(Some(file)))  // Creating a refcell for a single call is hacky, but very cheap
            .read_no_vm(format)
    }

    fn seek(&self, _lua_vm: &mut LuaVM, _position: SeekFrom) -> Result<Varargs, GenericError> {
        Err(GenericError::Str("Cannot seek in this file"))
    }

    fn setvbuf(&self, _lua_vm: &mut LuaVM, _mode: BufferMode, _size: usize) -> Result<Varargs, GenericError> {
        Ok(Varargs::nil())  // Buffermode currently unimplemented
    }

    fn write(&self, lua_vm: &mut LuaVM, values: &[&[u8]]) -> Result<bool, GenericError> {
        let file = match self {
            STDIOHandle::IN => return Ok(false),
            STDIOHandle::OUT => lua_vm.stdout(),
            STDIOHandle::ERR => lua_vm.stderr()
        };

        WriteHandle(RefCell::from(Some(file)))  // Creating a refcell for a single call is hacky, but _very_ cheap (1-2 x86 instructions); We still pay the RefCell access cost, as with regular IOStreams
            .write_no_vm(values)
    }

    fn is_closed(&self, _lua_vm: &mut LuaVM) -> bool {
        false   // Standard streams cannot be closed
    }

    fn available_features(&self, _lua_vm: &mut LuaVM) -> &'static [IOFeature] {
        match self {
            STDIOHandle::IN => &[IOFeature::READ],
            STDIOHandle::OUT => &[IOFeature::WRITE],
            STDIOHandle::ERR => &[IOFeature::WRITE]
        }
    }
}

/// Parses a slice of Lua parameters into IO Read formats
///
/// # Arguments
///
/// * `params`: Parameters to parse
/// * `start_index`: Start index of params, used as offset in errors
///
/// returns: Result<Vec<IOReadFormat, Global>, ArgumentError>
fn parse_read_format(params: &[LuaValue], start_index: usize) -> Result<Vec<IOReadFormat>, ArgumentError> {
    let formats = Vec::with_capacity(params.len());
    for (index, value) in params.iter().enumerate() {
        match value {
            LuaValue::STRING(_string) => todo!(),
            LuaValue::NUMBER(_number) => todo!(),
            _ => Err(ArgumentError::InvalidArgument {
                message: "invalid read format",
                argument_index: index + start_index,
            })?
        }
    }
    Ok(formats)
}

/// Metatable for file handles, delegates file functions to their [`IOStream`] implementation
const FILEHANDLE_METATABLE: fn() -> LuaValue = || {
    let methods = LuaTable::of_map([
        (
            "close",
            LuaFunction::RUST_FUNCTION(RustFunction::from_parts(
                "file:close",
                |lua_vm, params| params.coerce::<UserData>(0)?
                    .downcast::<IOHandle>()
                    .error_for_argument(0)?
                    .close(lua_vm)
                    .error_for_argument(0)
                    .map_err(LuaError::from),
            ))
        ),
        (
            "flush",
            LuaFunction::RUST_FUNCTION(RustFunction::from_parts(
                "file:flush",
                |lua_vm, params| params.coerce::<UserData>(0)?
                    .downcast::<IOHandle>()
                    .error_for_argument(0)?
                    .flush(lua_vm)
                    .error_for_argument(0)
                    .map_err(LuaError::from),
            )),
        ),
        (
            "lines",
            LuaFunction::RUST_FUNCTION(RustFunction::from_parts(
                "file:lines",
                |_, params| {
                    let userdata = params.coerce::<UserData>(0)?;
                    let formats = parse_read_format(&params[1..], 1)?;
                    Ok(Varargs::from(LuaFunction::RUST_CLOSURE(
                        LineIterator::new(userdata, formats, false)
                            .error_for_argument(0)?
                            .into_closure()
                    )))
                },
            )),
        ),
        (
            "read",
            LuaFunction::RUST_FUNCTION(RustFunction::from_parts(
                "file:read",
                |lua_vm, params| params.coerce::<UserData>(0)?
                    .downcast::<IOHandle>()
                    .error_for_argument(0)?
                    .read(lua_vm, &*parse_read_format(&params[1..], 0)?)
                    .map_err(LuaError::from),
            )),
        ),
        (
            "seek",
            LuaFunction::RUST_FUNCTION(RustFunction::from_parts(
                "file:seek",
                |lua_vm, params| {
                    let userdata = params.coerce::<UserData>(0)?;
                    let handle = userdata.downcast::<IOHandle>().error_for_argument(0)?;

                    let whence = params.coerce_opt::<LuaString>(1)?;
                    let offset = params.coerce_opt::<LUA_INT>(2).map(|opt| opt.unwrap_or(0))?;
                    let position = if let Some(string) = whence {
                        match string.as_bytes() {
                            b"set" | b"SET" => {
                                let offset = offset;
                                if offset >= 0 {
                                    Ok(SeekFrom::Start(offset as u64))
                                } else {
                                    Err(ArgumentError::InvalidArgument { message: "may not seek behind start", argument_index: 1 })
                                }
                            }
                            b"cur" | b"CUR" => Ok(SeekFrom::Current(offset)),
                            b"end" | b"END" => Ok(SeekFrom::End(offset)),
                            _ => Err(ArgumentError::InvalidArgument { message: "invalid option", argument_index: 0 }),
                        }?
                    } else {
                        SeekFrom::Start(0)
                    };
                    handle.seek(lua_vm, position)
                        .map_err(LuaError::from)
                },
            )),
        ),
        (
            "setvbuf",
            LuaFunction::RUST_FUNCTION(RustFunction::from_parts(
                "file:setvbuf",
                |lua_vm, params| {
                    let userdata = params.coerce::<UserData>(0)?;
                    let handle = userdata.downcast::<IOHandle>()
                        .error_for_argument(0)?;

                    let mode = match params.coerce::<LuaString>(1)?.as_bytes() {
                        b"no" => BufferMode::None,
                        b"line" => BufferMode::Line,
                        b"full" => BufferMode::Full,
                        _ => return Err(LuaError::new("invalid buffermode"))
                    };

                    let size = params.coerce_opt::<usize>(2)?.unwrap_or(1024);

                    handle.setvbuf(lua_vm, mode, size).map_err(LuaError::from)
                },
            )),
        ),
        (
            "write",
            LuaFunction::RUST_FUNCTION(RustFunction::from_parts(
                "file:write",
                |lua_vm, params| {
                    let value = params.first_or_nil();
                    let userdata = UserData::coerce_from(value)
                        .error_for_argument(0)?;
                    let handle = userdata.downcast::<IOHandle>()
                        .error_for_argument(0)?;
                    let mut string_buffer = Vec::with_capacity(params.len());   // We need to keep the strings so they can be referenced to
                    for (index, value) in params[1..].iter().enumerate() {
                        let string = LuaString::coerce_from(value)
                            .error_for_argument(index)?;
                        string_buffer.push(string);
                    }
                    let byte_buffer: Vec<&[u8]> = string_buffer.iter().map(LuaString::as_bytes).collect();
                    handle.write(lua_vm, &*byte_buffer)
                        .map(|could_write| if could_write {
                            Varargs::from(value.clone())
                        } else {
                            Varargs::from((LuaValue::NIL, "cannot write to this file"))
                        })
                        .map_err(LuaError::from)
                },
            )),
        )
    ]);

    LuaValue::from(LuaTable::of_map([("__index", methods)]))
};

/// Closes specified file, or default output file if no file was specified
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-io.close>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn close(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let userdata = match params.first() {
        Some(LuaValue::USERDATA(userdata)) => Cow::Borrowed(userdata),
        Some(LuaValue::NIL) | None => {
            let default_file = lua_vm.registry.raw_get("default_output");
            if let LuaValue::USERDATA(userdata) = default_file {
                Cow::Owned(userdata)
            } else {
                Err(LuaError::with_message(format!("default file of type '{}' instead of type 'file'", default_file.type_name())))?
            }
        }
        Some(lua_value) => Err(ArgumentError::InvalidValue {
            error: InvalidValueError {
                expected: "file",
                found: lua_value.type_name(),
            },
            argument_index: 0,
        })?,
    };
    userdata.downcast::<IOHandle>()?
        .close(lua_vm)
        .map_err(LuaError::from)
}

/// Flushes specified file, or default output file if no file was specified
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-io.flush>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn flush(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    let userdata = match params.first() {
        Some(LuaValue::USERDATA(userdata)) => Cow::Borrowed(userdata),
        Some(LuaValue::NIL) | None => {
            let default_file = lua_vm.registry.raw_get("default_output");
            if let LuaValue::USERDATA(userdata) = default_file {
                Cow::Owned(userdata)
            } else {
                Err(LuaError::with_message(format!("default file of type '{}' instead of type 'file'", default_file.type_name())))?
            }
        }
        Some(lua_value) => Err(ArgumentError::InvalidValue {
            error: InvalidValueError {
                expected: "file",
                found: lua_value.type_name(),
            },
            argument_index: 0,
        })?,
    };
    userdata.downcast::<IOHandle>()?
        .flush(lua_vm)
        .map_err(LuaError::from)
}

/// With no arguments: Returns default input file.
/// With file argument: Sets default input file.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-io.input>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn input(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    match params.first() {
        Some(value @ LuaValue::USERDATA(userdata)) => {
            userdata.downcast::<IOHandle>()
                .error_for_argument(0)?;

            lua_vm.registry.raw_set("default_input", value.clone());
            Ok(Varargs::from(value.clone()))
        }
        Some(LuaValue::STRING(path)) => {
            if cfg!(feature = "file-io") {
                let os_path = path.as_str()
                    .map(OsStr::new)
                    .ok_or(LuaError::new("Path must be valid UTF-8"))?;

                let file = File::open(os_path).map_err(|e| GenericError::String(format!("{}", e)))?;
                let handle = IOHandle::read_seek_handle(BufReader::new(file));

                let lua_value = LuaValue::from(UserData::new(handle, lua_vm));
                lua_vm.registry.raw_set("default_input", lua_value.clone());
                Ok(Varargs::from(lua_value))
            } else {
                Err(GenericError::Str("File-IO is not available").into())
            }
        }
        Some(LuaValue::NIL) | None => {
            Ok(Varargs::from(lua_vm.registry.raw_get("default_input")))
        }
        Some(lua_value) => Err(ArgumentError::InvalidValue {
            error: InvalidValueError {
                expected: "file",
                found: lua_value.type_name(),
            },
            argument_index: 0,
        })?,
    }
}

/// Implementation struct for Lua `file:lines()` iterator
struct LineIterator {
    handle: UserData,
    format: Vec<IOReadFormat>,
    close_on_end_of_file: bool,
}

impl LineIterator {
    pub fn new(userdata: UserData, format: Vec<IOReadFormat>, close_on_end_of_file: bool) -> Result<LineIterator, InvalidValueError> {
        if userdata.downcast::<IOHandle>().is_ok() {
            Ok(LineIterator {
                handle: userdata,
                format,
                close_on_end_of_file,
            })
        } else {
            Err(InvalidValueError { expected: "file", found: "userdata" })
        }
    }

    pub fn into_closure(self) -> RustClosure {
        RustClosure::new(
            "LineIterator",
            move |lua_vm: &mut LuaVM, _params: &[LuaValue]| {
                let handle = self.handle.downcast::<IOHandle>().unwrap();   // Constructor for LineIterator verifies this downcast works; Potential performance hack: use unchecked_unwrap
                let line = handle.read(lua_vm, &self.format);
                if self.close_on_end_of_file {
                    if let Ok(varargs) = &line {
                        if varargs.is_fail() && self.close_on_end_of_file {
                            handle.close(lua_vm).map_err(LuaError::from)?;
                        }
                    } else {
                        handle.close(lua_vm).map_err(LuaError::from)?;
                    }
                }
                line.map_err(LuaError::from)
            },
        )
    }
}

/// Opens file into an iterator. File is closed automatically on iterator exhaustion (or drop)
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-io.lines>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn lines(_lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    todo!()
}

/// Opens file, with optional mode
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-io.open>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn open(_lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    todo!()
}

/// With no arguments: Returns default output file.
/// With file argument: Sets default output file.
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-io.output>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn output(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    match params.first() {
        Some(value @ LuaValue::USERDATA(userdata)) => {
            userdata.downcast::<IOHandle>()
                .error_for_argument(0)?;

            lua_vm.registry.raw_set("default_output", value.clone());
            Ok(Varargs::from(value.clone()))
        }
        Some(LuaValue::STRING(path)) => {
            if cfg!(feature = "file-io") {
                let os_path = path.as_str()
                    .map(OsStr::new)
                    .ok_or(LuaError::new("Path must be valid UTF-8"))?;

                let file = File::create(os_path).map_err(|e| GenericError::String(format!("{}", e)))?;
                let handle = IOHandle::write_handle(file);

                let lua_value = LuaValue::from(UserData::new(handle, lua_vm));
                lua_vm.registry.raw_set("default_output", lua_value.clone());
                Ok(Varargs::from(lua_value))
            } else {
                Err(GenericError::Str("File-IO is not available").into())
            }
        }
        Some(LuaValue::NIL) | None => {
            Ok(Varargs::from(lua_vm.registry.raw_get("default_output")))
        }
        Some(lua_value) => Err(ArgumentError::InvalidValue {
            error: InvalidValueError {
                expected: "file",
                found: lua_value.type_name(),
            },
            argument_index: 0,
        })?,
    }
}

/// Opens specified script as new process, returning file handle to process stdio
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-io.popen>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn popen(_lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    todo!()
}

/// Reads from default file
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-io.read>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn read(_lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    todo!()
}

/// Creates a temporary file, opened in update mode
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-io.tmpfile>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn tmpfile(_lua_vm: &mut LuaVM, _params: &[LuaValue]) -> Result<Varargs, LuaError> {
    todo!()
}

/// Returns type of file (open/closed), or nil if passed argument is not a file
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-io.type>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn io_type(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    match params.first() {
        Some(LuaValue::USERDATA(userdata)) => {
            match userdata.downcast::<IOHandle>() {
                Err(_) => Ok(Varargs::fail()),
                Ok(handle) => {
                    if handle.is_closed(lua_vm) {
                        Ok(Varargs::from("closed file"))
                    } else {
                        Ok(Varargs::from("file"))
                    }
                }
            }
        }
        _ => Ok(Varargs::fail())
    }
}

/// Writes to default file
///
/// See: <https://www.lua.org/manual/5.4/manual.html#pdf-io.read>
///
/// # Arguments
///
/// * `lua_vm`: LuaVM for this call
/// * `params`: Parameters passed into function.
///
/// returns: Result<Varargs, LuaError>
///
pub fn write(lua_vm: &mut LuaVM, params: &[LuaValue]) -> Result<Varargs, LuaError> {
    output(lua_vm, &[])?
        .first()
        .clone()
        .method_call("write", lua_vm, params)
}

/// Adds the io library to the global environment of the specified LuaVM.
///
/// # Arguments
///
/// * `lua_vm`: LuaVM to add the io library to
///
/// returns: ()
pub fn insert_io_lib(lua_vm: &mut LuaVM) {
    let table = LuaTable::empty();

    set_table_func!(table, close);
    set_table_func!(table, flush);
    set_table_func!(table, input);
    set_table_func!(table, lines);
    set_table_func!(table, open);
    set_table_func!(table, output);
    set_table_func!(table, popen);
    set_table_func!(table, read);
    set_table_func!(table, tmpfile);
    set_table_func!(table, "type", io_type);
    set_table_func!(table, write);
    let stdin = UserData::new(IOHandle::stdin(), lua_vm);
    let stdout = UserData::new(IOHandle::stdout(), lua_vm);
    table.raw_set("stdin", stdin.clone());
    table.raw_set("stdout", stdout.clone());
    table.raw_set("stderr", UserData::new(IOHandle::stderr(), lua_vm));

    lua_vm.global_env.raw_set("io", table.clone());
    lua_vm.modules.insert("io", table);
    lua_vm.registry.raw_set("default_input", stdin);
    lua_vm.registry.raw_set("default_output", stdout);
    lua_vm.registry.raw_set("filehandle-metatable", FILEHANDLE_METATABLE());
}