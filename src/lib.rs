#![feature(option_result_contains)]
#![feature(stmt_expr_attributes)]
#![feature(try_blocks)]
#![feature(type_ascription)]
#![feature(int_log)]
#![feature(result_option_inspect)]

extern crate nom;
extern crate core;

#[macro_use]
mod macros;

#[allow(unused_variables, dead_code)]
pub mod types;
#[allow(unused_variables, dead_code)]
pub mod bytecode;
mod compiler;
pub mod vm;
pub mod constants;
mod util;
pub mod stdlib;
mod error;

#[cfg(test)]
mod test;
