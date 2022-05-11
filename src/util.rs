//! Module for various utility types

/// 2-type union with no greater meaning to each variant
pub enum Union2<T, U> {
    T(T),
    U(U),
}

/// 3-type union with no greater meaning to each variant
#[derive(Debug)]
pub enum Union3<T, U, V> {
    T(T),
    U(U),
    V(V),
}