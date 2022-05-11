//! Module for Lua 'table' type

use std::rc::Rc;
use std::cell::{RefCell, Ref};
use crate::types::value::{LuaValue, LuaKey};
use std::fmt;
use crate::constants::types::{LUA_INT, LUA_FLOAT};
use std::fmt::{Display, Formatter, Debug};
use crate::types::{AsLuaPointer, ref_to_pointer, LuaType, CoerceFrom};
use crate::types::value::table::table_impl::TableImpl;
use crate::types::value::number::LuaNumber;

/// Module containing table implementation
mod table_impl {
    use std::collections::HashMap;
    use crate::types::value::{LuaValue, LuaKey};
    use std::collections::hash_map::RandomState;

    /// Lua table implementation
    /// 'array' contains the array-part of the LuaTable
    /// 'map' contains the hash-part of the LuaTable
    /// 'key_array' is a mirror of the map's keys, used to speed up iteration
    #[derive(Debug)]
    pub struct TableImpl {
        pub array: Vec<LuaValue>,
        map: HashMap<LuaKey, LuaValue>,
        key_array: Vec<LuaKey>,
    }

    impl TableImpl {
        pub fn new() -> TableImpl {
            TableImpl {
                array: vec![],
                map: HashMap::new(),
                key_array: vec![],
            }
        }

        pub fn with_capacity(array_capacity: usize, hash_capacity: usize) -> TableImpl {
            TableImpl {
                map: HashMap::with_capacity(hash_capacity),
                array: Vec::with_capacity(array_capacity),
                key_array: Vec::with_capacity(hash_capacity),
            }
        }

        pub(super) fn map(&self) -> &HashMap<LuaKey, LuaValue, RandomState> {
            &self.map
        }

        pub(super) fn map_insert(&mut self, key: LuaKey, value: LuaValue) -> Option<LuaValue> {
            let prev_value = self.map.insert(key.clone(), value);
            if prev_value.is_none() {
                self.key_array.push(key);
            }
            prev_value
        }

        pub(super) fn map_remove(&mut self, key: &LuaKey) -> Option<LuaValue> {
            let prev_value = self.map.remove(&key);
            if prev_value.is_some() {
                self.key_array.swap_remove(
                    self.key_array.iter()
                        .position(|k| k == key)
                        .expect("Key array must contain keys for all values")
                );
            }
            prev_value
        }

        pub(super) fn map_key_at_iter_index(&self, index: &mut usize) -> Option<&LuaKey> {
            while let Some(key) = self.key_array.get(*index) {
                *index += 1;
                if key.inner == LuaValue::NIL {
                    continue;
                } else {
                    return Some(key);
                }
            }
            None
        }
    }
}

/// Lua table
///
/// Contains both an array (keys 1..=N) part and a hash (all other keys) part.
///
/// Values must be accessed through the [`LuaKey`] type as not all LuaValues (NaN, NIL) are valid keys, which may be obtained from [`LuaValue::try_key`]
#[derive(Clone)]
pub struct LuaTable {
    inner: Rc<(RefCell<TableImpl>, RefCell<Option<LuaTable>>)>,   // Where inner.0 = TableImpl, and inner.1 = table's metatable
}

impl LuaTable {
    /// Creates an empty LuaTable. This allocates on heap
    pub fn empty() -> LuaTable {
        LuaTable {
            inner: Rc::new((RefCell::new(TableImpl::new()), RefCell::from(None)))
        }
    }

    /// Creates an empty LuaTable with specified capacities. This allocates on heap
    ///
    /// # Arguments
    ///
    /// * `array_capacity`: Size of array-part, resulting array-part will cover 1..=N
    /// * `hash_capacity`: Capacity of hash-part
    ///
    /// returns: LuaTable
    pub fn with_capacity(array_capacity: usize, hash_capacity: usize) -> LuaTable {
        LuaTable {
            inner: Rc::new((RefCell::new(TableImpl::with_capacity(array_capacity, hash_capacity)), RefCell::from(None)))
        }
    }

    /// Creates LuaTable with specified values at indices 1..=N
    ///
    /// # Arguments
    ///
    /// * `values`: Values to add to table
    ///
    /// returns: LuaTable
    pub fn of_list<T: Into<LuaValue>, const N: usize>(values: [T; N]) -> LuaTable {
        LuaTable::with_list_at_index(values, 1)
    }

    /// Creates LuaTable with specified values at indices `index`..=`index+N`
    ///
    /// # Arguments
    ///
    /// * `values`: Values to add to table
    /// * `index`: Start-index of list
    ///
    /// returns: LuaTable
    pub fn with_list_at_index<T: Into<LuaValue>, const N: usize>(values: [T; N], mut index: LUA_INT) -> LuaTable {
        let table = LuaTable::with_capacity(N, 0);

        for value in values {
            table.raw_set(index, value);
            index += 1;
        }
        table
    }

    /// Creates LuaTable with specified keys and values
    ///
    /// # Arguments
    ///
    /// * `entries`: List of key-value pairs
    ///
    /// returns: Result<LuaTable, InvalidKeyError>
    pub fn of_map<K: Into<LuaKey>, V: Into<LuaValue>, const N: usize>(entries: [(K, V); N]) -> LuaTable {
        let table = LuaTable::with_capacity(0, N);
        for (key, value) in entries {
            table.raw_set(key, value.into());
        }
        table
    }

    // Note: DO NOT ENTER KEY/VALUE REFCELLS IN THIS METHOD
    /// Retrieves value from table, returning nil if no value was set for specified key
    ///
    /// Does not follow `__index` metamethod, if `__index` support is required [`LuaValue::index`] must be used
    ///
    /// # Arguments
    ///
    /// * `key`: Key to lookup
    ///
    /// returns: LuaValue
    pub fn raw_get<'a, K: Into<LuaKey>>(&self, key: K) -> LuaValue {
        let key = key.into();

        let (inner, _) = &*self.inner;
        let table = inner.borrow_mut();
        match key.inner {
            LuaValue::NUMBER(num) if num.try_int().map(|i| { i > 0 && ((i as usize - 1) < table.array.len()) }).unwrap_or(false) => {
                let index = num.try_int().unwrap() as usize - 1;
                if let Some(val) = table.array.get(index) {
                    if val != &LuaValue::NIL {
                        return val.clone();
                    }
                }
            }
            _ => {
                let mut key = key;
                if let LuaValue::NUMBER(LuaNumber::FLOAT(float)) = key.inner {    // coerce integer-valued floats to integer; This ensures table access respects the same numerical value
                    if float as LUA_INT as LUA_FLOAT == float {
                        key.inner = LuaValue::from(float as LUA_INT)
                    }
                }
                if let Some(val) = table.map().get(&key) {
                    if val != &LuaValue::NIL {
                        return val.clone();
                    }
                }
            }
        };
        LuaValue::NIL
    }

    /// Sets value in table, or removes mapping if value is nil
    ///
    /// Does not follow `__newindex` metamethod, if `__newindex` support is required [`LuaValue::set_index`] must be used
    ///
    /// # Arguments
    ///
    /// * `key`: Key to write to
    /// * `value`: Value to set, or nil to clear value
    ///
    /// returns: ()
    // TODO: Ensure table ownership is an acyclic-graph
    pub fn raw_set<K: Into<LuaKey>, V: Into<LuaValue>>(&self, key: K, value: V) {
        let mut key = key.into();
        let value = value.into();

        if let LuaValue::NUMBER(LuaNumber::FLOAT(float)) = key.inner {    // coerce integer-valued floats to integer; This ensures table access respects the same numerical value
            if float as LUA_INT as LUA_FLOAT == float {
                key.inner = LuaValue::from(float as LUA_INT)
            }
        }
        let (inner, _) = &*self.inner;
        let table = &mut *inner.borrow_mut();

        match key.inner {
            // In existing array-part
            LuaValue::NUMBER(num) if num.try_int().map(|i| { i > 0 && ((i as usize - 1) < table.array.len()) }).unwrap_or(false) => {
                let index = num.try_int().unwrap() as usize - 1; // We already checked num is greater than 0
                if value == LuaValue::NIL && index == table.array.len() - 1 {
                    table.array.truncate(table.array.len() - 1);
                    while let Some(LuaValue::NIL) = table.array.last() {
                        table.array.truncate(table.array.len() - 1);
                    }
                } else {
                    *table.array.get_mut(index).unwrap() = value;
                }
            }
            // Append to array-part
            LuaValue::NUMBER(num) if num.try_int().map(|i| { i > 0 && ((i as usize - 1) == table.array.len()) }).unwrap_or(false) => {
                if value != LuaValue::NIL {
                    if table.map().contains_key(&key) {
                        table.map_remove(&key);
                    }
                    table.array.push(value)
                } else if table.map().contains_key(&key) {
                    table.map_remove(&key);
                }
            }
            _ => {
                if value == LuaValue::NIL {
                    table.map_remove(&key);
                } else {
                    table.map_insert(key, value);
                }
            }
        };
    }

    /// Indexes metatable of this table, returning nil if value has no metatable, or metatable has no field for key
    pub fn index_metatable<'a, K: Into<LuaKey>>(&self, key: K) -> LuaValue {
        self.inner.1.borrow().as_ref().map(|table| table.raw_get(key)).unwrap_or(LuaValue::NIL)  // Metatable lookups are a raw get
    }

    /// Returns a copy of this table's metatable
    pub fn clone_metatable(&self) -> Option<LuaTable> {
        self.inner.1.borrow().as_ref().cloned()
    }

    /// Sets this table's metatable, does not respect __metatable metamethod
    pub fn set_metatable_raw(&self, metatable: LuaTable) {
        self.inner.1.replace(Some(metatable));
    }

    // TODO: Length is technically defined as "N where N != nil && N+1 = nil"; Implementation may have to switch to just array-part's length
    /// Size of this table
    ///
    /// WARNING: Currently returns total amount of values in table, subject to future change.
    pub fn len(&self) -> usize {
        let (inner, _) = &*self.inner;
        let tab = inner.borrow();
        tab.array.len() + tab.map().len()
    }

    /// Iterator over this table
    pub fn iter(&self) -> TableIterator {
        TableIterator::from(self)
    }


    /// Returns next key and value after the specified key
    ///
    /// # Arguments
    ///
    /// * `index`: Key to look up
    ///
    /// returns: Option<(LuaValue, LuaValue)>
    pub fn next(&self, index: &LuaValue) -> Option<(LuaValue, LuaValue)> {  // TODO: replace with performance-optimized implementation; Current implementation is extremely slow
        let table = &self.inner.0.borrow();
        let mut iter = table.map().iter()
            .map(|key| (key.0.inner.clone(), key.1))
            .chain(table.array.iter().enumerate().map(|key_value| (LuaValue::from((key_value.0 + 1) as LUA_INT), key_value.1)));
        if index != &LuaValue::NIL {
            while let Some(key_value) = iter.next() {
                if &key_value.0 == index { break; };
            }
        }
        let next_value = iter.next().map(|key_value| (key_value.0, key_value.1.clone()));
        next_value
    }
}

impl LuaType for LuaTable {
    const TYPE_NAME: &'static str = "table";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaTable {
    fn coerce_opt(value: &T) -> Option<Self> {
        if let LuaValue::TABLE(table) = value.clone().into() {
            Some(table)
        } else {
            None
        }
    }
}

impl AsLuaPointer for LuaTable {
    fn as_lua_pointer(&self) -> usize {
        ref_to_pointer(self.inner.as_ref())
    }
}

impl Debug for LuaTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "table:{:X}", self.as_lua_pointer())
    }
}

impl Display for LuaTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "table:{:X}", self.as_lua_pointer())
    }
}

impl PartialEq for LuaTable {
    fn eq(&self, other: &Self) -> bool {
        self.as_lua_pointer() == other.as_lua_pointer()
    }
}


/// Iterator over LuaTable
pub struct TableIterator<'a> {
    inner: &'a LuaTable,
    table_impl: Ref<'a, TableImpl>,
    in_map: bool,
    index: usize,
}

impl<'a> Iterator for TableIterator<'a> {
    type Item = (LuaValue, LuaValue);

    fn next(&mut self) -> Option<Self::Item> {
        if self.in_map {
            match self.table_impl.map_key_at_iter_index(&mut self.index) {
                None => {
                    self.in_map = false;
                    self.index = 0;
                    self.next()
                }
                Some(key) => {
                    Some(
                        (
                            key.inner.clone(),
                            self.table_impl.map()
                                .get(key)
                                .expect("key retrieved from map!")
                                .clone()
                        )
                    )
                }
            }
        } else {
            let value = self.table_impl.array.get(self.index)
                .map(|value| (LuaValue::from(self.index), value.clone()));
            self.index += 1;
            value
        }
    }
}

impl TableIterator<'_> {
    pub fn from(table: &LuaTable) -> TableIterator {
        TableIterator {
            inner: table,
            table_impl: table.inner.0.borrow(),
            in_map: true,
            index: 0,
        }
    }
}