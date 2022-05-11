//! Rust module for new pattern matching implementation

/// Performs a single pattern match
///
/// # Arguments
///
/// * `string`: String to search in
/// * `pattern`: Pattern to search for
///
/// returns: Result<(), ()>
///
pub fn match_pattern(_string: &[u8], pattern: &[u8]) -> Result<(), ()> {
    let pattern_index = 0usize;

    if pattern_index < pattern.len() {
        match pattern[pattern_index] {
            b'(' => {
                if pattern.get(pattern_index + 1).contains(&&b')') {
                    // position capture
                } else {
                    // Match capture
                }
            },
            b')' => {
                // End capture
            },
            b'$' if pattern_index + 1 == pattern.len() => {},
            b'%' => {},
            _ => {}
        }
    }

    Ok(())
}