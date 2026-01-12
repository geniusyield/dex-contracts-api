//// Utility functions for working with `Option` types.

/// Assert that an option is `Some`, returning the value inside.
pub fn assert(val: Option<a>) -> a {
  when val is {
    None -> fail @"Expected Some"
    Some(a) -> a
  }
}
