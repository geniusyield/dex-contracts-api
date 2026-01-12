//// Types & corresponding utilities related to sorted lists.

/// An opaque type that represents a sorted list of elements.
pub opaque type SortedList<a> {
  inner: List<a>,
}

/// Create an empty sorted list.
pub const empty: SortedList<a> = SortedList { inner: [] }

/// Get the underlying list.
pub fn to_list(self: SortedList<a>) -> List<a> {
  self.inner
}

/// Insert an element into sorted list.
pub fn insert(
  self: SortedList<a>,
  elem: a,
  cmp: fn(a, a) -> Ordering,
) -> SortedList<a> {
  SortedList { inner: self.inner |> do_insert(elem, cmp) }
}

fn do_insert(self: List<a>, elem: a, cmp: fn(a, a) -> Ordering) -> List<a> {
  when self is {
    [] -> [elem]
    [x, ..xs] ->
      when cmp(elem, x) is {
        Less -> [elem, ..self]
        Equal -> self
        Greater -> [x, ..do_insert(xs, elem, cmp)]
      }
  }
}

// TODO: Write unit tests for every function!
// FIXME: Remove `verify_sorted` if we don't rely on assumption of list being sorted for entries inside multisig.
/// Verify if the list is sorted.
pub fn verify_sorted(self: List<a>, cmp: fn(a, a) -> Ordering) -> Bool {
  when self is {
    [] -> True
    [_] -> True
    [x, y, ..ys] -> cmp(x, y) != Greater && verify_sorted([y, ..ys], cmp)
  }
}

/// Construct a sorted list from a list. This doesn't do any validation to check if the list is sorted.
pub fn unsafe_from_list(self: List<a>) -> SortedList<a> {
  SortedList { inner: self }
}
