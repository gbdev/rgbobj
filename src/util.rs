/// Reimplementation of [`slice::partition_point`], which is currently in Nightly.
/// Assuming all elements for which `pred` returns true are at the beginning of the slice, finds the
/// index of the first element for which this isn't true (which can be one past the slice's end).
pub fn partition_point<T, P: FnMut(&T) -> bool>(slice: &[T], mut pred: P) -> usize {
    let mut a = 0;
    let mut b = slice.len();

    while a != b {
        let mid = (a + b) / 2;
        if pred(&slice[mid]) {
            a = mid + 1;
        } else {
            b = mid;
        }
    }
    a
}
