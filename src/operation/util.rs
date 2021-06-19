pub fn grow_capacity(capacity: usize) -> usize {
    if capacity < 8 {
        8
    } else {
        capacity * 2
    }
}

pub fn grow_array<T>(array: &mut Vec<T>, new_capacity: usize, old_capacity: usize) -> Vec<T> {
    if new_capacity == 0 {
        return Vec::with_capacity(0);
    }
    if new_capacity < old_capacity {
        panic!("[grow_array]: panicked because new_capacity < old_capacity");
    }
    let mut grown_array: Vec<T> = array.drain(0..).collect();
    grown_array.reserve_exact(new_capacity - old_capacity);
    grown_array
}
