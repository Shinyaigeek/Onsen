#[derive(Clone)]
pub enum OperationCode {
    OP_RETURN(u8),
}

pub struct Chunk {
    // instruction, and other data
    code: Vec<OperationCode>,
    // how many of those allocated entries are actually in use
    // TODO: usize?
    count: isize,
    // the number of elements in the array we have allocated
    capacity: isize,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(0),
            count: 0,
            capacity: 0,
        }
    }

    pub fn write(&mut self, bytes: OperationCode) {
        if self.count >= self.capacity {
            let old_capacity = self.capacity;
            self.capacity = grow_capacity(self.capacity);
            self.code = grow_array(
                // TODO fixme, rethink about memory
                self.code.clone(),
                self.capacity as usize,
                old_capacity as usize,
            );
        }

        self.code.push(bytes);
        self.count += 1;
    }
}

pub fn grow_capacity(capacity: isize) -> isize {
    if capacity < 8 {
        8
    } else {
        capacity * 2
    }
}

// todo should i use vec<T> ?
pub fn grow_array(
    array: Vec<OperationCode>,
    new_capacity: usize,
    old_capacity: usize,
) -> Vec<OperationCode> {
    if new_capacity == 0 {
        return Vec::with_capacity(0);
    }
    if new_capacity < old_capacity {
        panic!("[grow_array]: panicked because new_capacity < old_capacity");
    }
    let mut grown_array: Vec<OperationCode> = array.clone().drain(0..).collect();
    grown_array.reserve_exact(new_capacity - old_capacity);
    grown_array
}

pub fn free(chunk: Chunk) {
    // do nothing for free Chunk
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_works() {
        let mut chunk = Chunk::new();
        assert_eq!(chunk.capacity, 0);
        assert_eq!(chunk.count, 0);

        chunk.write(OperationCode::OP_RETURN(0));
        assert_eq!(chunk.capacity, 8);
        assert_eq!(chunk.count, 1);
    }
}
