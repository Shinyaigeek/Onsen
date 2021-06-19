use super::util::{grow_array, grow_capacity};

pub type Value = f64;

pub struct ValueArray {
    pub count: usize,
    capacity: usize,
    pub values: Vec<Value>,
}

impl ValueArray {
    pub fn new() -> Self {
        Self {
            values: Vec::with_capacity(0),
            count: 0,
            capacity: 0,
        }
    }

    pub fn write(&mut self, value: Value) {
        if self.count >= self.capacity {
            let old_capacity = self.capacity;
            self.capacity = grow_capacity(self.capacity);
            self.values = grow_array(
                // TODO fixme, rethink about memory
                &mut self.values,
                self.capacity,
                old_capacity,
            );
        }

        self.values.push(value);
        self.count += 1;
    }

    pub fn print_value(value: Value) {
        println!("{:?}", value);
    }
}

pub fn free_value_array(value_array: ValueArray) {
    // do nothing for free Chunk
}
