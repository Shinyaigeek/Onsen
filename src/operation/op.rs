use super::util::{grow_array, grow_capacity};
use super::value::{ValueArray, free_value_array};

const OP_RETURN: &str = "OP_RETURN";

#[derive(Clone, Debug)]
pub enum OperationCode {
    OP_RETURN,
}

pub struct Chunk {
    // instruction, and other data
    code: Vec<OperationCode>,
    // how many of those allocated entries are actually in use
    count: usize,
    // the number of elements in the array we have allocated
    capacity: usize,
    constants: ValueArray
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(0),
            count: 0,
            capacity: 0,
            constants: ValueArray::new()
        }
    }

    pub fn write(&mut self, bytes: OperationCode) {
        if self.count >= self.capacity {
            let old_capacity = self.capacity;
            self.capacity = grow_capacity(self.capacity);
            self.code = grow_array(
                // TODO fixme, rethink about memory
                &mut self.code,
                self.capacity,
                old_capacity,
            );
        }

        self.code.push(bytes);
        self.count += 1;
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {:?} ==", name);

        let length = self.count;
        let mut offset = 0;

        while offset < length {
            self.disassembleInstruction(&mut offset)
        }
    }

    fn disassembleInstruction(&self, offset: &mut usize) {
        print!("{}", format!("{:04} ", offset));

        // TODO re thinking
        let instruction = self.code.get(offset.clone());
        let instruction = match instruction {
            Some(instruction) => instruction,
            None => {
                panic!("there is no operation code at offset {:?}", offset);
            }
        };

        match instruction {
            OperationCode::OP_RETURN => {
                Self::simpleInstruction(OP_RETURN, offset);
            }
            _ => {
                panic!("unknown operation code {:?}", instruction);
                *offset += 1;
            }
        }
    }

    fn simpleInstruction(name: &str, offset: &mut usize) {
        println!("{:?}\n", name);
        *offset += 1;
    }
}

pub fn free_chunk(chunk: Chunk) {
    free_value_array(chunk.constants);
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

        chunk.write(OperationCode::OP_RETURN);
        assert_eq!(chunk.capacity, 8);
        assert_eq!(chunk.count, 1);
    }
}
