use super::util::{grow_array, grow_capacity};
use super::value::{free_value_array, Value, ValueArray};

pub const OP_CONSTANT: u8 = 0;
pub const OP_RETURN: u8 = 1;

pub type OperationCode = u8;

pub struct Chunk {
    // instruction, and other data
    code: Vec<OperationCode>,
    // how many of those allocated entries are actually in use
    count: usize,
    // the number of elements in the array we have allocated
    capacity: usize,
    constants: ValueArray,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(0),
            count: 0,
            capacity: 0,
            constants: ValueArray::new(),
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

    // After we add the constant, we return the index where the constant was appended so that we can locate that same constant later.
    pub fn add_constants(&mut self, value: Value) -> usize {
        self.constants.write(value);
        self.constants.count - 1
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {:?} ==", name);

        let length = self.count;
        let mut offset = 0;

        while offset < length {
            self.disassemble_instruction(&mut offset)
        }
    }

    fn disassemble_instruction(&self, offset: &mut usize) {
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
            &OP_RETURN => {
                Self::simple_instruction("OP_RETURN", offset);
            }
            &OP_CONSTANT => {
                self.constant_instruction("OP_CONSTANT", offset);
            }
            _ => {
                panic!("unknown operation code {:?}", instruction);
                *offset += 1;
            }
        }
    }

    fn constant_instruction(&self, name: &str, offset: &mut usize) {
        let constant_index = offset.clone() + 1;
        let constant = self.code.get(constant_index);
        let constant = match constant {
            Some(constant) => constant,
            None => {
                panic!("there is no constant code at offset {:?}", offset);
            }
        };

        println!("{}", format!("{:?} {:04} ", name, constant));

        let value = self.constants.values.get(*constant as usize);
        let value = match value {
            Some(value) => value,
            None => {
                panic!("there is no value code at constant {:?}", constant);
            }
        };

        ValueArray::print_value(*value);
        println!("");
        // opcode, operands 2bytes
        *offset += 2;
    }

    fn simple_instruction(name: &str, offset: &mut usize) {
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

        chunk.write(OP_RETURN);
        assert_eq!(chunk.capacity, 8);
        assert_eq!(chunk.count, 1);
    }
}
