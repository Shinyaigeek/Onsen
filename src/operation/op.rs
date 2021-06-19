use super::util::{grow_array, grow_capacity};
use super::value::{free_value_array, Value, ValueArray};

pub const OP_CONSTANT: u8 = 0;
pub const OP_RETURN: u8 = 1;
pub const OP_NEGATIVE: u8 = 2;

pub type OperationCode = u8;

pub struct Chunk {
    // instruction, and other data
    pub code: Vec<OperationCode>,
    // how many of those allocated entries are actually in use
    count: usize,
    // the number of elements in the array we have allocated
    capacity: usize,
    pub constants: ValueArray,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(0),
            count: 0,
            capacity: 0,
            constants: ValueArray::new(),
            lines: Vec::with_capacity(0),
        }
    }

    pub fn write(&mut self, bytes: OperationCode, line: usize) {
        if self.count >= self.capacity {
            let old_capacity = self.capacity;
            self.capacity = grow_capacity(self.capacity);
            self.code = grow_array(&mut self.code, self.capacity, old_capacity);
            self.lines = grow_array(&mut self.lines, self.capacity, old_capacity);
        }

        self.code.push(bytes);
        self.lines.push(line);
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

    pub fn disassemble_instruction(&self, offset: &mut usize) {
        print!("{}", format!("{:04} ", offset));

        let cur_code_line = self.lines.get(*offset as usize);
        let cur_code_line = match cur_code_line {
            Some(cur_code_line) => cur_code_line,
            None => {
                panic!(
                    "[disassemble_instruction] cur_code_line is none with offset {:?}",
                    offset
                )
            }
        };

        if *offset > 0 {
            let prev_code_line_idx = offset.clone() - 1;
            let prev_code_line = self.lines.get(prev_code_line_idx);
            let prev_code_line = match prev_code_line {
                Some(prev_code_line) => prev_code_line,
                None => {
                    panic!(
                        "[disassemble_instruction] prev_code_line is none with offset {:?}",
                        offset
                    )
                }
            };

            if cur_code_line == prev_code_line {
                print!("   | ");
            } else {
                print!("{}", format!("{:04} ", cur_code_line));
            }
        } else {
            print!("{}", format!("{:04} ", cur_code_line));
        }

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
            &OP_NEGATIVE => {
                Self::simple_instruction("OP_NEGATIVE", offset);
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

        print!("{}", format!("{:?} {:04} ", name, constant));

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

        chunk.write(OP_RETURN, 123);
        assert_eq!(chunk.capacity, 8);
        assert_eq!(chunk.count, 1);
    }

    #[test]
    fn write_constant_works() {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constants(1.2);
        assert_eq!(constant, 0);
        chunk.write(OP_CONSTANT, 123);
        // TODO re think
        chunk.write(constant as u8, 123);
        chunk.write(OP_RETURN, 123);
        assert_eq!(chunk.constants.count, 1);
        assert_eq!(chunk.constants.values, vec![1.2]);
    }
}
