use crate::bytecode::bytecode::opcode::OpCode;

const REGISTER_LENGTH: usize = 16;

pub struct VM {
    ip: u8,
    accumulator: u64,
    pc: u8,
    program: Vec<OpCode>,
    register: [i64; REGISTER_LENGTH],
}

impl VM {
    pub fn init() -> Self {
        Self {
            ip: 0,
            accumulator: 0,
            pc: 0,
            program: Vec::new(),
            register: [0; REGISTER_LENGTH],
        }
    }
}
