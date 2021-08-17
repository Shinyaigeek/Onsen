use crate::bytecode::bytecode::opcode::OpCode;
use std::result::Result;

#[derive(Debug)]
pub enum ProgramError {
    DivisionByZero,
    UnexpectedTermination,
    // UnknownOpcode, // compiler ensures this can never happen
}

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

    pub fn execute(&mut self) -> Result<i64, ProgramError> {
        for opcode in &self.program {
            match opcode {
                OpCode::Load(source_idx, value) => {
                    self.register[*source_idx] = *value;
                }
                OpCode::Add(source_l_idx, source_r_idx, destination_idx) => {
                    self.register[*destination_idx] =
                        self.register[*source_l_idx] + self.register[*source_r_idx];
                }
                OpCode::Sub(source_l_idx, source_r_idx, destination_idx) => {
                    self.register[*destination_idx] =
                        self.register[*source_l_idx] - self.register[*source_r_idx];
                }
                OpCode::Mul(source_l_idx, source_r_idx, destination_idx) => {
                    self.register[*destination_idx] =
                        self.register[*source_l_idx] * self.register[*source_r_idx];
                }
                OpCode::Div(source_l_idx, source_r_idx, destination_idx) => {
                    if (self.register[*source_r_idx] == 0) {
                        return Err(ProgramError::DivisionByZero);
                    }
                    self.register[*destination_idx] =
                        self.register[*source_l_idx] / self.register[*source_r_idx];
                }
                OpCode::Done(source_idx) => {
                    return Ok(self.register[*source_idx]);
                }
            }

            self.ip += 1;
        }

        Err(ProgramError::UnexpectedTermination)
    }
}
