use crate::operation::op::{Chunk, OP_CONSTANT, OP_RETURN};
use crate::operation::value::{Value, ValueArray};

pub struct VM {
    chunk: Option<Chunk>,

    // actual, this is not ip?
    ip: Option<Vec<u8>>,
    ip_idx: usize,
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: None,
            ip: None,
            ip_idx: 0,
        }
    }
    pub fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        let code = chunk.code.clone();
        // TODO
        // store the chunk which will be executed by this vm
        self.chunk = Some(chunk);
        // the location of instructions which is executed by this vm, first bytes of chunks
        self.ip = Some(code);
        self.ip_idx = 0;
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let byte = match &self.ip {
                Some(ip) => ip[self.ip_idx],
                None => panic!("[read_bytes]read_bytes is invoked with self.ip is none"),
            };
            match byte {
                OP_RETURN => return InterpretResult::INTERPRET_OK,

                OP_CONSTANT => {
                    let constant = self.read_constants();
                    ValueArray::print_value(constant);
                    println!("");
                }
                _ => {
                    panic!("unknown operation code {:?}", byte);
                }
            }

            self.ip_idx += 1;
        }
    }

    fn read_bytes(&mut self) -> u8 {
        let byte = match &self.ip {
            Some(ip) => ip[self.ip_idx],
            None => panic!("[read_bytes]read_bytes is invoked with self.ip is none"),
        };
        let old_byte = byte.clone();
        self.ip_idx += 1;
        old_byte
    }

    fn read_constants(&mut self) -> Value {
        let cur_byte = self.read_bytes();
        let chunk = match &self.chunk {
            Some(chunk) => chunk,
            None => panic!("[read_constants]read_constants is invoked with self.chunk is none"),
        };
        let value = chunk.constants.values.get(cur_byte as usize);
        match value {
            Some(value) => *value,
            None => {
                panic!("[read_constants]: there is no constants on {:?}", cur_byte)
            }
        }
    }
}

pub fn free_vm(vm: VM) {
    // do nothing
}

const INTERPRET_OK: &str = "INTERPRET_OK";
const INTERPRET_COMPILE_ERROR: &str = "INTERPRET_COMPILE_ERROR";
const INTERPRET_RUNTIME_ERROR: &str = "INTERPRET_RUNTIME_ERROR";

pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
}
