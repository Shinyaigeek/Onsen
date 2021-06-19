use crate::operation::op::{Chunk, OP_CONSTANT, OP_RETURN};
use crate::operation::value::{Value, ValueArray};

const STACK_MAX: usize = 256;

pub struct VM {
    chunk: Option<Chunk>,

    // TODO actual, this is not ip? this should be pointer
    ip: Option<Vec<u8>>,
    ip_idx: usize,
    stack: Vec<Value>,
    // TODO this should be pointer
    stack_top_idx: usize,
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: None,
            ip: None,
            ip_idx: 0,
            stack: Vec::with_capacity(STACK_MAX),
            stack_top_idx: 0,
        }
    }
    pub fn init(&mut self) {
        self.reset_stack();
    }
    pub fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        let code = chunk.code.clone();
        println!("code: {:?}", code);
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
            let chunk = match &self.chunk {
                Some(chunk) => chunk,
                None => panic!("[run]run is invoked with self.chunk is none"),
            };
            print!("          ");
            for slot in &self.stack {
                print!("[ {:?} ]", slot);
            }
            println!("");
            chunk.disassemble_instruction(&mut self.ip_idx.clone());

            let instruction = self.read_bytes();
            match instruction {
                OP_RETURN => {
                    ValueArray::print_value(self.pop_stack());
                    println!("");
                    return InterpretResult::INTERPRET_OK;
                }

                OP_CONSTANT => {
                    let constant = self.read_constants();
                    self.push_stack(constant);
                }
                OP_NEGATIVE => {
                    let value = self.pop_stack();
                    self.push_stack((-1 as f64) * value);
                }
                // TODO I want macro
                OP_ADD => {
                    let r = self.pop_stack();
                    let l = self.pop_stack();
                    self.push_stack(l + r);
                }
                OP_SUBTRACT => {
                    let r = self.pop_stack();
                    let l = self.pop_stack();
                    self.push_stack(l - r);
                }
                OP_MULTIPLY => {
                    let r = self.pop_stack();
                    let l = self.pop_stack();
                    self.push_stack(l * r);
                }
                OP_DIVIDE => {
                    let r = self.pop_stack();
                    let l = self.pop_stack();
                    self.push_stack(l / r);
                }
                _ => {
                    panic!("unknown operation code {:?}", instruction);
                }
            }
        }
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
        self.stack_top_idx = 0;
    }

    fn push_stack(&mut self, value: Value) {
        self.stack_top_idx += 1;
        self.stack.push(value);
    }

    fn pop_stack(&mut self) -> Value {
        self.stack_top_idx -= 1;
        match self.stack.pop() {
            Some(v) => v,
            None => panic!("[pop_stack] vm.pop_stack is called when stack does not have any value"),
        }
    }

    fn read_bytes(&mut self) -> u8 {
        let byte = match &self.ip {
            Some(ip) => ip[self.ip_idx],
            None => panic!("[read_bytes]read_bytes is invoked with self.ip is none"),
        };
        self.ip_idx += 1;
        byte
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
