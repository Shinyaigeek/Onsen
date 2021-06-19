use crate::operation::op::{free_chunk, Chunk, OP_CONSTANT, OP_RETURN};
use crate::vm::vm::{free_vm, VM};
mod operation;
mod vm;

fn main() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();
    let constant = chunk.add_constants(1.2);
    chunk.write(OP_CONSTANT, 123);
    // TODO re think
    chunk.write(constant as u8, 123);
    chunk.write(OP_RETURN, 123);
    vm.interpret(chunk);
    free_vm(vm);
}
