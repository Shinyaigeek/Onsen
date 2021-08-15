use crate::operation::op::{
    free_chunk, Chunk, OP_ADD, OP_CONSTANT, OP_DIVIDE, OP_NEGATIVE, OP_RETURN,
};
use crate::vm::vm::{free_vm, VM};
mod ast;
mod bytecode;
mod operation;
mod utils;
mod vm;

fn main() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();
    let constant = chunk.add_constants(1.2);
    chunk.write(OP_CONSTANT, 123);
    // TODO re think
    chunk.write(constant as u8, 123);

    let constant = chunk.add_constants(3.8);
    chunk.write(OP_CONSTANT, 123);
    chunk.write(constant as u8, 123);

    chunk.write(OP_ADD, 123);

    let constant = chunk.add_constants(9.7);
    chunk.write(OP_CONSTANT, 123);
    chunk.write(constant as u8, 123);

    chunk.write(OP_DIVIDE, 123);

    chunk.write(OP_NEGATIVE, 123);
    chunk.write(OP_RETURN, 123);
    vm.interpret(chunk);
    free_vm(vm);
}
