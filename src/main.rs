use crate::operation::op::{free_chunk, Chunk, OP_CONSTANT, OP_RETURN};
mod operation;

fn main() {
    let mut chunk = Chunk::new();
    let constant = chunk.add_constants(1.2);
    chunk.write(OP_CONSTANT);
    // TODO re think
    chunk.write(constant as u8);
    chunk.write(OP_RETURN);
    chunk.disassemble("test chunk");
    free_chunk(chunk);
}
