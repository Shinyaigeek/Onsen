use crate::operation::op::{free_chunk, Chunk, OperationCode};
mod operation;

fn main() {
    let mut chunk = Chunk::new();
    chunk.write(OperationCode::OP_RETURN);
    chunk.disassemble("test chunk");
    free_chunk(chunk);
}
