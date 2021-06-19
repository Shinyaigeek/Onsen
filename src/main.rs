use crate::operation::op::{free, Chunk, OperationCode};
mod operation;

fn main() {
    let mut chunk = Chunk::new();
    chunk.write(OperationCode::OP_RETURN);
    chunk.disassemble("test chunk");
    free(chunk);
}
