use crate::operation::op::{free_chunk, Chunk, OperationCode};
mod operation;

fn assemble() {
    let mut chunk = Chunk::new();
    chunk.write(OperationCode::OP_RETURN);
    chunk.disassemble("test chunk");
    free_chunk(chunk);
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        assemble();
        assert_eq!(2 + 2, 4);
    }
}
