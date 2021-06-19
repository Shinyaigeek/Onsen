use crate::operation::op::{free, Chunk, OperationCode};
mod operation;

fn assemble() {
    let mut chunk = Chunk::new();
    chunk.write(OperationCode::OP_RETURN);
    chunk.disassemble("test chunk");
    free(chunk);
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
