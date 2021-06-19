use crate::utils::op::{free, Chunk, OperationCode};
mod utils;

fn assemble() {
    let mut chunk = Chunk::new();
    chunk.write(OperationCode::OP_RETURN(0));
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
