pub enum OpCode {
    Load(usize, i64),
    Add(usize, usize, usize),
    Sub(usize, usize, usize),
    Div(usize, usize, usize),
    Mul(usize, usize, usize),
    Done(usize),
}
