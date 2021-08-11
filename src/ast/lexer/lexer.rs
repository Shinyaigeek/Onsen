use crate::utils::number::Number;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: &String) -> Self {
        let bytes = &input.as_bytes();
        Lexer {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: 0,
        }
    }

    pub fn read_char(&mut self) -> u8 {
        if self.read_position >= self.input.len() {
            // * EOF
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;

        self.ch
    }

    pub fn peek(&self) -> u8 {
        if self.input.as_bytes().len() <= self.read_position {
            // * EOF
            return 0;
        }

        self.input.as_bytes()[self.read_position]
    }

    pub fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();
        loop {
            let next_ch = self.peek();
            if !Lexer::is_letter(next_ch) {
                break;
            }

            let next_ch = self.read_char();
            identifier.push(next_ch as char);
        }

        identifier
    }

    pub fn read_number(&mut self) -> Number {
        let mut identifier = String::new();
        loop {
            let next_ch = self.peek();
            if !Lexer::is_digit(next_ch) && !Lexer::is_period(next_ch) {
                break;
            }

            let next_ch = self.read_char();
            identifier.push(next_ch as char);
        }

        Number::from_str(identifier)
    }

    pub fn is_letter(ch: u8) -> bool {
        (b'a' <= ch && ch <= b'z') || (b'A' <= ch && ch <= b'Z') || ch == b'_'
    }

    pub fn is_identifier_end(ch: u8) -> bool {
        ch == b' ' || ch == b'\t' || ch == b'\n' || ch == b'\r' || ch == 0
    }

    pub fn is_digit(ch: u8) -> bool {
        b'0' <= ch && ch <= b'9'
    }

    pub fn is_period(ch: u8) -> bool {
        b'.' == ch
    }

    pub fn eat_white_space(&mut self) {
        loop {
            let next_ch = self.peek();
            if !Lexer::is_identifier_end(next_ch) {
                break;
            }

            // EOF
            if next_ch == 0 {
                break;
            }

            let next_ch = self.read_char();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_read() {
        let input = "1 2 3".to_string();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.read_char(), b'1');
        lexer.eat_white_space();
        assert_eq!(lexer.read_char(), b'2');
        lexer.eat_white_space();
        assert_eq!(lexer.read_char(), b'3');
        lexer.eat_white_space();
        assert_eq!(lexer.read_char(), 0);
    }

    #[test]
    fn test_peek() {
        let input = "1 2 3".to_string();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.read_char(), b'1');
        lexer.eat_white_space();
        assert_eq!(lexer.peek(), b'2');
        assert_eq!(lexer.read_char(), b'2');
    }

    #[test]
    fn test_read_identifier() {
        let input = "foo 1 bar".to_string();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.read_identifier(), "foo");
        lexer.eat_white_space();
        lexer.read_number();
        lexer.eat_white_space();
        assert_eq!(lexer.read_identifier(), "bar");
    }

    #[test]
    fn test_read_number() {
        let input = "1 2.3".to_string();
        let mut lexer = Lexer::new(&input);
        assert_eq!(
            lexer.read_number().to_string(),
            Number::from_str("1".to_string()).to_string()
        );
        lexer.eat_white_space();
        assert_eq!(
            lexer.read_number().to_string(),
            Number::from_str("2.3".to_string()).to_string()
        );
    }
}
