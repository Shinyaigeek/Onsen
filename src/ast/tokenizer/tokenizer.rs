use crate::ast::lexer::lexer::Lexer;

pub const EOF: u8 = 0;
pub const ASSIGN: u8 = b'=';
pub const PLUS: u8 = b'+';
pub const MINUS: u8 = b'-';
pub const ASTERISK: u8 = b'*';
pub const SLASH: u8 = b'/';
pub const PERIOD: u8 = b'.';
pub const COMMA: u8 = b',';
pub const COLON: u8 = b':';
pub const SEMICOLON: u8 = b';';
pub const LPAREN: u8 = b'(';
pub const RPAREN: u8 = b')';
pub const LBRACE: u8 = b'{';
pub const RBRACE: u8 = b'}';
pub const LBRACKET: u8 = b'[';
pub const RBRACKET: u8 = b']';
pub const GRATER: u8 = b'>';
pub const LESS: u8 = b'<';
pub const QUOTE: u8 = b'"';
pub const SQUOTE: u8 = b'\'';
pub const TQUOTE: u8 = b'`';
pub const EXCLAMATION: u8 = b'!';

pub const LET: &str = "let";
pub const FUNCTION: &str = "function";
pub const IDENTIFIER: &str = "IDENTIFIER";
pub const INT: &str = "INT";
pub const FLOAT: &str = "FLOAT";
pub const RETURN: &str = "return";
pub const TRUE: &str = "true";
pub const FALSE: &str = "false";
pub const IF: &str = "if";
pub const ELSE: &str = "else";
pub const EQUAL: &str = "==";
pub const NOT_EQUAL: &str = "!=";
pub const MUTATE: &str = "mut";
pub const GRATER_EQUAL: &str = ">=";
pub const LESS_EQUAL: &str = "<=";
pub const ARROW: &str = "=>";

pub const ILLEGAL: &str = "ILLEGAL";

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    EOF,
    ASSIGN,
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    PERIOD,
    COMMA,
    COLON,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    GRATER,
    LESS,
    QUOTE,
    SQUOTE,
    TQUOTE,
    EXCLAMATION,
    LET,
    FUNCTION,
    IDENTIFIER,
    INT,
    FLOAT,
    RETURN,
    TRUE,
    FALSE,
    IF,
    ELSE,
    MUTATE,
    EQUAL,
    NOT_EQUAL,
    GRATER_EQUAL,
    LESS_EQUAL,
    ARROW,
    ILLEGAL,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
}

impl Token {
    pub fn new(token_type: TokenType, bytes: Vec<u8>) -> Self {
        Token {
            token_type,
            value: String::from_utf8(bytes).unwrap(),
        }
    }

    // ! THIS IS A METHOD FOR TEST, DON'T USE THIS METHOD
    pub fn __raw_new_(token_type: TokenType, value: String) -> Self {
        Self { token_type, value }
    }

    //TODO is this ok
    pub fn copy_token(token: &Token) -> Self {
        Self {
            token_type: token.token_type.clone(),
            value: token.value.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Tokens {
    // TODO is this ok?
    tokens: Vec<Token>,
    lexer: Lexer,
    read_position: usize,
    position: usize,
}

impl Tokens {
    pub fn new(lexer: Lexer) -> Self {
        let mut token = Tokens {
            tokens: Vec::new(),
            lexer,
            read_position: 1,
            position: 0,
        };

        token.tokenize();

        token
    }

    pub fn read_token(&mut self) -> &Token {
        if self.read_position >= self.tokens.len() {
            // TODO is this ok?
            panic!("there is no more token!!");
        }

        self.position = self.read_position;
        self.read_position += 1;

        self.tokens.get(self.position).unwrap()
    }

    pub fn peek_token(&self) -> &Token {
        if self.read_position >= self.tokens.len() {
            // TODO is this ok?
            panic!("there is no more token!!");
        }

        self.tokens.get(self.read_position).unwrap()
    }

    pub fn cur_token(&self) -> &Token {
        if self.position >= self.tokens.len() {
            // TODO is this ok?
            panic!("there is no more token!!");
        }

        self.tokens.get(self.position).unwrap()
    }

    fn tokenize(&mut self) {
        loop {
            let ch = self.lexer.peek();

            let token = match ch {
                ASSIGN => {
                    let ch = self.lexer.read_char();
                    let next_ch = self.lexer.peek();
                    //* ==
                    if next_ch == ASSIGN {
                        Token::new(TokenType::EQUAL, vec![ch, self.lexer.read_char()])
                        //* =>
                    } else if next_ch == GRATER {
                        Token::new(TokenType::ARROW, vec![ch, self.lexer.read_char()])
                    } else {
                        Token::new(TokenType::ASSIGN, vec![ch])
                    }
                }
                EXCLAMATION => {
                    let ch = self.lexer.read_char();
                    let next_ch = self.lexer.peek();
                    //* !=
                    if next_ch == ASSIGN {
                        Token::new(TokenType::NOT_EQUAL, vec![ch, self.lexer.read_char()])
                        //* !
                    } else {
                        Token::new(TokenType::EXCLAMATION, vec![ch])
                    }
                }
                PLUS => Token::new(TokenType::PLUS, vec![self.lexer.read_char()]),
                MINUS => Token::new(TokenType::MINUS, vec![self.lexer.read_char()]),
                ASTERISK => Token::new(TokenType::ASTERISK, vec![self.lexer.read_char()]),
                SLASH => Token::new(TokenType::SLASH, vec![self.lexer.read_char()]),
                PERIOD => Token::new(TokenType::PERIOD, vec![self.lexer.read_char()]),
                COMMA => Token::new(TokenType::COMMA, vec![self.lexer.read_char()]),
                COLON => Token::new(TokenType::COLON, vec![self.lexer.read_char()]),
                SEMICOLON => Token::new(TokenType::SEMICOLON, vec![self.lexer.read_char()]),
                LPAREN => Token::new(TokenType::LPAREN, vec![self.lexer.read_char()]),
                RPAREN => Token::new(TokenType::RPAREN, vec![self.lexer.read_char()]),
                LBRACE => Token::new(TokenType::LBRACE, vec![self.lexer.read_char()]),
                RBRACE => Token::new(TokenType::RBRACE, vec![self.lexer.read_char()]),
                LBRACKET => Token::new(TokenType::LBRACKET, vec![self.lexer.read_char()]),
                RBRACKET => Token::new(TokenType::RBRACKET, vec![self.lexer.read_char()]),
                LESS => {
                    let ch = self.lexer.read_char();
                    let next_ch = self.lexer.peek();
                    //* <=
                    if next_ch == ASSIGN {
                        Token::new(TokenType::LESS_EQUAL, vec![ch, self.lexer.read_char()])
                    } else {
                        //* <
                        Token::new(TokenType::LESS, vec![ch])
                    }
                }
                GRATER => {
                    let ch = self.lexer.read_char();
                    let next_ch = self.lexer.peek();
                    //* >=
                    if next_ch == ASSIGN {
                        Token::new(TokenType::GRATER_EQUAL, vec![ch, self.lexer.read_char()])
                    } else {
                        //* >
                        Token::new(TokenType::GRATER, vec![ch])
                    }
                }
                QUOTE => Token::new(TokenType::QUOTE, vec![self.lexer.read_char()]),
                SQUOTE => Token::new(TokenType::SQUOTE, vec![self.lexer.read_char()]),
                TQUOTE => Token::new(TokenType::TQUOTE, vec![self.lexer.read_char()]),
                0 => Token::new(TokenType::EOF, vec![self.lexer.read_char()]),
                _ => {
                    if Lexer::is_letter(ch) {
                        // TODO :thinking_face:
                        let identifier = self.lexer.read_identifier();
                        let identifier: &str = &identifier;
                        let token = match identifier {
                            LET => Token::new(TokenType::LET, identifier.as_bytes().to_vec()),
                            FUNCTION => {
                                Token::new(TokenType::FUNCTION, identifier.as_bytes().to_vec())
                            }
                            IF => Token::new(TokenType::IF, identifier.as_bytes().to_vec()),
                            ELSE => Token::new(TokenType::ELSE, identifier.as_bytes().to_vec()),
                            TRUE => Token::new(TokenType::TRUE, identifier.as_bytes().to_vec()),
                            FALSE => Token::new(TokenType::FALSE, identifier.as_bytes().to_vec()),
                            MUTATE => Token::new(TokenType::MUTATE, identifier.as_bytes().to_vec()),
                            RETURN => Token::new(TokenType::RETURN, identifier.as_bytes().to_vec()),
                            _ => Token::new(TokenType::IDENTIFIER, identifier.as_bytes().to_vec()),
                        };

                        token
                    } else if Lexer::is_digit(ch) {
                        let num = self.lexer.read_number();
                        let token = if num.is_float() {
                            Token::new(TokenType::FLOAT, num.to_string().as_bytes().to_vec())
                        } else {
                            Token::new(TokenType::INT, num.to_string().as_bytes().to_vec())
                        };

                        token
                    } else {
                        Token::new(TokenType::ILLEGAL, vec![self.lexer.read_char()])
                    }
                }
            };

            self.lexer.eat_white_space();

            if token.token_type == TokenType::EOF {
                self.tokens.push(Token::new(TokenType::EOF, vec![ch]));
                break;
            } else {
                self.tokens.push(token);
            }
        }
    }
}
