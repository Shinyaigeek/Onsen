use crate::ast::lexer::lexer::Lexer;
use crate::ast::tokenizer::tokenizer::{
    Token, TokenType, Tokens, ASTERISK, EQUAL, EXCLAMATION, GRATER, GRATER_EQUAL, LESS, LESS_EQUAL,
    MINUS, NOT_EQUAL, PLUS, SLASH,
};

const LOWEST: u8 = 0;
const EQUALS: u8 = 1; // ==
const LESSGREATER: u8 = 2; // > or <
const SUM: u8 = 3; // +
const PRODUCT: u8 = 4; // *
const PREFIX: u8 = 5; // !X or -X
const CALL: u8 = 6; // function(x)

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

// TODO 一処理 (便宜上のnode) -> Node -> nodeの種類のステップ挟むの辛い, そもそもこれはNodeなのか :thinking_face:
#[derive(Debug, PartialEq)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub body: Vec<Statement>,
}

impl Program {
    pub fn print(&self) -> String {
        let mut res = String::from("");
        for stmt in &self.body {
            res.push_str(&stmt.print());
            res.push_str(";\n");
        }

        res
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    pub statement: StatementType,
}

impl Statement {
    pub fn print(&self) -> String {
        self.statement.print()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementType {
    VariableDeclaration(VariableDeclaration),
    ReturnStatement(ReturnStatement),
    Expression(Expression),
    IfStatement(IfStatement),
    BlockStatement(BlockStatement),
    FunctionDeclarationStatement(FunctionDeclarationStatement),
}

impl StatementType {
    pub fn print(&self) -> String {
        match self {
            Self::VariableDeclaration(variable_declaration) => {
                VariableDeclaration::print(&variable_declaration)
            }
            Self::ReturnStatement(return_statement) => ReturnStatement::print(&return_statement),
            Self::Expression(expression) => Expression::print(&expression),
            Self::IfStatement(if_statement) => IfStatement::print(&if_statement),
            Self::BlockStatement(block_statement) => BlockStatement::print(&block_statement),
            Self::FunctionDeclarationStatement(function_declaration_statement) => {
                FunctionDeclarationStatement::print(&function_declaration_statement)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement {
    pub test: Box<Expression>,
    pub alternate: Box<Option<StatementType>>,
    pub consequents: Vec<Statement>,
}

impl IfStatement {
    pub fn print(target: &IfStatement) -> String {
        String::from("if")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclarationStatement {
    pub arguments: Vec<Identifier>,
    pub identifier: Identifier,
    pub body: Box<StatementType>,
}

impl FunctionDeclarationStatement {
    pub fn print(target: &FunctionDeclarationStatement) -> String {
        format!("fn {}() {}", target.identifier.print(), target.body.print())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
}

impl BlockStatement {
    pub fn print(target: &BlockStatement) -> String {
        let mut res = String::from("{");
        for stmt in &target.body {
            res.push_str(&stmt.print());
            res.push_str(";");
        }
        res.push_str("};");

        res
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub value: String,
}

impl Identifier {
    pub fn print(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub callee: Identifier,
    pub arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn print(&self) -> String {
        let mut arguments = String::from("");
        for arg in &self.arguments {
            let arg = Expression::print(&arg);
            arguments.push_str(&arg);
            arguments.push_str(", ");
        }
        format!("{}({})", self.callee.print(), arguments)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDeclaration {
    // TODO LiteralUnionField
    pub kind: String,
    pub identifier: Identifier,
    pub mutation: bool,
    pub init: Expression,
}

impl VariableDeclaration {
    pub fn print(target: &VariableDeclaration) -> String {
        format!(
            "{} {} = {}",
            target.kind,
            target.identifier.print(),
            Expression::print(&target.init)
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub arguments: Expression,
}

impl ReturnStatement {
    pub fn print(target: &ReturnStatement) -> String {
        let mut res = String::from("return ");

        res.push_str(&Expression::print(&target.arguments));

        res
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(Literal),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Identifier(Identifier),
    CallExpression(CallExpression),
}

impl Expression {
    pub fn print(target: &Expression) -> String {
        match target {
            Expression::Literal(literal) => Literal::print(literal),
            Expression::PrefixExpression(prefix_expression) => {
                PrefixExpression::print(prefix_expression)
            }
            Expression::InfixExpression(infix_expression) => {
                InfixExpression::print(infix_expression)
            }
            Expression::CallExpression(call_expression) => CallExpression::print(call_expression),
            Expression::Identifier(identifier) => Identifier::print(identifier),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub operator: PrefixOperator,
    pub right: Box<Expression>,
}

impl PrefixExpression {
    pub fn print(target: &PrefixExpression) -> String {
        format!(
            "{}{}",
            PrefixOperator::print(&target.operator),
            Expression::print(&target.right)
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub operator: InfixOperator,
    pub right: Box<Expression>,
    pub left: Box<Expression>,
}

impl InfixExpression {
    pub fn print(target: &InfixExpression) -> String {
        format!(
            "{} {} {}",
            Expression::print(&target.left),
            InfixOperator::print(&target.operator),
            Expression::print(&target.right)
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixOperator {
    EXCLAMATION,
    MINUS,
}

impl PrefixOperator {
    pub fn print(target: &PrefixOperator) -> String {
        match target {
            PrefixOperator::EXCLAMATION => String::from("!"),
            PrefixOperator::MINUS => String::from("-"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum InfixOperator {
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    GRATER,
    GRATER_EQUAL,
    LESS,
    LESS_EQUAL,
    EQUAL,
    NOT_EQUAL,
}

impl InfixOperator {
    pub fn print(target: &InfixOperator) -> String {
        match target {
            InfixOperator::PLUS => String::from("+"),
            InfixOperator::MINUS => String::from("-"),
            InfixOperator::ASTERISK => String::from("*"),
            InfixOperator::SLASH => String::from("/"),
            InfixOperator::GRATER => String::from(">"),
            InfixOperator::GRATER_EQUAL => String::from(">="),
            InfixOperator::LESS => String::from("<"),
            InfixOperator::LESS_EQUAL => String::from("<="),
            InfixOperator::EQUAL => String::from("=="),
            InfixOperator::NOT_EQUAL => String::from("!="),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(Integer),
    Float(Float),
    Boolean(Boolean),
    ObjectLiteral(ObjectLiteral),
}

impl Literal {
    pub fn print(target: &Literal) -> String {
        match target {
            Literal::Integer(int) => int.value.clone(),
            Literal::Float(float) => float.value.clone(),
            Literal::Boolean(boolean) => boolean.value.clone(),
            Literal::ObjectLiteral(object) => {
                let mut stringify = String::from("");
                stringify.push_str("{\n");
                for property in &object.properties {
                    stringify.push_str(&property.key.print());
                    stringify.push_str(":");
                    stringify.push_str(&Value::print(&property.value));
                    stringify.push_str(",");
                    stringify.push_str("\n");
                }
                stringify.push_str("};");
                stringify.clone()
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Integer {
    pub value: String,
    pub literal_type: LiteralType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Float {
    pub value: String,
    pub literal_type: LiteralType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Boolean {
    pub value: String,
    pub literal_type: LiteralType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ObjectLiteral {
    pub properties: Vec<ObjectProperty>,
    pub literal_type: LiteralType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ObjectProperty {
    key: Identifier,
    value: Value,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Identifier(Identifier),
    Literal(Literal),
}

impl Value {
    pub fn print(target: &Value) -> String {
        match target {
            Value::Identifier(identifier) => identifier.print(),
            Value::Literal(literal) => Literal::print(&literal),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralType {
    INT,
    FLOAT,
    BOOLEAN,
    OBJECT_LITERAL,
}

#[derive(Debug, PartialEq)]
pub struct Parser {
    tokens: Tokens,
    pub program: Program,
}

impl Parser {
    pub fn new(tokens: Tokens) -> Self {
        let mut program = Program { body: vec![] };
        let mut parser = Parser { tokens, program };
        parser
    }

    pub fn parse(&mut self) {
        let statements = self.handle_statements();
        self.program.body = statements;
    }

    fn handle_statements(&mut self) -> Vec<Statement> {
        let mut statements: Vec<Statement> = vec![];
        loop {
            // TODO should fix
            let token = if Parser::is_semicolon(self.tokens.cur_token()) {
                self.tokens.read_token()
            } else {
                self.tokens.cur_token()
            };
            let statement = if Parser::is_variable_declaration(token) {
                self.handle_variable_declaration()
            } else if Parser::is_return_statement(token) {
                self.handle_return_statement()
            } else if token.token_type == TokenType::EOF || token.token_type == TokenType::RBRACE {
                break;
            } else if Parser::is_if_block(token) {
                self.handle_if_statement()
            } else if Parser::is_left_brace(token) {
                self.handle_block_statement()
            } else if Parser::is_function_declaration(token) {
                self.handle_function_declaration_statement()
            } else {
                self.handle_expression_statement()
            };

            let statement = Statement { statement };
            statements.push(statement);

            if self.tokens.cur_token().token_type == TokenType::EOF {
                break;
            }
            if self.tokens.peek_token().token_type == TokenType::RBRACE {
                self.tokens.read_token();
                break;
            }

            if self.tokens.peek_token().token_type == TokenType::SEMICOLON {
                self.tokens.read_token();
            }
        }

        statements
    }

    fn is_return_statement(token: &Token) -> bool {
        token.token_type == TokenType::RETURN
    }

    fn is_variable_declaration(token: &Token) -> bool {
        token.token_type == TokenType::LET
    }

    fn is_function_declaration(token: &Token) -> bool {
        token.token_type == TokenType::FUNCTION
    }

    fn is_if_block(token: &Token) -> bool {
        token.token_type == TokenType::IF
    }

    fn is_assign(token: &Token) -> bool {
        token.token_type == TokenType::ASSIGN
    }

    fn is_left_brace(token: &Token) -> bool {
        token.token_type == TokenType::LBRACE
    }

    fn is_right_brace(token: &Token) -> bool {
        token.token_type == TokenType::RBRACE
    }

    fn is_left_paren(token: &Token) -> bool {
        token.token_type == TokenType::LPAREN
    }

    fn is_expression(token: &Token) -> bool {
        Parser::is_literal(&token)
            || Parser::is_identifier(&token)
            || Parser::is_prefix_operator(&token)
    }

    fn is_literal(token: &Token) -> bool {
        Parser::is_number(&token) || Parser::is_boolean(&token) || Parser::is_left_brace(&token)
    }

    fn is_number(token: &Token) -> bool {
        token.token_type == TokenType::INT || token.token_type == TokenType::FLOAT
    }

    fn is_boolean(token: &Token) -> bool {
        token.token_type == TokenType::TRUE || token.token_type == TokenType::FALSE
    }

    fn is_identifier(token: &Token) -> bool {
        token.token_type == TokenType::IDENTIFIER
    }

    fn is_sum_operator(token: &Token) -> bool {
        token.token_type == TokenType::PLUS || token.token_type == TokenType::MINUS
    }

    fn is_product_operator(token: &Token) -> bool {
        token.token_type == TokenType::ASTERISK || token.token_type == TokenType::SLASH
    }

    fn is_equal_operator(token: &Token) -> bool {
        token.token_type == TokenType::EQUAL || token.token_type == TokenType::NOT_EQUAL
    }

    fn is_greater_less_operator(token: &Token) -> bool {
        token.token_type == TokenType::GRATER
            || token.token_type == TokenType::LESS
            || token.token_type == TokenType::GRATER_EQUAL
            || token.token_type == TokenType::LESS_EQUAL
    }

    fn is_prefix_operator(token: &Token) -> bool {
        token.token_type == TokenType::EXCLAMATION
            || token.token_type == TokenType::MINUS
            || token.token_type == TokenType::LPAREN
    }

    fn is_semicolon(token: &Token) -> bool {
        token.token_type == TokenType::SEMICOLON
    }

    fn is_colon(token: &Token) -> bool {
        token.token_type == TokenType::COLON
    }

    fn is_comma(token: &Token) -> bool {
        token.token_type == TokenType::COMMA
    }

    fn is_left_precedencer(left: Precedence, right: Precedence) -> bool {
        left as u8 > right as u8
    }

    fn parse_identifier(&mut self) -> Expression {
        Expression::Identifier(Identifier {
            value: self.tokens.cur_token().value.clone(),
        })
    }

    fn parse_call_expression(&mut self) -> Expression {
        let callee = self.tokens.cur_token();
        let callee = Identifier {
            value: callee.value.clone(),
        };
        let arguments = self.parse_function_arguments();
        Expression::CallExpression(CallExpression { callee, arguments })
    }

    fn parse_literal(&mut self) -> Expression {
        let token = self.tokens.cur_token();
        if Parser::is_number(token) {
            if token.token_type == TokenType::INT {
                Parser::parse_int(token)
            } else {
                Parser::parse_float(token)
            }
        } else if Parser::is_boolean(token) {
            Parser::parse_boolean(token)
        } else if Parser::is_left_brace(token) {
            self.parse_object()
        } else {
            panic!(
                "parse_literal should get only int or boolean or objet, but got {:?}",
                token
            );
        }
    }

    fn parse_int(token: &Token) -> Expression {
        Expression::Literal(Literal::Integer(Integer {
            value: token.value.clone(),
            literal_type: LiteralType::INT,
        }))
    }

    fn parse_float(token: &Token) -> Expression {
        Expression::Literal(Literal::Float(Float {
            value: token.value.clone(),
            literal_type: LiteralType::FLOAT,
        }))
    }

    fn parse_boolean(token: &Token) -> Expression {
        Expression::Literal(Literal::Boolean(Boolean {
            value: token.value.clone(),
            literal_type: LiteralType::BOOLEAN,
        }))
    }

    fn parse_object(&mut self) -> Expression {
        let l_brace = self.tokens.cur_token();

        if !Parser::is_left_brace(l_brace) {
            panic!("object should starts with {{, but got {:?}", l_brace);
        }

        let mut properties: Vec<ObjectProperty> = vec![];

        loop {
            let key = self.tokens.read_token();

            if !Parser::is_identifier(&key) {
                panic!("object key should be identifier, but got {:?}", key);
            }

            let key = Identifier {
                value: key.value.clone(),
            };

            let colon = self.tokens.read_token();

            if !Parser::is_colon(&colon) {
                panic!("colon should be next to object key, but got {:?}", colon);
            }

            let value = self.tokens.read_token();

            if !Parser::is_literal(&value) && !Parser::is_identifier(&value) {
                panic!(
                    "object value should be identifier or literal, but got {:?}",
                    value
                );
            }

            let value = if Parser::is_literal(&value) {
                self.parse_literal()
            } else {
                self.parse_identifier()
            };

            let value = match value {
                Expression::Literal(literal) => Value::Literal(literal),
                Expression::Identifier(identifier) => Value::Identifier(identifier),
                _ => panic!(
                    "object value should be literal or identifier, but got {:?}",
                    value
                ),
            };

            let comma = self.tokens.read_token();

            if !Parser::is_comma(&comma) {
                panic!("object sould be separated with , but got {:?}", comma);
            }

            let property = ObjectProperty {
                key: key,
                value: value,
            };

            properties.push(property);

            let peek = self.tokens.peek_token();

            if Parser::is_right_brace(&peek) {
                self.tokens.read_token();
                break;
            }
        }

        Expression::Literal(Literal::ObjectLiteral(ObjectLiteral {
            properties: properties,
            literal_type: LiteralType::OBJECT_LITERAL,
        }))
    }

    fn parse_prefix_operator(token: &Token) -> PrefixOperator {
        if token.token_type == TokenType::EXCLAMATION {
            PrefixOperator::EXCLAMATION
        } else if token.token_type == TokenType::MINUS {
            PrefixOperator::MINUS
        } else {
            panic!("parse_prefix_operator should get exclamation or minus");
        }
    }

    fn parse_infix_operator(token: &Token) -> InfixOperator {
        if token.token_type == TokenType::PLUS {
            InfixOperator::PLUS
        } else if token.token_type == TokenType::MINUS {
            InfixOperator::MINUS
        } else if token.token_type == TokenType::ASTERISK {
            InfixOperator::ASTERISK
        } else if token.token_type == TokenType::SLASH {
            InfixOperator::SLASH
        } else if token.token_type == TokenType::GRATER {
            InfixOperator::GRATER
        } else if token.token_type == TokenType::GRATER_EQUAL {
            InfixOperator::GRATER_EQUAL
        } else if token.token_type == TokenType::LESS {
            InfixOperator::LESS
        } else if token.token_type == TokenType::LESS_EQUAL {
            InfixOperator::LESS_EQUAL
        } else if token.token_type == TokenType::EQUAL {
            InfixOperator::EQUAL
        } else if token.token_type == TokenType::NOT_EQUAL {
            InfixOperator::NOT_EQUAL
        } else {
            panic!("parse_infix_operator should get exclamation or minus");
        }
    }

    fn peek_precedence(&self) -> Precedence {
        let token = self.tokens.peek_token();

        if Parser::is_sum_operator(&token) {
            Precedence::SUM
        } else if Parser::is_product_operator(&token) {
            Precedence::PRODUCT
        } else if Parser::is_equal_operator(&token) {
            Precedence::EQUALS
        } else if Parser::is_greater_less_operator(&token) {
            Precedence::LESSGREATER
        } else {
            Precedence::LOWEST
        }
    }

    fn cur_precedence(&self) -> Precedence {
        let token = self.tokens.cur_token();

        if Parser::is_sum_operator(&token) {
            Precedence::SUM
        } else if Parser::is_product_operator(&token) {
            Precedence::PRODUCT
        } else if Parser::is_equal_operator(&token) {
            Precedence::EQUALS
        } else if Parser::is_greater_less_operator(&token) {
            Precedence::LESSGREATER
        } else {
            Precedence::LOWEST
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Expression {
        let token = if Parser::is_semicolon(self.tokens.cur_token()) {
            self.tokens.read_token()
        } else {
            self.tokens.cur_token()
        };

        let mut depth = if token.token_type == TokenType::LPAREN {
            1
        } else {
            0
        };

        let mut left_expression = if Parser::is_literal(&token) {
            self.parse_literal()
        } else if Parser::is_identifier(&token) {
            if Parser::is_left_paren(&self.tokens.peek_token()) {
                self.parse_call_expression()
            } else {
                self.parse_identifier()
            }
        } else if Parser::is_prefix_operator(&token) {
            self.parse_prefix_expression()
        } else {
            panic!("prefix should be literal or identifier or prefix")
        };

        // TODO
        while !Parser::is_semicolon(self.tokens.peek_token())
            && !Parser::is_left_precedencer(precedence, self.peek_precedence())
            && self.tokens.peek_token().token_type != TokenType::EOF
            && self.tokens.peek_token().token_type != TokenType::LBRACE
            && !(self.tokens.peek_token().token_type == TokenType::COMMA && depth == 0)
            && depth >= 0
        {
            let token = self.tokens.read_token();

            left_expression = if Parser::is_sum_operator(&token) {
                self.parse_infix_expression(left_expression)
            } else if Parser::is_product_operator(&token) {
                self.parse_infix_expression(left_expression)
            } else if Parser::is_equal_operator(&token) {
                self.parse_infix_expression(left_expression)
            } else if Parser::is_greater_less_operator(&token) {
                self.parse_infix_expression(left_expression)
            } else {
                left_expression
            };

            if self.tokens.peek_token().token_type == TokenType::LPAREN {
                depth += 1;
            } else if self.tokens.peek_token().token_type == TokenType::RPAREN {
                depth -= 1;
            }
        }

        // TODO refactor
        if depth < 0 {
            self.tokens.read_token();
        }

        left_expression
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let token = self.tokens.cur_token();
        if token.token_type == TokenType::LPAREN {
            self.tokens.read_token();
            let expression = self.parse_expression(Precedence::LOWEST);
            if self.tokens.peek_token().token_type == TokenType::RPAREN {
                panic!(") should be next to (");
            }

            return expression;
        }
        let operator = Parser::parse_prefix_operator(&token);
        self.tokens.read_token();
        let expression = PrefixExpression {
            operator,
            right: Box::new(self.parse_expression(Precedence::PREFIX)),
        };

        Expression::PrefixExpression(expression)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let token = self.tokens.cur_token();
        let operator = Parser::parse_infix_operator(token);

        let precedence = self.cur_precedence();

        self.tokens.read_token();

        let expression = InfixExpression {
            operator,
            right: Box::new(self.parse_expression(precedence)),
            left: Box::new(left),
        };

        Expression::InfixExpression(expression)
    }

    fn handle_expression_statement(&mut self) -> StatementType {
        let expression = self.parse_expression(Precedence::LOWEST);

        StatementType::Expression(expression)
    }

    fn handle_block_statement(&mut self) -> StatementType {
        self.tokens.read_token();
        let statements = self.handle_statements();
        let r_brace = self.tokens.cur_token();

        if r_brace.token_type != TokenType::RBRACE {
            panic!(
                "block statement should close with r_brace, but got {:?}",
                r_brace
            );
        }

        self.tokens.read_token();

        StatementType::BlockStatement(BlockStatement { body: statements })
    }

    fn handle_if_statement(&mut self) -> StatementType {
        let l_paren = if self.tokens.peek_token().token_type == TokenType::IF {
            self.tokens.read_token();
            self.tokens.read_token()
        } else {
            self.tokens.read_token()
        };

        let l_paren = self.tokens.peek_token();

        if l_paren.token_type == TokenType::LPAREN {
            panic!("if statement's test should braced ()");
        }
        let test = self.parse_expression(Precedence::LOWEST);

        if self.tokens.read_token().token_type == TokenType::RPAREN {
            panic!(") should be next to (");
        }

        let l_brace = self.tokens.cur_token();

        if l_brace.token_type != TokenType::LBRACE {
            panic!("if statement's consequents should braced {}");
        }

        self.tokens.read_token();

        let consequents = self.handle_statements();

        let r_brace = if self.tokens.cur_token().token_type == TokenType::SEMICOLON {
            self.tokens.read_token()
        } else {
            self.tokens.cur_token()
        };

        if r_brace.token_type != TokenType::RBRACE {
            panic!("if statements' consecuents should end with }")
        }

        if self.tokens.peek_token().token_type == TokenType::ELSE {
            self.tokens.read_token();
            let alternate = if self.tokens.peek_token().token_type == TokenType::IF {
                self.handle_if_statement()
            } else {
                self.tokens.read_token();
                self.handle_block_statement()
            };

            return StatementType::IfStatement(IfStatement {
                test: Box::new(test),
                consequents,
                alternate: Box::new(Some(alternate)),
            });
        }

        StatementType::IfStatement(IfStatement {
            test: Box::new(test),
            consequents,
            alternate: Box::new(None),
        })
    }

    fn handle_return_statement(&mut self) -> StatementType {
        let return_token = self.tokens.cur_token();
        if return_token.token_type != TokenType::RETURN {
            panic!("handle_return_statement can only handle return");
        }
        let token = self.tokens.read_token();

        if Parser::is_expression(&token) {
            let arguments = self.parse_expression(Precedence::LOWEST);
            let statement = ReturnStatement { arguments };
            return StatementType::ReturnStatement(statement);
        } else {
            panic!(
                "return statement's arguments should be expression, but got {:?}",
                token
            );
        }
    }

    fn handle_function_declaration_statement(&mut self) -> StatementType {
        let identifier = self.tokens.read_token();

        if !Parser::is_identifier(&identifier) {
            panic!(
                "identifier should next to function declarament, but got {:?}",
                identifier
            );
        }

        let identifier = Identifier {
            value: identifier.value.clone(),
        };

        let l_paren = self.tokens.peek_token();

        if !Parser::is_left_paren(l_paren) {
            panic!(
                "( should next to function declarament, but got {:?}",
                l_paren
            );
        }

        let arguments = self.parse_function_declaration_arguments();

        self.tokens.read_token();

        let body = self.handle_block_statement();

        StatementType::FunctionDeclarationStatement(FunctionDeclarationStatement {
            body: Box::new(body),
            arguments,
            identifier,
        })
    }

    fn parse_function_declaration_arguments(&mut self) -> Vec<Identifier> {
        self.tokens.read_token();
        let mut arguments: Vec<Identifier> = vec![];

        if self.tokens.peek_token().token_type == TokenType::RPAREN {
            self.tokens.read_token();
            return arguments;
        }

        loop {
            let argument = self.tokens.read_token();

            if !Parser::is_identifier(argument) {
                panic!("argument should be identifier, but got {:?}", argument);
            }

            let argument = Identifier {
                value: argument.value.clone(),
            };

            arguments.push(argument);

            let comma = self.tokens.read_token();

            if comma.token_type == TokenType::RPAREN {
                break;
            }

            if comma.token_type != TokenType::COMMA {
                panic!("argument should join with , but got {:?}", comma);
            }
        }

        arguments
    }

    fn parse_function_arguments(&mut self) -> Vec<Expression> {
        self.tokens.read_token();
        let mut arguments: Vec<Expression> = vec![];

        if self.tokens.peek_token().token_type == TokenType::RPAREN {
            self.tokens.read_token();
            return arguments;
        }

        loop {
            let argument = self.tokens.read_token();

            if !Parser::is_expression(argument) {
                panic!("argument should be expression, but got {:?}", argument);
            }

            let argument = self.parse_expression(Precedence::LOWEST);

            arguments.push(argument);

            let comma = self.tokens.cur_token();

            if comma.token_type == TokenType::RPAREN {
                break;
            }

            let comma = self.tokens.read_token();

            if comma.token_type != TokenType::COMMA {
                panic!("argument should join with , but got {:?}", comma);
            }
        }

        arguments
    }

    fn handle_variable_declaration(&mut self) -> StatementType {
        // TODO should fix
        let declaration_token = self.tokens.cur_token();
        // TODO 今の所ExpressionにはLiteral Intしか来ないようにしている
        if declaration_token.token_type == TokenType::LET {
            let token = self.tokens.read_token();
            let is_mutate = if token.token_type == TokenType::MUTATE {
                true
            } else {
                false
            };

            let identifier = if is_mutate {
                self.tokens.read_token()
            } else {
                token
            };

            if Parser::is_identifier(&identifier) == false {
                panic!("TokenType::IDENTIFIER should be next to let or let mut");
            }

            let identifier = Identifier {
                value: identifier.value.clone(),
            };

            let token = self.tokens.read_token();

            // TODO ! ?
            if Parser::is_assign(&token) == false {
                panic!("TokenType::ASSIGN should be next to let identifier or let mut identifier");
            }

            let token = self.tokens.read_token();

            let initializer = self.parse_expression(Precedence::LOWEST);

            let statement = VariableDeclaration {
                // TODO support const
                kind: String::from("let"),
                identifier: identifier,
                mutation: is_mutate,
                init: initializer,
            };

            return StatementType::VariableDeclaration(statement);

            panic!("VariableDeclaration's initializer should be expression");
        } else {
            panic!("handleVariableDeclaration should take LET as Token::token_type");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn object_declaration_works() {
        let mut lexer = Lexer::new(&String::from(
            "let i = {
hoge: 1,
};",
        ));
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        assert_eq!(
            parser.program.print(),
            "let i = {
hoge:1,
};;
"
        );
    }

    #[test]
    fn variable_declaration_works() {
        let mut lexer = Lexer::new(&String::from(
            "let five = 5;
        let ten = 2 * 8;
        let i = (1 + 2) * 8.5;
        let t = true;
        let f = !true;",
        ));
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let expected = Program {
            body: vec![
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Identifier {
                            value: String::from("five"),
                        },
                        mutation: false,
                        init: Expression::Literal(Literal::Integer(Integer {
                            value: String::from("5"),
                            literal_type: LiteralType::INT,
                        })),
                    }),
                },
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Identifier {
                            value: String::from("ten"),
                        },
                        mutation: false,
                        init: Expression::InfixExpression(InfixExpression {
                            operator: InfixOperator::ASTERISK,
                            right: Box::new(Expression::Literal(Literal::Integer(Integer {
                                value: "8".to_string(),
                                literal_type: LiteralType::INT,
                            }))),
                            left: Box::new(Expression::Literal(Literal::Integer(Integer {
                                value: "2".to_string(),
                                literal_type: LiteralType::INT,
                            }))),
                        }),
                    }),
                },
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Identifier {
                            value: String::from("i"),
                        },
                        mutation: false,
                        init: Expression::InfixExpression(InfixExpression {
                            operator: InfixOperator::ASTERISK,
                            right: Box::new(Expression::Literal(Literal::Float(Float {
                                value: "8.5".to_string(),
                                literal_type: LiteralType::FLOAT,
                            }))),
                            left: Box::new(Expression::InfixExpression(InfixExpression {
                                operator: InfixOperator::PLUS,
                                right: Box::new(Expression::Literal(Literal::Integer(Integer {
                                    value: "2".to_string(),
                                    literal_type: LiteralType::INT,
                                }))),
                                left: Box::new(Expression::Literal(Literal::Integer(Integer {
                                    value: "1".to_string(),
                                    literal_type: LiteralType::INT,
                                }))),
                            })),
                        }),
                    }),
                },
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Identifier {
                            value: String::from("t"),
                        },
                        mutation: false,
                        init: Expression::Literal(Literal::Boolean(Boolean {
                            value: "true".to_string(),
                            literal_type: LiteralType::BOOLEAN,
                        })),
                    }),
                },
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Identifier {
                            value: String::from("f"),
                        },
                        mutation: false,
                        init: Expression::PrefixExpression(PrefixExpression {
                            operator: PrefixOperator::EXCLAMATION,
                            right: Box::new(Expression::Literal(Literal::Boolean(Boolean {
                                value: "true".to_string(),
                                literal_type: LiteralType::BOOLEAN,
                            }))),
                        }),
                    }),
                },
            ],
        };

        assert_eq!(parser.program, expected);
    }

    #[test]
    fn return_statement_works() {
        let mut lexer = Lexer::new(&String::from(
            "let mut five = 5;
        return 5;
        return 2 * 8;",
        ));
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let expected = Program {
            body: vec![
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Identifier {
                            value: String::from("five"),
                        },
                        mutation: true,
                        init: Expression::Literal(Literal::Integer(Integer {
                            value: String::from("5"),
                            literal_type: LiteralType::INT,
                        })),
                    }),
                },
                Statement {
                    statement: StatementType::ReturnStatement(ReturnStatement {
                        arguments: Expression::Literal(Literal::Integer(Integer {
                            value: String::from("5"),
                            literal_type: LiteralType::INT,
                        })),
                    }),
                },
                Statement {
                    statement: StatementType::ReturnStatement(ReturnStatement {
                        arguments: Expression::InfixExpression(InfixExpression {
                            operator: InfixOperator::ASTERISK,
                            right: Box::new(Expression::Literal(Literal::Integer(Integer {
                                value: "8".to_string(),
                                literal_type: LiteralType::INT,
                            }))),
                            left: Box::new(Expression::Literal(Literal::Integer(Integer {
                                value: "2".to_string(),
                                literal_type: LiteralType::INT,
                            }))),
                        }),
                    }),
                },
            ],
        };

        assert_eq!(parser.program, expected);
    }

    #[test]
    fn parse_expression_works() {
        let mut lexer = Lexer::new(&String::from("1 + 3 * 6 + 2;"));
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let expected = Program {
            body: vec![Statement {
                statement: StatementType::Expression(Expression::InfixExpression(
                    InfixExpression {
                        operator: InfixOperator::PLUS,
                        right: Box::new(Expression::InfixExpression(InfixExpression {
                            operator: InfixOperator::PLUS,
                            right: Box::new(Expression::Literal(Literal::Integer(Integer {
                                value: "2".to_string(),
                                literal_type: LiteralType::INT,
                            }))),
                            left: Box::new(Expression::InfixExpression(InfixExpression {
                                operator: InfixOperator::ASTERISK,
                                right: Box::new(Expression::Literal(Literal::Integer(Integer {
                                    value: "6".to_string(),
                                    literal_type: LiteralType::INT,
                                }))),
                                left: Box::new(Expression::Literal(Literal::Integer(Integer {
                                    value: "3".to_string(),
                                    literal_type: LiteralType::INT,
                                }))),
                            })),
                        })),
                        left: Box::new(Expression::Literal(Literal::Integer(Integer {
                            value: "1".to_string(),
                            literal_type: LiteralType::INT,
                        }))),
                    },
                )),
            }],
        };

        assert_eq!(parser.program, expected);
    }

    #[test]
    fn if_declaration_works() {
        let mut lexer = Lexer::new(&String::from(
            "if (1 == 1) {
            let hoge = 33;
        } else if (true) {
            let fuga = 909;
        }else {
            let bar = 666;
        };",
        ));
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let expected = Program {
            body: vec![Statement {
                statement: StatementType::IfStatement(IfStatement {
                    test: Box::new(Expression::InfixExpression(InfixExpression {
                        operator: InfixOperator::EQUAL,
                        right: Box::new(Expression::Literal(Literal::Integer(Integer {
                            value: "1".to_string(),
                            literal_type: LiteralType::INT,
                        }))),
                        left: Box::new(Expression::Literal(Literal::Integer(Integer {
                            value: "1".to_string(),
                            literal_type: LiteralType::INT,
                        }))),
                    })),
                    alternate: Box::new(Some(StatementType::IfStatement(IfStatement {
                        test: Box::new(Expression::Literal(Literal::Boolean(Boolean {
                            value: "true".to_string(),
                            literal_type: LiteralType::BOOLEAN,
                        }))),
                        alternate: Box::new(Some(StatementType::BlockStatement(BlockStatement {
                            body: vec![Statement {
                                statement: StatementType::VariableDeclaration(
                                    VariableDeclaration {
                                        kind: "let".to_string(),
                                        identifier: Identifier {
                                            value: "bar".to_string(),
                                        },
                                        mutation: false,
                                        init: Expression::Literal(Literal::Integer(Integer {
                                            value: "666".to_string(),
                                            literal_type: LiteralType::INT,
                                        })),
                                    },
                                ),
                            }],
                        }))),
                        consequents: vec![Statement {
                            statement: StatementType::VariableDeclaration(VariableDeclaration {
                                kind: "let".to_string(),
                                identifier: Identifier {
                                    value: "fuga".to_string(),
                                },
                                mutation: false,
                                init: Expression::Literal(Literal::Integer(Integer {
                                    value: "909".to_string(),
                                    literal_type: LiteralType::INT,
                                })),
                            }),
                        }],
                    }))),
                    consequents: vec![Statement {
                        statement: StatementType::VariableDeclaration(VariableDeclaration {
                            kind: "let".to_string(),
                            identifier: Identifier {
                                value: "hoge".to_string(),
                            },
                            mutation: false,
                            init: Expression::Literal(Literal::Integer(Integer {
                                value: "33".to_string(),
                                literal_type: LiteralType::INT,
                            })),
                        }),
                    }],
                }),
            }],
        };

        assert_eq!(parser.program, expected);
    }
}
