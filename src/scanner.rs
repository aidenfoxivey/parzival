pub struct Scanner {
    pub source: Vec<char>,
    pub start: usize,
    pub current: usize,
    pub line: usize,
}

impl Scanner {
    pub fn new(source: Vec<char>) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    /// Skip whitespace or line comments, incrementing the line count on newlines.
    pub fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek() {
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                // Alright, yeah, comments aren't _technically_ whitespace.
                '/' if self.peek_next() == Some(&'/') => {
                    self.skip_line_comment();
                }
                _ => {
                    break;
                }
            }
        }
    }

    fn skip_line_comment(&mut self) {
        // Skip the initial "//"
        self.advance();
        self.advance();

        while let Some(&c) = self.peek() {
            self.advance();
            if c == '\n' {
                self.line += 1;
                break;
            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.source.get(self.current).is_none()
    }

    fn peek(&self) -> Option<&char> {
        self.source.get(self.current)
    }

    fn peek_next(&self) -> Option<&char> {
        self.source.get(self.current + 1)
    }

    pub fn scan_token(&mut self) -> Result<Token> {
        self.skip_whitespace();
        self.start = self.current;

        // We have reached the end. Note that we still need to do this check
        // when we're lexing other things inside.
        if self.current == self.source.len() {
            return Ok(self.make_token(TokenType::Eof));
        }

        if let Some(c) = self.advance() {
            match c {
                '(' => Ok(self.make_token(TokenType::LeftParen)),
                ')' => Ok(self.make_token(TokenType::RightParen)),
                '{' => Ok(self.make_token(TokenType::LeftBrace)),
                '}' => Ok(self.make_token(TokenType::RightBrace)),
                ';' => Ok(self.make_token(TokenType::Semicolon)),
                ',' => Ok(self.make_token(TokenType::Comma)),
                '.' => Ok(self.make_token(TokenType::Dot)),
                '-' => Ok(self.make_token(TokenType::Minus)),
                '+' => Ok(self.make_token(TokenType::Plus)),
                '/' => Ok(self.make_token(TokenType::Slash)),
                '*' => Ok(self.make_token(TokenType::Star)),
                '!' => {
                    if self.partner('=') {
                        Ok(self.make_token(TokenType::BangEqual))
                    } else {
                        Ok(self.make_token(TokenType::Bang))
                    }
                }
                '=' => {
                    if self.partner('=') {
                        Ok(self.make_token(TokenType::EqualEqual))
                    } else {
                        Ok(self.make_token(TokenType::Equal))
                    }
                }
                '<' => {
                    if self.partner('=') {
                        Ok(self.make_token(TokenType::LessEqual))
                    } else {
                        Ok(self.make_token(TokenType::Less))
                    }
                }
                '>' => {
                    if self.partner('=') {
                        Ok(self.make_token(TokenType::GreaterEqual))
                    } else {
                        Ok(self.make_token(TokenType::Greater))
                    }
                }
                '"' => self.string(),
                c if c.is_ascii_digit() => Ok(self.number()),
                c if c.is_ascii_alphabetic() => Ok(self.identifier()),
                _ => panic!("Unknown character"),
            }
        } else {
            return Ok(self.make_token(TokenType::Eof));
        }
    }

    /// Since `match` is a reserved word in Rust, I'm using `partner`.
    pub fn partner(&mut self, expected: char) -> bool {
        if let Some(&c) = self.source.get(self.current) {
            if c == expected {
                self.current += 1;
                return true;
            }
        }
        false
    }

    fn make_token(&self, ty: TokenType) -> Token {
        Token {
            ty,
            start: self.start,
            length: (self.current - self.start),
            line: self.line,
        }
    }

    pub fn advance(&mut self) -> Option<&char> {
        self.current += 1;
        self.source.get(self.current - 1)
    }

    fn string(&mut self) -> Result<Token> {
        while let Some(c) = self.peek() {
            if *c == '"' {
                break;
            }

            if *c == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            return self.report_error("File ends before string is terminated");
        }

        // consume the final "
        self.advance();

        Ok(self.make_token(TokenType::String))
    }

    /// Convenience method to report a scan error.
    fn report_error<T>(&self, msg: &'static str) -> Result<T> {
        return Err(ScanError {
            line: self.line,
            position: self.start,
            message: msg,
        });
    }

    fn number(&mut self) -> Token {
        while self.peek().is_some_and(|c| (*c).is_ascii_digit()) {
            self.advance();
        }

        if self.peek().is_some_and(|c| *c == '.')
            && self.peek_next().is_some_and(|c| (*c).is_ascii_digit())
        {
            self.advance();

            while self.peek().is_some_and(|c| (*c).is_ascii_digit()) {
                self.advance();
            }
        }
        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token {
        while self.peek().is_some_and(|c| c.is_ascii_alphanumeric()) {
            self.advance();
        }

        self.make_token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenType {
        match self.source[self.start] {
            'a' => self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
            'f' => {
                if self.current - self.start > 1 {
                    match self.source[self.start + 1] {
                        'a' => self.check_keyword(2, 3, "lse", TokenType::False),
                        'o' => self.check_keyword(2, 1, "r", TokenType::For),
                        'u' => self.check_keyword(2, 1, "n", TokenType::Fun),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            'i' => self.check_keyword(1, 1, "f", TokenType::If),
            'n' => self.check_keyword(1, 2, "il", TokenType::Nil),
            'o' => self.check_keyword(1, 1, "r", TokenType::Or),
            'p' => self.check_keyword(1, 4, "rint", TokenType::Print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => self.check_keyword(1, 4, "uper", TokenType::Super),
            't' => {
                if self.current - self.start > 1 {
                    match self.source[self.start + 1] {
                        'h' => self.check_keyword(2, 2, "is", TokenType::This),
                        'r' => self.check_keyword(2, 2, "ue", TokenType::True),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            'v' => self.check_keyword(1, 2, "ar", TokenType::Var),
            'w' => self.check_keyword(1, 4, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    fn check_keyword(&self, start: usize, length: usize, rest: &str, ty: TokenType) -> TokenType {
        if self.current - self.start == start + length
            && self.source[self.start + start..self.start + start + length]
                == rest.chars().collect::<Vec<char>>()
        {
            ty
        } else {
            TokenType::Identifier
        }
    }
}

#[derive(Debug)]
pub struct ScanError {
    pub line: usize,
    pub position: usize,
    pub message: &'static str,
}

pub type Result<T> = std::result::Result<T, ScanError>;

#[derive(Debug, Copy, Clone)]
pub struct Token {
    ty: TokenType,
    start: usize,
    length: usize,
    line: usize,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    String,
    Number,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

mod tests {
    use super::*;

    fn accumulate_tokens(scanner: &mut Scanner) -> Vec<Token> {
        let mut tokens = vec![];
        while !scanner.is_at_end() {
            let tok = scanner.scan_token().unwrap();
            tokens.push(tok);
        }
        tokens
    }

    #[test]
    fn test_parser_valid_lox() {
        // This example is from Crafting Interpreters chapter 3.
        let source: Vec<char> = r#"
            fun addPair(a, b) {
                return a + b;
            }

            fun identity(a) {
                return a;
            }

            print identity(addPair)(1, 2); // Prints "3".
        "#
        .chars()
        .collect();
        let mut scanner = Scanner::new(source);
        let tokens = accumulate_tokens(&mut scanner);
        let tys: Vec<TokenType> = tokens.iter().map(|tok| tok.ty).collect();
        insta::assert_debug_snapshot!(tys);
    }

    #[test]
    fn test_parser_string() {
        // This example is from Crafting Interpreters chapter 3.
        let source: Vec<char> = r#"
            var s = "Hello World!";
        "#
        .chars()
        .collect();
        let mut scanner = Scanner::new(source.clone());
        let tokens = accumulate_tokens(&mut scanner);
        let tys: Vec<TokenType> = tokens.iter().map(|tok| tok.ty).collect();
        insta::assert_debug_snapshot!(tys);
        assert_eq!(tokens.len(), 6); // Including the Eof token
        let string_token = tokens[3];
        assert_eq!(string_token.ty, TokenType::String);
        assert_eq!(
            source[string_token.start..string_token.start + string_token.length],
            "\"Hello World!\"".chars().collect::<Vec<char>>()
        );
    }
}
