use std::fmt::Display;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::{env, io, ops::Neg, process::exit};

type Value = f64;

#[derive(Default)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn write_chunk(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    /// Disassemble each instruction from a chunk.
    pub fn disassemble_chunk(&mut self, name: &str) {
        println!("== {} ==", name);

        let mut offset = 0;
        while offset < self.code.len() {
            // `disassemble_instruction` will return the next offset, since the
            // specific offset depends on the instruction disassembled.
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&mut self, offset: usize) -> usize {
        print!("{:04} ", offset);

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }

        let instr = self.code[offset];
        match OpCode::from_byte(instr) {
            OpCode::Return => Chunk::simple_instruction("OP_RETURN", offset),
            OpCode::Constant => Chunk::constant_instruction("OP_CONSTANT", self, offset),
            OpCode::Negate => Chunk::simple_instruction("OP_NEGATE", offset),
            OpCode::Add => Chunk::simple_instruction("OP_ADD", offset),
            OpCode::Substract => Chunk::simple_instruction("OP_SUBTRACT", offset),
            OpCode::Multiply => Chunk::simple_instruction("OP_MULTIPLY", offset),
            OpCode::Divide => Chunk::simple_instruction("OP_DIVIDE", offset),
        }
    }

    fn simple_instruction(name: &str, offset: usize) -> usize {
        println!("{}", name);
        offset + 1
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn constant_instruction(name: &str, chunk: &mut Chunk, offset: usize) -> usize {
        let constant = chunk.code[offset + 1];
        print!("{:-16} {:4} '", name, constant);
        print!("{}", &chunk.constants[constant as usize]);
        println!();
        offset + 2 // OP_CONSTANT is two bytes (opcode and operand)
    }
}

macro_rules! define_opcodes {
    ($($name:ident = $val:expr),+ $(,)?) => {
        #[derive(Debug, PartialEq, Clone, Copy)]
        #[repr(u8)]
        enum OpCode {
            $($name = $val),+
        }

        impl OpCode {
            pub fn variants() -> Vec<OpCode> {
                vec![$(OpCode::$name),+]
            }

            pub fn as_byte(&self) -> u8 {
                *self as u8
            }

            pub fn from_byte(x: u8) -> Self {
                match x {
                    $($val => OpCode::$name,)+
                    _ => panic!("Invalid opcode"),
                }
            }
        }
    };
}

define_opcodes! {
    Return = 0,
    Constant = 1,
    Negate = 2,
    Add = 3,
    Substract = 4,
    Multiply = 5,
    Divide = 6,
}

/// `VM` holds a current stack (`stack`), an instruction pointer (`ip`), and the bytecode instructions (`chunk`).
#[derive(Default)]
pub struct VM {
    pub chunk: Chunk,
    pub ip: usize,
    pub stack: Vec<Value>,
}

impl VM {
    /// Compile and run `source`.
    pub fn interpret(&mut self, source: &str) -> Result<(), VMError> {
        let mut chunk = Chunk::default();

        self.compile(source, &mut chunk)?;

        self.chunk = chunk;
        self.ip = 0;

        self.run()?;

        Ok(())
    }

    /// Compile `source` into instructions stored within `chunk`.
    fn compile(&mut self, source: &str, chunk: &mut Chunk) -> Result<(), VMError> {
        let chars: Vec<char> = source.chars().collect();
        let mut scanner = Scanner::new(chars);

        Ok(())
    }

    /// Execute the code.
    fn run(&mut self) -> Result<(), VMError> {
        loop {
            #[cfg(feature = "trace_exec")]
            {
                print!("          ");
                for elt in &self.stack {
                    print!("[ {} ]", elt);
                }
                println!();
                self.chunk.disassemble_instruction(self.ip);
            }

            match OpCode::from_byte(self.read_byte()) {
                OpCode::Return => {
                    println!("{}", self.pop().unwrap());
                    return Ok(());
                }
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant);
                    println!("{}", &constant);
                }
                OpCode::Negate => {
                    let top = self.pop().unwrap();
                    self.push(top.neg());
                }
                OpCode::Add => self.binary_op(|a, b| a + b)?,
                OpCode::Substract => self.binary_op(|a, b| a - b)?,
                OpCode::Multiply => self.binary_op(|a, b| a * b)?,
                OpCode::Divide => self.binary_op(|a, b| a / b)?,
            };
        }
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.code[self.ip];
        self.ip += 1;
        byte
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte();
        self.chunk.constants[index as usize]
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    fn binary_op(&mut self, op: fn(Value, Value) -> Value) -> Result<(), VMError> {
        if self.stack.len() < 2 {
            return Err(VMError::RuntimeError(format!(
                "binary_op: stack needs two elements to execute {:#?}",
                op
            )));
        }
        let b = self.pop().unwrap();
        let a = self.pop().unwrap();
        self.push(op(a, b));

        Ok(())
    }
}

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

    pub fn scan_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        self.start = self.current;

        // We have reached the end. Note that we still need to do this check
        // when we're lexing other things inside.
        if self.current == self.source.len() {
            return Some(self.make_token(TokenType::Eof));
        }

        match self.advance() {
            '(' => Some(self.make_token(TokenType::LeftParen)),
            ')' => Some(self.make_token(TokenType::RightParen)),
            '{' => Some(self.make_token(TokenType::LeftBrace)),
            '}' => Some(self.make_token(TokenType::RightBrace)),
            ';' => Some(self.make_token(TokenType::Semicolon)),
            ',' => Some(self.make_token(TokenType::Comma)),
            '.' => Some(self.make_token(TokenType::Dot)),
            '-' => Some(self.make_token(TokenType::Minus)),
            '+' => Some(self.make_token(TokenType::Plus)),
            '/' => Some(self.make_token(TokenType::Slash)),
            '*' => Some(self.make_token(TokenType::Star)),
            '!' => {
                if self.partner('=') {
                    Some(self.make_token(TokenType::BangEqual))
                } else {
                    Some(self.make_token(TokenType::Bang))
                }
            }
            '=' => {
                if self.partner('=') {
                    Some(self.make_token(TokenType::EqualEqual))
                } else {
                    Some(self.make_token(TokenType::Equal))
                }
            }
            '<' => {
                if self.partner('=') {
                    Some(self.make_token(TokenType::LessEqual))
                } else {
                    Some(self.make_token(TokenType::Less))
                }
            }
            '>' => {
                if self.partner('=') {
                    Some(self.make_token(TokenType::GreaterEqual))
                } else {
                    Some(self.make_token(TokenType::Greater))
                }
            }
            '"' => self.string(),
            c if c.is_ascii_digit() => Some(self.number()),
            c if c.is_ascii_alphabetic() => Some(self.identifier()),
            _ => None,
        }
    }

    /// Since `match` is a reserved word in Rust, I'm using `partner`.
    pub fn partner(&mut self, expected: char) -> bool {
        if self.current == self.source.len() {
            return false;
        }

        if self.source[self.current] != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn make_token(&self, ty: TokenType) -> Token {
        Token {
            ty,
            start: self.start,
            length: (self.current - self.start),
            line: self.line,
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }

    fn string(&mut self) -> Option<Token> {
        while self.peek().is_some_and(|c| *c != '"') && !self.is_at_end() {
            if self.peek().is_some_and(|c| *c == '\n') {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return None;
        }

        // consume the final "
        self.advance();

        Some(self.make_token(TokenType::String))
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
            && self.source[self.start..self.current] == rest.chars().collect::<Vec<char>>()
        {
            return ty;
        }

        TokenType::Identifier
    }
}

#[derive(Copy, Clone)]
pub struct Token {
    ty: TokenType,
    start: usize,
    length: usize,
    line: usize,
}

#[derive(Copy, Clone, PartialEq)]
enum TokenType {
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

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "LeftParen"),
            TokenType::RightParen => write!(f, "RightParen"),
            TokenType::LeftBrace => write!(f, "LeftBrace"),
            TokenType::RightBrace => write!(f, "RightBrace"),
            TokenType::Comma => write!(f, "Comma"),
            TokenType::Dot => write!(f, "Dot"),
            TokenType::Minus => write!(f, "Minus"),
            TokenType::Plus => write!(f, "Plus"),
            TokenType::Semicolon => write!(f, "Semicolon"),
            TokenType::Slash => write!(f, "Slash"),
            TokenType::Star => write!(f, "Star"),
            TokenType::Bang => write!(f, "Bang"),
            TokenType::BangEqual => write!(f, "BangEqual"),
            TokenType::Equal => write!(f, "Equal"),
            TokenType::EqualEqual => write!(f, "EqualEqual"),
            TokenType::Greater => write!(f, "Greater"),
            TokenType::GreaterEqual => write!(f, "GreaterEqual"),
            TokenType::Less => write!(f, "Less"),
            TokenType::LessEqual => write!(f, "LessEqual"),
            TokenType::Identifier => write!(f, "Identifier"),
            TokenType::String => write!(f, "String"),
            TokenType::Number => write!(f, "Number"),
            TokenType::And => write!(f, "And"),
            TokenType::Class => write!(f, "Class"),
            TokenType::Else => write!(f, "Else"),
            TokenType::False => write!(f, "False"),
            TokenType::For => write!(f, "For"),
            TokenType::Fun => write!(f, "Fun"),
            TokenType::If => write!(f, "If"),
            TokenType::Nil => write!(f, "Nil"),
            TokenType::Or => write!(f, "Or"),
            TokenType::Print => write!(f, "Print"),
            TokenType::Return => write!(f, "Return"),
            TokenType::Super => write!(f, "Super"),
            TokenType::This => write!(f, "This"),
            TokenType::True => write!(f, "True"),
            TokenType::Var => write!(f, "Var"),
            TokenType::While => write!(f, "While"),
            TokenType::Eof => write!(f, "Eof"),
        }
    }
}

#[derive(Debug)]
pub enum VMError {
    ScanError(String),
    CompileError(String),
    RuntimeError(String),
}

fn repl() -> Result<(), VMError> {
    let mut vm = VM::default();

    println!("\x1b[3mloxrust\x1b[0m - Aiden Fox Ivey (c) 2025");
    println!();

    loop {
        print!("# ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            // Slightly bespoke logic so far Ctrl+D works.
            Ok(0) | Err(_) => {
                println!();
                break;
            }
            Ok(1) => {
                break;
            }
            _ => {
                vm.interpret(&line)?;
            }
        }
    }

    Ok(())
}

fn run_file(path: &str) -> Result<(), VMError> {
    let path = PathBuf::from(path);
    if !path.exists() {
        eprintln!("rustlox cannot open path {:#?}", path);
        exit(74);
    }

    let content = fs::read_to_string(path).unwrap();

    let mut vm = VM::default();

    vm.interpret(&content)
}

fn main() -> Result<(), VMError> {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        repl()?;
    } else if args.len() == 2 {
        run_file(&args[1])?;
    } else {
        eprintln!("Usage: loxrust [path]");
        exit(64);
    }

    Ok(())
}
