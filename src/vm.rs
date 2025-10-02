use std::ops::Neg;

use crate::chunk::{Chunk, OpCode};
use crate::scanner::Scanner;

pub type Value = f64;

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

#[derive(Debug)]
pub enum VMError {
    CompileError(String),
    RuntimeError(String),
}
