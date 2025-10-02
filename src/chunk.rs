use crate::vm::Value;

#[derive(Default)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
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
        pub enum OpCode {
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
