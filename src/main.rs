mod chunk;
mod scanner;
mod vm;

use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::{env, io, process::exit};

use vm::{VM, VMError};

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
