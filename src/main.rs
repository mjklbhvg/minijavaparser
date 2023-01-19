mod types;
mod parser;
mod dot;
mod x64;

use std::io::{self, Read};

enum Flags {
    DOT,
    X64,
}

fn main() {
    let mut action = Flags::DOT;
    for arg in std::env::args() {
        if arg == "-x64" {
            action = Flags::X64;
        } else if arg == "-dot" {
            action = Flags::DOT;
        }
    }

    let mut source = String::new();
    io::stdin().read_to_string(&mut source).unwrap();
    let (_, ast) = parser::program(&source).unwrap();

    match action {
        Flags::DOT => dot::dump(&ast),
        Flags::X64 => x64::dump(&ast),
    }
}
