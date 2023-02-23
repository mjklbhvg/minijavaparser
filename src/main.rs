mod dot;
mod jvm;
mod parser;
mod types;
mod x64;

use std::io::{self, Read};
use std::process::exit;
use nom::error::VerboseError;
use nom::Err;
use nom::error::convert_error;

enum Format {
    DOT,
    X64,
    JVM, // the minijava vm, not the real one lool
}

fn main() {
    let mut format = Format::DOT;
    for arg in std::env::args().skip(1) {
        if arg == "-h" {
            println!(
                "Usage: {} [-h] format",
                std::env::args().nth(0).unwrap_or(String::from("minajava"))
            );
            println!("\n\t-h\tshows this help");
            println!("\tformat\toutput format: one of '-x64' '-dot' '-jvm'");
            println!("\n\tInput is read from stdin and output written to stdout.");
            return;
        }
        if arg == "-x64" {
            format = Format::X64;
        } else if arg == "-dot" {
            format = Format::DOT;
        } else if arg == "-jvm" {
            format = Format::JVM;
        } else {
            eprintln!("Unknown format '{}', try -h for a list.", arg);
            exit(1);
        }
    }

    let mut source = String::new();
    io::stdin().read_to_string(&mut source).unwrap();
    let ast = match parser::parse_program::<VerboseError<&str>>(&source) {
        Ok((_, ast)) => ast,
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            eprintln!("{}", convert_error(source.as_str(), e));
            exit(2);
        },
        _ => exit(3),
    };

    match format {
        Format::DOT => dot::dump(&ast),
        Format::X64 => x64::dump(&ast),
        Format::JVM => jvm::dump(&ast),
    }
}
