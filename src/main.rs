mod dot;
mod jvm;
mod parser;
mod types;
mod x64;

use nom::Err;
use nom::error::convert_error;
use nom::error::VerboseError;
use std::fs::OpenOptions;
use std::io;
use std::io::Read;
use std::process::exit;

enum Format {
    DOT,
    X64,
    JVM, // the minijava vm, not the real one lool
}

fn main() {
    let mut format = Format::DOT;
    let mut inputfile: Option<String> = None;
    for arg in std::env::args().skip(1) {
        if arg == "-h" {
            println!(
                "Usage: {} [-h] format prog.minijava",
                std::env::args().nth(0).unwrap_or(String::from("minajava"))
            );
            println!("\n\t-h\tshows this help");
            println!("\tformat\toutput format: one of '-x64' '-dot' '-jvm'");
            println!("\t\t-dot:\tSyntax tree in graphviz dot format.");
            println!("\t\t-jvm:\tInstructions for the MiniJVM");
            println!("\t\t-x64:\tJIT compile to x64 code and execute");
            return;
        }
        if arg == "-x64" {
            format = Format::X64;
        } else if arg == "-dot" {
            format = Format::DOT;
        } else if arg == "-jvm" {
            format = Format::JVM;
        } else {
            if inputfile != None {
                eprintln!("Unknown option '{}', try -h for a list.", arg);
                exit(1);
            }
            inputfile = Some(arg.to_string());
        }
    }

    let mut source = String::new();

    match inputfile {
        None => { io::stdin().read_to_string(&mut source).unwrap(); },
        Some(filename) => {
            let mut f = OpenOptions::new().read(true).open(filename).unwrap();
            f.read_to_string(&mut source).unwrap();
        },
    }

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
        Format::JVM => jvm::dump(&ast),
        Format::X64 => x64::run(&ast),
    }
}
