use getopts::Options;
use minifun::{Scope, parser::Parser};
use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::process;

fn main() {
    let mut opts = Options::new();
    opts.optflag("i", "interactive", "Interactive mode (default if no file is given)");
    opts.optflagmulti("v", "verbose", "Verbose output");
    opts.optflag("h", "help", "Show help");
    
    let args: Vec<String> = env::args().collect();
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            eprintln!("minifun: {}", f);
            eprintln!("Try 'minifun --help' for more information.");
            process::exit(1)
        }
    };

    if matches.opt_present("help") {
        eprint!("{}", opts.usage("Usage: minifun [options] [files ...]"));
        process::exit(0)
    }

    let mut scope = Scope::new();

    for next in matches.free.iter() {
        let f = match File::open(next) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("minifun: Cannot open file {}: {}", next, e);
                continue
            }
        };
        let mut parser = Parser::new(next.clone(), BufReader::new(f).bytes());
        loop {
            match parser.parse_stmt_eof() {
                Ok(Some(stmt)) => {
                    match stmt.eval(&mut scope) {
                        Ok(_) => {},
                        Err(e) => {
                            println!("{}", e);
                            continue
                        }
                    }
                },
                Ok(None) =>
                    break,
                Err(e) => {
                    println!("{}", e);
                    continue
                }
            };
        }
    }

    if matches.free.is_empty() || matches.opt_present("interactive") {
        let mut parser = Parser::new(String::from("stdin"),
            RLRead::new(rustyline::Editor::new()));

        loop {
            parser.input_mut().prompt();
            match parser.parse_stmt_eof() {
                Ok(Some(stmt)) => {
                    match stmt.eval(&mut scope) {
                        Ok(Some(val)) =>
                            println!("{}", val),
                        Ok(None) =>
                            {},
                        Err(e) =>
                            println!("{}", e)
                    }
                },
                Ok(None) =>
                    break,
                Err(e) => {
                    println!("{}", e);
                    parser.skipline();
                }
            };
        }
    }
}

struct RLRead {
    editor: rustyline::Editor<()>,
    line: std::vec::IntoIter<u8>,
    eof: bool,
    do_prompt: bool
}

impl RLRead {
    fn new(editor: rustyline::Editor<()>) -> RLRead {
        RLRead { editor, line: vec![].into_iter(), eof: false, do_prompt: false }
    }

    fn prompt(&mut self) {
        self.do_prompt = true;
    }
}

impl Iterator for RLRead {
    type Item = io::Result<u8>;

    fn next(&mut self) -> Option<io::Result<u8>> {
        if self.eof {
            return None;
        }

        match self.line.next() {
            Some(b) => Some(Ok(b)),
            None => {
                let res =
                    if self.do_prompt {
                        self.do_prompt = false;
                        self.editor.readline("> ")
                    } else {
                        self.editor.readline("  ")
                    };
                match res {
                    Ok(mut line) => {
                        if line != "" {
                            self.editor.add_history_entry(line.clone());
                        }
                        line.push('\n');
                        self.line = line.into_bytes().into_iter();
                        Some(Ok(self.line.next().unwrap()))
                    },
                    //TODO: error handling
                    _ => {
                        self.eof = true;
                        None
                    }
                }
            }
        }
    }
}
