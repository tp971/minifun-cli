use getopts::Options;
use minifun::{Scope, Value, intrinsic_fn, intrinsic_fn_move, parser::Parser};
use std::cell::RefCell;
use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::process;
use std::rc::Rc;

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
    minifun::insert_stdlib(&mut scope);
    scope.insert("val_dump".to_string(), intrinsic_fn! {
        (Value) -> ():
            |_, val| { println!("{}", val); Ok(()) }
    });
    scope.insert("print".to_string(), intrinsic_fn! {
        (Str) -> ():
            |_, (s, _)| { print!("{}", s); Ok(()) }
    });
    scope.insert("println".to_string(), intrinsic_fn! {
        (Str) -> ():
            |_, (s, _)| { println!("{}", s); Ok(()) }
    });
    scope.insert("eprint".to_string(), intrinsic_fn! {
        (Str) -> ():
            |_, (s, _)| { eprint!("{}", s); Ok(()) }
    });
    scope.insert("eprintln".to_string(), intrinsic_fn! {
        (Str) -> ():
            |_, (s, _)| { eprintln!("{}", s); Ok(()) }
    });

    let readln = RefCell::new(rustyline::Editor::<()>::new());
    scope.insert("read_line".to_string(), intrinsic_fn_move! {
        (Str) -> Value:
            |_, (prompt, _)| {
                let mut readln = readln.borrow_mut();
                match readln.readline(prompt) {
                    Ok(line) => {
                        if line != "" {
                            readln.add_history_entry(line.clone());
                        }
                        let size = line.chars().count() as i64;
                        Ok(Rc::new(Value::Tuple(vec![
                            Rc::new(Value::Str(line, size))
                        ])))
                    },
                    Err(_) =>
                        Ok(Rc::new(Value::Tuple(vec![])))
                }
            }
    });

    for next in matches.free.iter() {
        if next == "-" {
            process_input("stdin".to_string(), io::stdin(), &mut scope);
        } else {
            let f = match File::open(next) {
                Ok(f) => f,
                Err(e) => {
                    eprintln!("minifun: Cannot open file {}: {}", next, e);
                    continue
                }
            };
            process_input(next.clone(), f, &mut scope);
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
                            eprintln!("{}", e)
                    }
                },
                Ok(None) =>
                    break,
                Err(e) => {
                    eprintln!("{}", e);
                    parser.skipline();
                }
            };
        }
    }
}

fn process_input<R: Read>(name: String, r: R, scope: &mut Scope) {
    let mut parser = Parser::new(name, BufReader::new(r).bytes());
    let mut stmts = Vec::new();
    loop {
        match parser.parse_stmt_eof() {
            Ok(Some(stmt)) =>
                stmts.push(stmt),
            Ok(None) =>
                break,
            Err(e) => {
                eprintln!("{}", e);
                return
            }
        };
    }
    for stmt in stmts {
        match stmt.eval(scope) {
            Ok(_) => {},
            Err(e) => {
                eprintln!("{}", e);
                return
            }
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
