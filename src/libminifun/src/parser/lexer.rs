use std::collections::vec_deque::VecDeque;
use std::io;
use super::*;

pub struct Lexer<I: Iterator<Item = io::Result<u8>>> {
    source: String,
    input: I,
    row: usize,
    col: usize,
    ch: Option<char>,
    lookahead: usize,
    tokens: VecDeque<Result<TokenInfo>>
}

impl<I: Iterator<Item = io::Result<u8>>> Lexer<I> {
    pub fn new(source: String, input: I, lookahead: usize) -> Lexer<I> {
        Lexer::with_offset(source, input, lookahead, 1, 1)
    }

    pub fn with_offset(source: String, input: I, lookahead: usize, row: usize, col: usize) -> Lexer<I> {
        Lexer {
            source,
            input,
            row,
            col,
            ch: None,
            lookahead,
            tokens: VecDeque::with_capacity(lookahead)
        }
    }

    pub fn row(&self) -> usize {
        self.row
    }

    pub fn col(&self) -> usize {
        self.col
    }

    pub fn ch(&self) -> Option<char> {
        self.ch
    }

    pub fn into_iter(self) -> I {
        self.input
    }

    pub fn input_mut(&mut self) -> &mut I {
        &mut self.input
    }

    pub fn skipline(&mut self) {
        self.tokens.clear();
        loop {
            match self.ch {
                Some('\n') => break,
                None => break,
                _ => self._read()
            }
        }
    }

    pub fn peek(&mut self, i: usize) -> Result<TokenInfo> {
        assert!(i < self.lookahead);

        while i >= self.tokens.len() {
            let next = self._next();
            self.tokens.push_back(next);
        }

        self.tokens[i].clone()
    }

    pub fn next(&mut self) {
        self.tokens.pop_front();
    }

    fn _next(&mut self) -> Result<TokenInfo> {
        if self.ch.is_none() {
            self._read();
        }

        loop {
            match self.ch {
                Some(ch) if ch.is_whitespace() => self._read(),
                _ => break
            }
        }

        if self.ch.is_none() {
            return Ok(TokenInfo {
                source: self.source.clone(),
                row: self.row,
                col: self.col,
                token: Token::EOF
            });
        }

        let row = self.row;
        let col = self.col;
        let token = match self.ch {
            Some('(') => { self._read(); Ok(Token::LPAR) },
            Some(')') => { self._read(); Ok(Token::RPAR) },
            Some(',') => { self._read(); Ok(Token::COMMA) },
            Some('{') => { self._read(); Ok(Token::LBRACE) },
            Some('}') => { self._read(); Ok(Token::RBRACE) },
            Some(';') => { self._read(); Ok(Token::SEMICOLON) },
            Some('+') => { self._read(); Ok(Token::PLUS) },
            Some('-') => { self._read(); Ok(Token::MINUS) },
            Some('*') => { self._read(); Ok(Token::STAR) },
            Some('%') => { self._read(); Ok(Token::PERCENT) },
            Some('^') => { self._read(); Ok(Token::HAT) },
            Some('/') => {
                self._read();
                match self.ch {
                    Some('/') => { self.skipline(); return self._next() },
                    Some('*') => {
                        let mut last = '\0';
                        let mut depth = 1;
                        while depth > 0 {
                            self._read();
                            match (last, self.ch) {
                                ('/', Some('*')) => {
                                    depth += 1;
                                    last = '\0';
                                },
                                ('*', Some('/')) => {
                                    depth -= 1;
                                    last = '\0';
                                },
                                (_, Some(ch)) =>
                                    last = ch,
                                (_, None) =>
                                    break
                            };
                        }
                        self._read();
                        return self._next()
                    },
                    _ => Ok(Token::SLASH)
                }
            },
            Some('=') => {
                self._read();
                match self.ch {
                    Some('>') => { self._read(); Ok(Token::RIGHTARROW) },
                    _ => Ok(Token::EQ)
                }
            },
            Some('!') => {
                self._read();
                match self.ch {
                    Some('=') => { self._read(); Ok(Token::BANGEQ) },
                    _ => Ok(Token::BANG)
                }
            },
            Some('&') => {
                self._read();
                match self.ch {
                    Some('&') => { self._read(); Ok(Token::ANDAND) },
                    _ => Ok(Token::AND)
                }
            },
            Some('|') => {
                self._read();
                match self.ch {
                    Some('|') => { self._read(); Ok(Token::PIPEPIPE) },
                    _ => Ok(Token::PIPE)
                }
            },
            Some('<') => {
                self._read();
                match self.ch {
                    Some('=') => { self._read(); Ok(Token::LESSEQ) },
                    _ => Ok(Token::LESS)
                }
            },
            Some('>') => {
                self._read();
                match self.ch {
                    Some('=') => { self._read(); Ok(Token::GREATEREQ) },
                    _ => Ok(Token::GREATER)
                }
            },
            Some('\'') =>
                self._next_str(true),
            Some('"') =>
                self._next_str(false),
            Some(ch) => {
                if ch.is_alphabetic() || ch == '_' {
                    self._next_id()
                } else if ch.is_numeric() || ch == '.' {
                    self._next_num()
                } else {
                    Err(self._error_skip(format!("unexpected `{}`", ch)))
                }
            },
            None => {
                Err(self._error_skip("unexpected end of file".to_string()))
            }
        }?;

        Ok(TokenInfo { source: self.source.clone(), row, col, token })
    }

    fn _next_id(&mut self) -> Result<Token> {
        let mut name = String::new();
        loop {
            match self.ch {
                Some(ch) if ch.is_alphanumeric() || ch == '_' => {
                    name.push(ch);
                    self._read();
                },
                _ => { break; }
            }
        }

        Ok(match name.as_ref() {
            "true" => Token::TRUE,
            "false" => Token::FALSE,
            "fn" => Token::FN,
            "if" => Token::IF,
            "then" => Token::THEN,
            "else" => Token::ELSE,
            "let" => Token::LET,
            "fun" => Token::FUN,
            _ => Token::ID(name)
        })
    }

    fn _next_num(&mut self) -> Result<Token> {
        let (row, col) = (self.row, self.col);

        let mut num = String::new();
        loop {
            match self.ch {
                Some(ch) if ch.is_numeric() => {
                    num.push(ch);
                    self._read();
                },
                _ => { break; }
            }
        }
        match self.ch {
            Some('.') => {
                self._read();
                num.push('.');
                loop {
                    match self.ch {
                        Some(ch) if ch.is_numeric() => {
                            num.push(ch);
                            self._read();
                        },
                        _ => { break; }
                    }
                }
                match num.parse::<f64>() {
                    Ok(num) => Ok(Token::REAL(num)),
                    Err(_) => Err(ParserError::Lexer(row, col, "invalid number".to_string()))
                }
            },
            _ => match num.parse::<i64>() {
                Ok(num) => Ok(Token::INT(num)),
                Err(_) => Err(ParserError::Lexer(row, col, "invalid number".to_string()))
            }
        }
    }

    fn _next_str(&mut self, c: bool) -> Result<Token> {
        if c {
            match self.ch {
                Some('\'') => {},
                _ => return Err(ParserError::Lexer(self.row, self.col, "expected `\"`".to_string()))
            }
        } else {
            match self.ch {
                Some('"') => {},
                _ => return Err(ParserError::Lexer(self.row, self.col, "expected `'`".to_string()))
            }
        }

        let mut s = Vec::new();
        loop {
            match self.input.next() {
                Some(Ok(b'\'')) if c =>
                    break,
                Some(Ok(b'"')) if !c =>
                    break,
                //TODO string escape
                Some(Ok(ch)) if ch == b'\r' || ch == b'\n' => {
                    self.ch = Some(ch as char);
                    self.row += 1;
                    self.col = 0;
                    return Err(ParserError::Lexer(self.row, self.col, "unexpected newline".to_string()))
                },
                Some(Ok(ch)) =>
                    s.push(ch),
                _ =>
                    return Err(ParserError::Lexer(self.row, self.col, "unexpected end of file".to_string()))
            };
        }
        let s = String::from_utf8_lossy(&s).to_string();
        self.col += 1 + s.chars().count();
        self._read();

        if c {
            let mut it = s.chars();
            let c = match it.next() {
                Some(c) => c,
                None =>
                    return Err(ParserError::Lexer(self.row, self.col, "empty char constant".to_string()))
            };
            match it.next() {
                None =>
                    Ok(Token::CHAR(c)),
                Some(_) =>
                    Err(ParserError::Lexer(self.row, self.col, "char constant longer than one char".to_string()))
            }
        } else {
            Ok(Token::STRING(s))
        }
    }

    fn _error_skip(&mut self, desc: String) -> ParserError {
        let res = ParserError::Lexer(self.row, self.col, desc);
        self._read();
        res
    }

    fn _read(&mut self) {
        let ch = match self.input.next() {
            Some(Ok(ch)) if ch <= 127u8 => Some(ch as char),
            Some(Ok(_)) => Some(std::char::REPLACEMENT_CHARACTER),
            _ => None
        };

        match ch {
            Some('\r') | Some('\n') => {
                match (self.ch, ch) {
                    (Some('\r'), Some('\n')) => {},
                    _ => {
                        self.row += 1;
                        self.col = 0;
                    }
                }
            },
            _ => {
                if self.ch.is_some() {
                    self.col += 1;
                }
            }
        }
        self.ch = ch;
    }
}
