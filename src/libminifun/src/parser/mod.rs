mod lexer;
use std::fmt;
use std::io;
use std::rc::Rc;

pub use super::{Token, TokenInfo, Exp, Stmt};
pub use self::lexer::Lexer;

pub type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug, Clone)]
pub enum ParserError {
    Lexer(usize, usize, String),
    Unexpected(TokenInfo, Vec<Token>)
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::Lexer(row, col, desc) =>
                write!(f, "{}:{}: {}", row, col, desc),
            ParserError::Unexpected(token, expected) => {
                write!(f, "{}:{}: unexpected {}", token.row, token.col, token)?;
                if !expected.is_empty() {
                    write!(f, ", expected ")?;
                    for (i, next) in expected.iter().enumerate() {
                        if i > 0 {
                            if i == expected.len() {
                                write!(f, " or ")?;
                            } else {
                                write!(f, ", ")?;
                            }
                        }
                        next.fmt_esc(f)?;
                    }
                }
                Ok(())
            }
        }
    }
}

pub struct Parser<I: Iterator<Item = io::Result<u8>>> {
    lexer: Lexer<I>
}

impl<I: Iterator<Item = io::Result<u8>>> Parser<I> {
    pub fn new(source: String, input: I) -> Parser<I> {
        Parser {
            lexer: Lexer::new(source, input, 1)
        }
    }

    pub fn with_offset(source: String, input: I, row: usize, col: usize) -> Parser<I> {
        Parser {
            lexer: Lexer::with_offset(source, input, 1, row, col)
        }
    }

    pub fn row(&self) -> usize {
        self.lexer.row()
    }

    pub fn col(&self) -> usize {
        self.lexer.col()
    }

    pub fn ch(&self) -> Option<char> {
        self.lexer.ch()
    }

    pub fn into_iter(self) -> I {
        self.lexer.into_iter()
    }

    pub fn input_mut(&mut self) -> &mut I {
        self.lexer.input_mut()
    }

    pub fn skipline(&mut self) {
        self.lexer.skipline();
    }

    pub fn parse_stmt_eof(&mut self) -> Result<Option<Rc<Stmt>>> {
        match self.lexer.peek(0)?.token {
            Token::EOF => Ok(None),
            _ => Ok(Some(self.parse_stmt()?))
        }
    }

    pub fn parse_stmt(&mut self) -> Result<Rc<Stmt>> {
        let mut t = self.lexer.peek(0)?;
        match t.token {
            Token::SEMICOLON => {
                self.lexer.next();
                Ok(Rc::new(Stmt::Empty))
            },
            Token::LET => {
                let token = t;
                self.lexer.next();
                let arg = self._parse_arg()?;
                t = self.lexer.peek(0)?;
                match t.token {
                    Token::EQ => {},
                    _ =>
                        return Err(ParserError::Unexpected(t,
                            vec![Token::EQ]))
                }
                self.lexer.next();
                let exp = self.parse_exp()?;
                t = self.lexer.peek(0)?;
                if let Token::SEMICOLON = t.token {
                    self.lexer.next();
                } else {
                    return Err(ParserError::Unexpected(t,
                        vec![Token::SEMICOLON]))
                }
                Ok(Rc::new(Stmt::Let(token, arg, exp)))
            },
            Token::FUN => {
                let token = t;
                self.lexer.next();
                t = self.lexer.peek(0)?;
                let name = match t.token {
                    Token::ID(name) => name,
                    _ =>
                        return Err(ParserError::Unexpected(t,
                            vec![Token::ID(String::new())]))
                };
                self.lexer.next();
                let mut args = Vec::new();
                args.push(self._parse_arg()?);
                loop {
                    t = self.lexer.peek(0)?;
                    match t.token {
                        Token::EQ => {
                            self.lexer.next();
                            break;
                        },
                        _ => {
                            args.push(self._parse_arg()?);
                        }
                    }
                }
                let exp = self.parse_exp()?;
                t = self.lexer.peek(0)?;
                if let Token::SEMICOLON = t.token {
                    self.lexer.next();
                } else {
                    return Err(ParserError::Unexpected(t,
                        vec![Token::SEMICOLON]))
                }
                Ok(Rc::new(Stmt::Fun(token, name, args, exp)))
            },
            _ => {
                let token = t;
                let exp = self.parse_exp()?;
                t = self.lexer.peek(0)?;
                if let Token::SEMICOLON = t.token {
                    self.lexer.next();
                } else {
                    return Err(ParserError::Unexpected(t,
                        vec![Token::SEMICOLON]))
                }
                Ok(Rc::new(Stmt::Exp(token, exp)))
            }
        }
    }

    pub fn parse_exp(&mut self) -> Result<Rc<Exp>> {
        self._parse_exp(0)
    }

    fn _parse_exp(&mut self, prec: i16) -> Result<Rc<Exp>> {
        let mut t = self.lexer.peek(0)?;
        let mut exp = match t.token {
            Token::PLUS | Token::MINUS | Token::BANG => {
                self.lexer.next();
                let exp = self._parse_exp(Token::max_prec())?;
                let token = t.token.clone();
                Rc::new(Exp::Unary(t, token, exp))
            },
            _ => {
                self._parse_primary()?
            }
        };

        t = self.lexer.peek(0)?;
        loop {
            match t.token {
                Token::ID(_) | Token::TRUE | Token::FALSE
              | Token::INT(_) | Token::REAL(_) | Token::STRING(_) | Token::CHAR(_)
              | Token::LPAR | Token::LBRACE => {
                    let token = t.clone();
                    let arg = self._parse_primary()?;
                    exp = Rc::new(Exp::App(token, exp, arg));
                    t = self.lexer.peek(0)?;
                },
                _ => { break; }
            }
        }

        loop {
            t = self.lexer.peek(0)?;
            let (lprec, rprec) = t.token.prec();
            if lprec < prec {
                return Ok(exp);
            } else {
                self.lexer.next();
                let rhs = self._parse_exp(rprec)?;
                let token = t.token.clone();
                exp = Rc::new(Exp::Binary(t, token, exp, rhs));
            }
        }
    }

    fn _parse_primary(&mut self) -> Result<Rc<Exp>> {
        let mut t = self.lexer.peek(0)?;
        match t.token.clone() {
            Token::ID(id) => {
                self.lexer.next();
                Ok(Rc::new(Exp::ID(t, id)))
            },
            Token::TRUE => {
                self.lexer.next();
                Ok(Rc::new(Exp::BoolConst(t, true)))
            },
            Token::FALSE => {
                self.lexer.next();
                Ok(Rc::new(Exp::BoolConst(t, false)))
            },
            Token::INT(num) => {
                self.lexer.next();
                Ok(Rc::new(Exp::IntConst(t, num)))
            },
            Token::REAL(num) => {
                self.lexer.next();
                Ok(Rc::new(Exp::RealConst(t, num)))
            },
            Token::CHAR(c) => {
                self.lexer.next();
                Ok(Rc::new(Exp::CharConst(t, c)))
            },
            Token::STRING(s) => {
                self.lexer.next();
                Ok(Rc::new(Exp::StrConst(t, s)))
            },
            Token::LPAR => {
                let token = t;
                self.lexer.next();
                t = self.lexer.peek(0)?;
                if let Token::RPAR = t.token {
                    self.lexer.next();
                    return Ok(Rc::new(Exp::Tuple(token, vec![])));
                }

                let res = self.parse_exp()?;
                t = self.lexer.peek(0)?;
                match t.token {
                    Token::RPAR => {
                        self.lexer.next();
                        Ok(res)
                    },
                    Token::COMMA => {
                        let mut res = vec![res];
                        loop {
                            match t.token {
                                Token::COMMA => {
                                    self.lexer.next();
                                    res.push(self.parse_exp()?);
                                    t = self.lexer.peek(0)?;
                                },
                                Token::RPAR => {
                                    self.lexer.next();
                                    break Ok(Rc::new(Exp::Tuple(token, res)));
                                },
                                _ =>
                                    return Err(ParserError::Unexpected(t,
                                        vec![Token::RPAR, Token::COMMA]))
                            }
                        }
                    },
                    _ =>
                        return Err(ParserError::Unexpected(t,
                            vec![Token::RPAR]))
                }
            },
            Token::IF => {
                let token = t;
                self.lexer.next();
                let cond = self.parse_exp()?;
                t = self.lexer.peek(0)?;
                match t.token {
                    Token::THEN => {},
                    _ =>
                        return Err(ParserError::Unexpected(t,
                            vec![Token::THEN]))
                }
                self.lexer.next();
                let etrue = self.parse_exp()?;
                t = self.lexer.peek(0)?;
                match t.token {
                    Token::ELSE => {},
                    _ =>
                        return Err(ParserError::Unexpected(t,
                            vec![Token::ELSE]))
                }
                self.lexer.next();
                let efalse = self.parse_exp()?;
                Ok(Rc::new(Exp::If(token, cond, etrue, efalse)))
            }
            Token::FN => {
                let token = t;
                self.lexer.next();
                let arg = self._parse_arg()?;
                t = self.lexer.peek(0)?;
                match t.token {
                    Token::RIGHTARROW => {},
                    _ =>
                        return Err(ParserError::Unexpected(t,
                            vec![Token::RIGHTARROW]))
                }
                self.lexer.next();
                let exp = self.parse_exp()?;
                Ok(Rc::new(Exp::FnExp(token, arg, exp)))
            },
            Token::LBRACE => {
                let token = t;
                self.lexer.next();
                let mut res = Vec::new();
                loop {
                    t = self.lexer.peek(0)?;
                    match t.token {
                        Token::SEMICOLON | Token::LET | Token::FUN => {
                            res.push(self.parse_stmt()?);
                        },
                        Token::RBRACE => {
                            self.lexer.next();
                            return Ok(Rc::new(Exp::Block(token, res, None)));
                        },
                        _ => {
                            let token = t;
                            let exp = self.parse_exp()?;
                            t = self.lexer.peek(0)?;
                            match t.token {
                                Token::SEMICOLON => {
                                    self.lexer.next();
                                    res.push(Rc::new(Stmt::Exp(t, exp)));
                                },
                                Token::RBRACE => {
                                    self.lexer.next();
                                    return Ok(Rc::new(Exp::Block(token, res, Some(exp))));
                                },
                                _ => {
                                    return Err(ParserError::Unexpected(t,
                                        vec![Token::SEMICOLON, Token::RBRACE]))
                                }
                            }
                        }
                    }
                }
            },
            _ =>
                Err(ParserError::Unexpected(t, vec![]))
        }
    }

    fn _parse_arg(&mut self) -> Result<Rc<Exp>> {
        let mut t = self.lexer.peek(0)?;
        match t.token.clone() {
            Token::ID(id) => {
                self.lexer.next();
                Ok(Rc::new(Exp::ID(t, id)))
            },
            Token::LPAR => {
                let token = t;
                self.lexer.next();
                t = self.lexer.peek(0)?;
                if let Token::RPAR = t.token {
                    self.lexer.next();
                    return Ok(Rc::new(Exp::Tuple(token, vec![])));
                }

                let res = self._parse_arg()?;
                t = self.lexer.peek(0)?;
                match t.token {
                    Token::RPAR => {
                        self.lexer.next();
                        Ok(res)
                    },
                    Token::COMMA => {
                        let mut res = vec![res];
                        loop {
                            match t.token {
                                Token::COMMA => {
                                    self.lexer.next();
                                    res.push(self._parse_arg()?);
                                    t = self.lexer.peek(0)?;
                                },
                                Token::RPAR => {
                                    self.lexer.next();
                                    break Ok(Rc::new(Exp::Tuple(token, res)));
                                },
                                _ =>
                                    break Err(ParserError::Unexpected(t,
                                        vec![Token::RPAR, Token::COMMA]))
                            }
                        }
                    },
                    _ =>
                        Err(ParserError::Unexpected(t,
                            vec![Token::RPAR]))
                }
            },
            _ =>
                Err(ParserError::Unexpected(t, vec![]))
        }
    }
}
