pub mod parser;
pub mod value;
pub use self::value::*;

use std::cmp;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct EvalError {
    token: TokenInfo,
    desc: String
}

impl EvalError {
    pub fn new(token: &TokenInfo, desc: String) -> EvalError {
        EvalError { token: token.clone(), desc }
    }

    pub fn with_token(self, token: &TokenInfo) -> EvalError {
        EvalError { token: token.clone(), desc: self.desc }
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}: {}", self.token.source, self.token.row, self.token.col, self.desc)
    }
}

pub type Result<T> = std::result::Result<T, EvalError>;
pub type Scope = std::collections::HashMap<String, Rc<Value>>;

#[derive(Debug, Clone)]
pub enum Token {
    EOF,
    ID(String), TRUE, FALSE,
    INT(i64), REAL(f64), CHAR(char), STRING(String),
    LPAR, RPAR, COMMA, LBRACE, RBRACE,
    FN, RIGHTARROW, IF, THEN, ELSE,
    SEMICOLON, LET, FUN,
    PLUS, MINUS, BANG, STAR, SLASH, PERCENT,
    AND, PIPE, HAT,
    LESS, LESSEQ, GREATER, GREATEREQ,
    EQ, BANGEQ, ANDAND, PIPEPIPE
}

impl Token {
    pub fn prec(&self) -> (i16, i16) {
        match self {
            Token::PIPEPIPE =>
                (0, 1),
            Token::ANDAND =>
                (2, 3),
            Token::EQ | Token::BANGEQ =>
                (4, 5),
            Token::LESS | Token::LESSEQ | Token::GREATER | Token::GREATEREQ =>
                (6, 7),
            Token::PIPE =>
                (8, 9),
            Token::HAT =>
                (10, 11),
            Token::AND =>
                (12, 13),
            Token::PLUS | Token::MINUS =>
                (14, 15),
            Token::STAR | Token::SLASH | Token::PERCENT =>
                (16, 17),
            _ =>
                (-1, -1)
        }
    }

    pub fn max_prec() -> i16 {
        18
    }

    fn fmt_esc(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Token::EOF => 
                write!(f, "end of file"),
            Token::ID(id) if id == "" => 
                write!(f, "identifier"),
            Token::ID(id) => 
                write!(f, "identifier `{}`", id),
            Token::STRING(_) => 
                write!(f, "string constant"),
            _ =>
                write!(f, "`{}`", self)
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::EOF => write!(f, "end of file"),
            Token::ID(id) => write!(f, "{}", id),
            Token::TRUE => write!(f, "true"),
            Token::FALSE => write!(f, "false"),
            Token::INT(num) => write!(f, "{}", num),
            Token::REAL(num) => write!(f, "{}", num),
            Token::CHAR(c) => write!(f, "{:?}", c),
            Token::STRING(s) => write!(f, "{:?}", s),
            Token::LPAR => write!(f, "("),
            Token::RPAR => write!(f, ")"),
            Token::COMMA => write!(f, ","),
            Token::LBRACE => write!(f, "{{"),
            Token::RBRACE => write!(f, "}}"),
            Token::FN => write!(f, "fn"),
            Token::RIGHTARROW => write!(f, "=>"),
            Token::IF => write!(f, "if"),
            Token::THEN => write!(f, "then"),
            Token::ELSE => write!(f, "else"),
            Token::SEMICOLON => write!(f, ";"),
            Token::LET => write!(f, "let"),
            Token::FUN => write!(f, "fun"),
            Token::PLUS => write!(f, "+"),
            Token::MINUS => write!(f, "-"),
            Token::BANG => write!(f, "!"),
            Token::STAR => write!(f, "*"),
            Token::SLASH => write!(f, "/"),
            Token::PERCENT => write!(f, "%"),
            Token::AND => write!(f, "&"),
            Token::PIPE => write!(f, "|"),
            Token::HAT => write!(f, "^"),
            Token::LESS => write!(f, "<"),
            Token::LESSEQ => write!(f, "<="),
            Token::GREATER => write!(f, ">"),
            Token::GREATEREQ => write!(f, ">="),
            Token::EQ => write!(f, "="),
            Token::BANGEQ => write!(f, "!="),
            Token::ANDAND => write!(f, "&&"),
            Token::PIPEPIPE => write!(f, "||")
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenInfo {
    pub source: String,
    pub row: usize,
    pub col: usize,
    pub token: Token
}

impl fmt::Display for TokenInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.token.fmt_esc(f)
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Empty,
    Exp(TokenInfo, Rc<Exp>),
    Let(TokenInfo, Rc<Exp>, Rc<Exp>),
    Fun(TokenInfo, String, Vec<Rc<Exp>>, Rc<Exp>)
}

#[derive(Debug, Clone)]
pub enum Exp {
    ID(TokenInfo, String),
    BoolConst(TokenInfo, bool),
    IntConst(TokenInfo, i64),
    RealConst(TokenInfo, f64),
    CharConst(TokenInfo, char),
    StrConst(TokenInfo, String),
    Tuple(TokenInfo, Vec<Rc<Exp>>),
    Unary(TokenInfo, Token, Rc<Exp>),
    Binary(TokenInfo, Token, Rc<Exp>, Rc<Exp>),
    If(TokenInfo, Rc<Exp>, Rc<Exp>, Rc<Exp>),
    FnExp(TokenInfo, Rc<Exp>, Rc<Exp>),
    App(TokenInfo, Rc<Exp>, Rc<Exp>),
    Block(TokenInfo, Vec<Rc<Stmt>>, Option<Rc<Exp>>),

    //used for partial evaluation
    Value(Rc<Value>),
    Scope(Scope, Rc<Exp>)
}

#[derive(Debug, Clone)]
pub enum Partial {
    Value(Rc<Value>),
    Exp(Rc<Exp>)
}

impl Partial {
    fn into_value(self) -> Rc<Value> {
        match self {
            Partial::Value(val) => val,
            _ => panic!("Partial::into_value on non-value")
        }
    }

    fn into_exp(self) -> Rc<Exp> {
        match self {
            Partial::Value(val) => Rc::new(Exp::Value(val)),
            Partial::Exp(exp) => exp
        }
    }
}

pub fn assign_arg(scope: &mut Scope, arg: &Rc<Exp>, val: &Rc<Value>) -> Result<()> {
    match (arg.as_ref(), val.as_ref()) {
        (Exp::ID(_, id), _) => {
            scope.insert(id.clone(), Rc::clone(val));
            Ok(())
        },
        (Exp::Tuple(token, args), Value::Tuple(vals)) => {
            if args.len() != vals.len() {
                return Err(EvalError::new(token,
                    format!("cannot assign {} = {}: lengths do not match", *arg, *val)));
            }
            for (arg, val) in args.iter().zip(vals.iter()) {
                assign_arg(scope, &arg, &val)?;
            }
            Ok(())
        },
        (Exp::Tuple(token, _), val) => {
            Err(EvalError::new(token,
                format!("cannot assign {} = {}: rhs is not tuple", *arg, *val)))
        }
        (_, _) => {
            panic!("This should not happen: assign_arg: arg is not ID or Tuple")
        }
    }
}

impl Stmt {
    pub fn eval(&self, scope: &mut Scope) -> Result<Option<Rc<Value>>> {
        match self {
            Stmt::Empty =>
                Ok(None),
            Stmt::Exp(_, exp) =>
                Ok(Some(exp.eval(scope)?)),
            Stmt::Let(token, arg, exp) => {
                let val = exp.eval(scope)?;
                match assign_arg(scope, arg, &val) {
                    Ok(()) => Ok(None),
                    Err(err) => Err(err.with_token(token))
                }
            },
            Stmt::Fun(_, id, args, exp) => {
                let scope2 = scope.clone();
                scope.insert(id.clone(), Rc::new(
                    Value::FnVal(scope2, Some(id.clone()), args.clone(), Rc::clone(exp))));
                Ok(None)
            }
        }
    }
}

const MAX_DEPTH: usize = 64;

impl Exp {
    pub fn eval(&self, scope: &Scope) -> Result<Rc<Value>> {
        let mut res = self.eval_max(scope, MAX_DEPTH)?;
        loop {
            match res {
                Partial::Value(val) => {
                    break Ok(val);
                },
                Partial::Exp(exp) => {
                    //eprintln!("{}", exp);
                    let depth = exp.depth();
                    res = exp.eval_max(scope,
                        (depth + MAX_DEPTH - 1) / MAX_DEPTH * MAX_DEPTH)?;
                }
            }
        }
    }

    pub fn eval_max(&self, scope: &Scope, depth: usize) -> Result<Partial> {
        if depth == 0 {
            return Ok(Partial::Exp(Rc::new(self.clone())))
        }

        match self {
            Exp::ID(token, id) =>
                match scope.get(id) {
                    Some(val) =>
                        Ok(Partial::Value(Rc::clone(val))),
                    None =>
                        Err(EvalError::new(token,
                            format!("unbound identifier `{}`", id)))
                },
            Exp::BoolConst(_, b) =>
                Ok(Partial::Value(Rc::new(Value::Bool(*b)))),
            Exp::IntConst(_, num) =>
                Ok(Partial::Value(Rc::new(Value::Int(*num)))),
            Exp::RealConst(_, num) =>
                Ok(Partial::Value(Rc::new(Value::Real(*num)))),
            Exp::CharConst(_, c) =>
                Ok(Partial::Value(Rc::new(Value::Char(*c)))),
            Exp::StrConst(_, s) =>
                Ok(Partial::Value(Rc::new(Value::Str(s.clone(), s.chars().count() as i64)))),

            Exp::Tuple(token, exps) => {
                let mut res = Vec::new();
                let mut is_value = true;
                for exp in exps {
                    let val = exp.eval_max(scope, depth - 1)?;
                    match val {
                        Partial::Value(_) => {},
                        _ => { is_value = false; }
                    }
                    res.push(val);
                }

                if is_value {
                    Ok(Partial::Value(Rc::new(Value::Tuple(
                        res.into_iter().map(Partial::into_value).collect()))))
                } else {
                    Ok(Partial::Exp(Rc::new(Exp::Tuple(token.clone(),
                        res.into_iter().map(Partial::into_exp).collect()))))
                }
            },

            Exp::Unary(token, op, rhs) => {
                let rval = rhs.eval_max(scope, depth - 1)?;
                let rval = match rval {
                    Partial::Value(val) => val,
                    rpart => {
                        return Ok(Partial::Exp(
                            Rc::new(Exp::Unary(token.clone(), op.clone(), rpart.into_exp()))));
                    }
                };

                match rval.eval_unary(&token.token) {
                    Ok(val) => Ok(Partial::Value(val)),
                    Err(e) => Err(EvalError::new(&token, e))
                }
            },

            Exp::Binary(token, op, lhs, rhs) => {
                match token.token {
                    Token::ANDAND | Token::PIPEPIPE => {
                        let lval = lhs.eval_max(scope, depth - 1)?;
                        let lval = match lval {
                            Partial::Value(val) => val,
                            lpart =>
                                return Ok(Partial::Exp(
                                    Rc::new(Exp::Binary(token.clone(), op.clone(), lpart.into_exp(), Rc::clone(rhs)))))
                        };

                        let short =
                            if let Token::ANDAND = token.token {
                                false
                            } else {
                                true
                            };

                        return Ok(match lval.as_ref() {
                            Value::Bool(b) if *b == short =>
                                Partial::Value(Rc::new(Value::Bool(short))),
                            Value::Bool(_) => {
                                let rval = rhs.eval_max(scope, depth - 1)?;
                                let rval = match rval {
                                    Partial::Value(val) => val,
                                    rpart =>
                                        return Ok(Partial::Exp(
                                            Rc::new(Exp::Binary(token.clone(), op.clone(),
                                                Rc::new(Exp::Value(Rc::clone(&lval))),
                                                rpart.into_exp()))))
                                };
                                
                                match rval.as_ref() {
                                    Value::Bool(b) =>
                                        Partial::Value(Rc::new(Value::Bool(*b))),
                                    _ =>
                                        return Err(EvalError::new(&token,
                                            format!("invalid values for binary {}: _, {}", token, rval)))
                                }
                            },
                            _ => 
                                return Err(EvalError::new(&token,
                                    format!("invalid values for binary {}: {}, _", token, lval)))
                        })
                    },
                    _ => {}
                }

                let lval = lhs.eval_max(scope, depth - 1)?;
                let rval = rhs.eval_max(scope, depth - 1)?;
                let (lval, rval) = match (lval, rval) {
                    (Partial::Value(lval), Partial::Value(rval)) => (lval, rval),
                    (lpart, rpart) => {
                        return Ok(Partial::Exp(
                            Rc::new(Exp::Binary(token.clone(), op.clone(), lpart.into_exp(), rpart.into_exp()))));
                    }
                };

                match lval.eval_binary(&token.token, rval.as_ref()) {
                    Ok(val) => Ok(Partial::Value(val)),
                    Err(e) => Err(EvalError::new(&token, e))
                }
            },

            Exp::If(token, cond, texp, fexp) => {
                let cval = cond.eval_max(scope, depth - 1)?;
                let cval = match cval {
                    Partial::Value(val) => val,
                    cpart => {
                        return Ok(Partial::Exp(Rc::new(
                            Exp::If(token.clone(), cpart.into_exp(), Rc::clone(texp), Rc::clone(fexp)))));
                    }
                };

                match cval.as_ref() {
                    Value::Bool(true) =>
                        texp.eval_max(scope, depth - 1),
                    Value::Bool(false) =>
                        fexp.eval_max(scope, depth - 1),
                    val =>
                        Err(EvalError::new(&token,
                            format!("invalid value for if condition: {}", val)))
                }
            }

            Exp::FnExp(_, arg, exp) =>
                Ok(Partial::Value(Rc::new(
                    Value::FnVal(scope.clone(), None, vec![Rc::clone(arg)], Rc::clone(exp))))),

            Exp::App(token, func, arg) => {
                let fval = func.eval_max(scope, depth - 1)?;
                let aval = arg.eval_max(scope, depth - 1)?;

                let (fval, aval) = match (fval, aval) {
                    (Partial::Value(fval), Partial::Value(aval)) => (fval, aval),
                    (fpart, apart) => {
                        return Ok(Partial::Exp(Rc::new(
                            Exp::App(token.clone(), fpart.into_exp(), apart.into_exp()))));
                    }
                };

                match fval.as_ref() {
                    Value::FnVal(scope, name, fargs, fexp) => {
                        let mut scope2 = scope.clone();
                        if let Some(name) = name {
                            scope2.insert(name.clone(), Rc::clone(&fval));
                        }
                        assign_arg(&mut scope2, &fargs[0], &aval)
                            .map_err(|err| err.with_token(token))?;
                        if fargs.len() > 1 {
                            Ok(Partial::Value(Rc::new(
                                Value::FnVal(scope2, None, (&fargs[1..]).to_vec(), Rc::clone(fexp)))))
                        } else {
                            Ok(match fexp.eval_max(&scope2, depth - 1)? {
                                Partial::Exp(exp) =>
                                    if let Exp::Scope(_, _) = exp.as_ref() {
                                        Partial::Exp(exp)
                                    } else {
                                        Partial::Exp(Rc::new(
                                            Exp::Scope(scope2, exp)))
                                    },
                                v => v
                            })
                        }
                    },
                    Value::Tuple(vals) => {
                        match aval.as_ref() {
                            Value::Tuple(vals2) if vals2.is_empty() =>
                                Ok(Partial::Value(Rc::new(Value::Int(vals.len() as i64)))),
                            Value::Int(num) => {
                                if *num < 0 || *num as usize >= vals.len() {
                                    Err(EvalError::new(&token,
                                        format!("out of range: {} {}", fval, aval)))
                                } else {
                                    Ok(Partial::Value(Rc::clone(&vals[*num as usize])))
                                }
                            }
                            _ =>
                                Err(EvalError::new(&token,
                                    format!("invalid index: {} {}", fval, aval)))
                        }
                    },
                    Value::Str(s, len) => {
                        match aval.as_ref() {
                            Value::Tuple(vals) if vals.is_empty() =>
                                Ok(Partial::Value(Rc::new(Value::Int(*len)))),
                            Value::Int(off) if *off >= 0 && off < len =>
                                Ok(Partial::Value(Rc::new(
                                    Value::Char(s.chars().nth(*off as usize).unwrap())))),
                            Value::Tuple(vals) if vals.len() == 2 => {
                                let (off, count) = match (vals[0].as_ref(), vals[1].as_ref()) {
                                    (Value::Int(off), Value::Int(count))
                                        if off < len && *count >= 0 && off + count <= *len =>
                                            (off, count),
                                    _ => return Err(EvalError::new(&token,
                                        format!("invalid index: {} {}", fval, aval)))
                                };
                                Ok(Partial::Value(Rc::new(
                                    Value::Str(s.chars().skip(*off as usize).take(*count as usize).collect(), *count))))
                            }
                            _ =>
                                Err(EvalError::new(&token,
                                    format!("invalid index: {} {}", fval, aval)))
                        }
                    },
                    val =>
                        Err(EvalError::new(&token,
                            format!("cannot call value: {}", val)))
                }
            },

            Exp::Block(_, stmts, exp) => {
                let mut scope2 = scope.clone();
                for stmt in stmts {
                    stmt.eval(&mut scope2)?;
                }
                match exp {
                    Some(exp) =>
                        Ok(match exp.eval_max(&scope2, depth - 1)? {
                            Partial::Exp(exp) =>
                                if let Exp::Scope(_, _) = exp.as_ref() {
                                    Partial::Exp(exp)
                                } else {
                                    Partial::Exp(Rc::new(
                                        Exp::Scope(scope2, exp)))
                                },
                            v => v
                        }),
                    None => Ok(Partial::Value(Rc::new(Value::Tuple(vec![]))))
                }
            },

            Exp::Value(val) =>
                Ok(Partial::Value(Rc::clone(val))),

            Exp::Scope(scope2, exp) => {
                Ok(match exp.eval_max(scope2, depth - 1)? {
                    Partial::Exp(exp) =>
                        if let Exp::Scope(_, _) = exp.as_ref() {
                            Partial::Exp(exp)
                        } else {
                            Partial::Exp(Rc::new(
                                Exp::Scope(scope2.clone(), exp)))
                        },
                    v => v
                })
            }
        }
    }

    pub fn depth(&self) -> usize {
        match self {
            Exp::ID(_, _)
          | Exp::BoolConst(_, _)
          | Exp::IntConst(_, _)
          | Exp::RealConst(_, _)
          | Exp::CharConst(_, _)
          | Exp::StrConst(_, _)
          | Exp::FnExp(_, _, _)
          | Exp::Value(_) =>
                1,
            Exp::Tuple(_, exps) => {
                let mut depth = 0;
                for next in exps {
                    depth = cmp::max(depth, next.depth());
                }
                1 + depth
            },
            Exp::Unary(_, _, rhs) =>
                1 + rhs.depth(),
            Exp::Binary(_, _, lhs, rhs) =>
                1 + cmp::max(lhs.depth(), rhs.depth()),
            Exp::If(_, cond, texp, fexp) =>
                1 + cmp::max(cond.depth(), cmp::max(texp.depth(), fexp.depth())),
            Exp::App(_, func, arg) =>
                1 + cmp::max(func.depth(), arg.depth()),
            Exp::Block(_, _, Some(exp)) =>
                1 + exp.depth(),
            Exp::Block(_, _, None) =>
                1,
            Exp::Scope(_, exp) =>
                1 + exp.depth()
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Empty =>
                write!(f, ";"),
            Stmt::Exp(_, exp) =>
                write!(f, "{};", exp),
            Stmt::Let(_, arg, exp) =>
                write!(f, "let {} = {};", arg, exp),
            Stmt::Fun(_, name, args, exp) => {
                write!(f, "fun {}", name)?;
                for next in args {
                    write!(f, " {}", next)?;
                }
                write!(f, " = {};", exp)
            }
        }
    }
}

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Exp::ID(_, id) =>
                write!(f, "{}", id),
            Exp::BoolConst(_, b) =>
                write!(f, "{}", b),
            Exp::IntConst(_, num) =>
                write!(f, "{}", num),
            Exp::RealConst(_, num) =>
                write!(f, "{}", num),
            Exp::StrConst(_, s) =>
                write!(f, "{:?}", s),
            Exp::CharConst(_, c) =>
                write!(f, "{:?}", c),
            Exp::Tuple(_, exps) => {
                write!(f, "(")?;
                for (i, next) in exps.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", next)?;
                }
                write!(f, ")")
            },
            Exp::Unary(_, op, rhs) =>
                write!(f, "({}{})", op, rhs),
            Exp::Binary(_, op, lhs, rhs) =>
                write!(f, "({} {} {})", lhs, op, rhs),
            Exp::If(_, cond, texp, fexp) =>
                write!(f, "(if {} then {} else {})", cond, texp, fexp),
            Exp::FnExp(_, arg, exp) =>
                write!(f, "(fn {} => {})", arg, exp),
            Exp::App(_, func, arg) =>
                write!(f, "({} {})", func, arg),
            Exp::Block(_, stmts, exp) => {
                write!(f, "{{ ")?;
                for next in stmts {
                    write!(f, "{} ", next)?;
                }
                match exp {
                    Some(exp) => write!(f, "{} }}", exp),
                    None => write!(f, "}}")
                }
            },
            Exp::Value(val) =>
                write!(f, "$( {} )", val),
            Exp::Scope(_, exp) =>
                write!(f, "${{ {} }}", exp)
        }
    }
}
