
#[derive(Debug)]
pub struct WithPos<T> {
  // [start, end1)
  pub start :usize,
  pub end1 :usize,
  pub val :T,
}

impl<T> WithPos<T> {
  fn map<U,F>(self, f :F) -> WithPos<U> where F :FnOnce(T) -> U {
    WithPos {
      start: self.start,
      end1: self.end1,
      val: f(self.val),
    }
  }

  fn map2<U,R,F>(self, f :F) -> Result<WithPos<U>,WithPos<R>>
  where F :FnOnce(T) -> Result<U,R> {
    match f(self.val) {
      Ok(u)  =>  Ok(WithPos {start:self.start, end1:self.end1, val:u}),
      Err(r) => Err(WithPos {start:self.start, end1:self.end1, val:r}),
    }
  }
}

impl<T> PartialEq for WithPos<T> where T :PartialEq {
  fn eq(&self, other :&Self) -> bool {
    self.val == other.val
  }
}

impl<T> PartialOrd for WithPos<T> where T :PartialOrd {
  fn partial_cmp(&self, other :&Self) -> Option<Ordering> {
    self.val.partial_cmp(&other.val)
  }
}


#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
  // LexError
  UnknownOperator,
  UnknownToken,
  InvalidNum,

  // EvalError
  DivisionByZero, 
  DivRem(i64, i64),

  // Malformed Expression
  EmptyExpr,
  UnpairedParenthesis,
  NotEnoughOperator,
  NotEnoughOperand,
}
use ErrorKind::*;
pub type Error = WithPos<ErrorKind>;

// for whatever the T is, creates an 'Error' of given 'kind' at
// position indicated by 'wpl'
fn error<T,U>(wpl :&WithPos<T>, kind :ErrorKind)
-> Result<U,Error> {
  error_at(wpl.start, wpl.end1, kind)
}

// creates 'Error' of 'kind' at given position
fn error_at<U>(start :usize, end1 :usize, kind :ErrorKind)
-> Result<U,Error> {
  Err(WithPos { start, end1, val: kind })
}


#[derive(Debug)]
enum TokenKernel<'a> {
  Operator(&'a str),
  Number(bool, u32, &'a str), // (is_negative, radix, num)
  Parenthesis(bool), // is opening paren??
  Unknown,
}
use TokenKernel::*;
type Token<'a> = WithPos<TokenKernel<'a>>;

struct Tokenizer<'a> {
  buf :&'a str,
  at :usize,
}

use regex::Regex;
use lazy_static::lazy_static;

impl<'a> Iterator for Tokenizer<'a> {
  type Item = Token<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    lazy_static! {
      static ref RE_OP :Regex = Regex::new(r#"(?x) ^(
        ! | @ | / | & | = | > | , | - | ~ | % | < | ; | `
        \# | \\ | \. | \+ | \* | \? | \| | \[ | \] | \{ | \} | \^ | \$
        )+"#).unwrap();
      static ref RE_NUM :Regex = Regex::new(r#"(?x) ^
        (m)?(b|o|d|x)?([0-9A-F]+)
      "#).unwrap();
      static ref RE_PRN :Regex = Regex::new(r#"^\(|\)"#).unwrap();
      static ref RE_SPS :Regex = Regex::new(r#"^\s*"#).unwrap();
    }

    // skip white spaces
    self.at += RE_SPS.find(&self.buf[self.at..]).unwrap().end();
    if self.at == self.buf.len() { return None; }

    // starting position of this token
    let start = self.at;

    let (len, val) =
    if let Some(cap) = RE_NUM.captures(&self.buf[self.at..]) {
      let sign = ! cap.get(1).is_some();
      let radix = match cap.get(2).map(|m| m.as_str()) {
        Some("b") => 2,
        Some("o") => 8,
        Some("d") => 10,
        Some("x") => 16,
        Some(_) => panic!(), // not possible
        None => 10,
      };
      let digits = cap.get(3)?.as_str();

      let len = cap.get(0).unwrap().end();
      (len, Number(sign, radix, digits))

    } else if let Some(mat) = RE_OP.find(&self.buf[self.at..]) {
      let len = mat.end();
      (len, Operator(&self.buf[self.at..self.at+len]))

    } else if self.buf.get(self.at..=self.at) == Some("(") {
      (1, Parenthesis(true))

    } else if self.buf.get(self.at..=self.at) == Some(")") {
      (1, Parenthesis(false))

    } else {(1, Unknown)};

    self.at += len;
    Some(WithPos { start, end1:self.at, val })
  }
}

fn tokenize<'a>(buf :&'a str) -> Tokenizer<'a> {
  Tokenizer {buf, at:0}
}


#[derive(Debug, PartialEq, Eq)]
enum LexKernel {
  Op(Ops),
  Num(i64),
  Paren(bool),
}
use LexKernel::*;
type Lex = WithPos<LexKernel>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Ops {
  Add, Sub, Mul, Div,
}

use Ops::*;
use std::cmp::Ordering;
impl PartialOrd for LexKernel {
  fn partial_cmp(&self, other :&Self) -> Option<Ordering> {
    let order = |x :&LexKernel| match *x {
      Paren(_) => 0,
      Op(Add) => 3,
      Op(Sub) => 4,
      Op(Mul) => 5,
      Op(Div) => 6,
      Num(_) => 9,
    };
    Some(order(self).cmp(&order(other)))
  }
}

fn lexer(tok :TokenKernel) -> Result<LexKernel,ErrorKind> {
  match tok {
    TokenKernel::Operator(s) => match s {
      "+" => Ok(LexKernel::Op(Add)),
      "-" => Ok(LexKernel::Op(Sub)),
      "*" => Ok(LexKernel::Op(Mul)),
      "/" => Ok(LexKernel::Op(Div)),
      _ => Err(UnknownOperator),
    },
    TokenKernel::Number(sign, radix, s) => {
      match i64::from_str_radix(s, radix) {
        Ok(val) => Ok(LexKernel::Num(val * if sign {1} else {-1})),
        Err(_) => Err(InvalidNum),
      }
    },
    TokenKernel::Parenthesis(b) => Ok(LexKernel::Paren(b)),
    TokenKernel::Unknown => Err(UnknownToken),
  }
}


fn pop_while<F,T>(d :&mut Vec<T>, v :&mut Vec<T>, f :F)
where F :Fn(&T)->bool {
  while v.last().map_or(false, |x| f(x)) { d.push(v.pop().unwrap()); }
}

fn reverse_polish(ls :Vec<Lex>)
-> Result<Vec<Lex>, Error> {

  let mut stack = Vec::new();
  let mut ret = Vec::new();
  for wpl in ls { match wpl.val {
    Paren(true) => stack.push(wpl),
    Paren(false) => {
      pop_while(&mut ret, &mut stack, |x| x.val != Paren(true));
      if ! stack.is_empty() { stack.pop(); }
      else { error(&wpl, UnpairedParenthesis)? }
    },
    ref val => {
      pop_while(&mut ret, &mut stack, |x| &x.val >= val);
      stack.push(wpl);
    },
  }}

  // flush!
  pop_while(&mut ret, &mut stack, |x| x.val != Paren(true));
  if let Some(wpl) = stack.last() {
    // it is Paren(true)
    error(wpl, UnpairedParenthesis)?
  }

  Ok(ret)
}


fn evaluate(ls :Vec<Lex>) -> Result<i64,Error> {
  let mut stack = Vec::new();

  let ex = |o| match o {
    Add => |a, b| a + b,
    Sub => |a, b| a - b,
    Mul => |a, b| a * b,
    Div => |a, b| a / b,
  };

  for wpl in ls { match wpl.val {
    // TODO push wpl, and merge two range when calculating,
    // improve operand's error message.
    // or even, show partial calc history?
    Num(n) => stack.push(n),

    // TODO use checked_* versions?
    // and handle errors altogether?
    Op(op) => {
      let (b,a) = match (stack.pop(), stack.pop()) {
        (Some(b), Some(a)) => (b,a),
        _ => error(&wpl, NotEnoughOperand)?,
      };

      if op == Div {
        if b == 0 { error(&wpl, DivisionByZero)? }
        if a % b != 0 { error(&wpl, DivRem(a,b))? }
      }

      stack.push(ex(op)(a,b));
    },
    _ => panic!(), // not possible
  }}

  match (stack.pop(), stack.pop()) {
    (None,    None) => error_at(0, 0, EmptyExpr),
    (Some(a), None) => Ok(a),
    (Some(_), Some(_)) | _ => error_at(0, 0, NotEnoughOperator),
  }
}


pub fn process_a_line(s :&str) -> Result<i64,Error> {
  let ls :Vec<_> =
    tokenize(s).map(|wpt| wpt.map2(lexer))
    .collect::<Result<Vec<_>,_>>()?;

  let rp = reverse_polish(ls)?;

  evaluate(rp)
}
