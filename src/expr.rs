
#[derive(Debug)]
struct WithPos<T> {
  start :usize,
  end :usize,
  val :T,
}

impl<T> WithPos<T> {
  fn map<U,F>(self, f :F) -> WithPos<U> where F :FnOnce(T) -> U {
    WithPos {
      start: self.start,
      end: self.end,
      val: f(self.val),
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

#[derive(Debug)]
enum Token<'a> {
  Operator(&'a str),
  Number(bool, u32, &'a str), // (is_negative, radix, num)
  Parenthesis(bool), // is opening paren??
}
use Token::*;

struct Tokenizer<'a> {
  buf :&'a str,
  at :usize,
}

use regex::Regex;
use lazy_static::lazy_static;

impl<'a> Iterator for Tokenizer<'a> {
  type Item = WithPos<Token<'a>>;

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

    // starting position of this token
    let start = self.at;

    let oval =
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
      Some((len, Number(sign, radix, digits)))

    } else if let Some(mat) = RE_OP.find(&self.buf[self.at..]) {
      let len = mat.end();
      Some((len, Operator(&self.buf[self.at..self.at+len])))

    } else if self.buf.get(self.at..=self.at) == Some("(") {
      Some((1, Token::Parenthesis(true)))

    } else if self.buf.get(self.at..=self.at) == Some(")") {
      Some((1, Token::Parenthesis(false)))

    } else {None};

    oval.map(|(len, val)| {
      self.at += len;
      WithPos { start, end:self.at-1, val }
    })
  }
}

fn tokenize<'a>(buf :&'a str) -> Tokenizer<'a> {
  Tokenizer {buf, at:0}
}

#[derive(Debug, PartialEq, Eq)]
enum Lex {
  Op(Ops),
  Num(i64),
  Paren(bool),
}
use Lex::*;

use std::cmp::Ordering;

impl PartialOrd for Lex {
  fn partial_cmp(&self, other :&Self) -> Option<Ordering> {
    let order = |x :&Lex| match *x {
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

#[derive(Debug, PartialEq, Eq)]
enum Ops {
  Add, Sub, Mul, Div,
}
use Ops::*;

#[derive(Debug)]
enum LexError {
  UnknownOperator,
  NumError(String),
}

impl LexError {
  fn show_pretty(&self, s :&str) -> String {
    match self {
      LexError::UnknownOperator => {
        format!("'{}' {}. {}.",
          s, "is none of known operators",
          "currently supported operators are '+', '-', '*', '/'")
      },
      LexError::NumError(s) => s.to_string(),
    }
  }
}

fn lexer(tok :Token) -> Result<Lex,LexError> {
  match tok {
    Token::Operator(s) => match s {
      "+" => Ok(Lex::Op(Add)),
      "-" => Ok(Lex::Op(Sub)),
      "*" => Ok(Lex::Op(Mul)),
      "/" => Ok(Lex::Op(Div)),
      _ => Err(LexError::UnknownOperator),
    },
    Token::Number(sign, radix, s) => {
      match i64::from_str_radix(s, radix) {
        Ok(val) => Ok(Lex::Num(val * if sign {1} else {-1})),
        Err(pie) => Err(LexError::NumError(pie.to_string())),
      }
    },
    Token::Parenthesis(b) => Ok(Lex::Paren(b)),
  }
}

fn pop_while<F,T>(d :&mut Vec<T>, v :&mut Vec<T>, f :F)
where F :Fn(&T)->bool {
  while v.last().map_or(false, |x| f(x)) { d.push(v.pop().unwrap()); }
}

#[derive(Debug)]
enum ParseError {
  UnpairedParenthesis(usize),
}
use ParseError::*;

fn reverse_polish(ls :Vec<WithPos<Lex>>)
-> Result<Vec<WithPos<Lex>>, ParseError> {

  let mut stack = Vec::new();
  let mut ret = Vec::new();
  for wpl in ls { match wpl.val {
    Paren(true) => stack.push(wpl),
    Paren(false) => {
      pop_while(&mut ret, &mut stack, |x| x.val != Paren(true));
      if ! stack.is_empty() { stack.pop(); }
      else { return Err(UnpairedParenthesis(wpl.start)); }
    },
    ref val => {
      pop_while(&mut ret, &mut stack, |x| &x.val >= val);
      stack.push(wpl);
    },
  }}

  // flush!
  pop_while(&mut ret, &mut stack, |x| x.val != Paren(true));
  if let Some(WithPos{start, ..}) = stack.last() {
    // it is Paren(true)
    return Err(UnpairedParenthesis(*start));
  } else {
    Ok(ret)
  }
}

fn evaluate(ls :Vec<WithPos<Lex>>) -> Result<i64,String> {
  let mut stack = Vec::new();

  let ex = |o| match o {
    Add => |a, b| a + b,
    Sub => |a, b| a - b,
    Mul => |a, b| a * b,
    Div => |a, b| a / b,
  };

  for wpl in ls { match wpl.val {
    Num(n) => stack.push(n),

    // TODO use checked_* versions?
    // and handle errors altogether?
    Op(op) => {
      let (b,a) = match (stack.pop(), stack.pop()) {
        (Some(b), Some(a)) => (b,a),
        _ => { return Err(format!(
          "not enough operand for operator at '{}-{}'", 
          wpl.start, wpl.end));
        },
      };

      if op == Div {
        if b == 0 { return Err(format!(
          "devided by 0 at {}-{}", wpl.start, wpl.end));
        }
        if a % b != 0 { return Err(format!(
          "division with remainder at {}/{}", a, b));
        }
      }

      stack.push(ex(op)(a,b));
    },
    _ => panic!(), // not possible
  }}

  match stack.len() {
    0 => Err(format!("empty expression")),
    1 => Ok(stack.pop().unwrap()),
    _ => Err(format!("no operand for operand {}-{}", 0,0)),
    // TODO
  }
}

pub fn process_a_line(s :&str) {
  let rls :Result<Vec<_>,_> = tokenize(s).map(|wpt|
    match lexer(wpt.val) {
      Ok(val) => Ok(WithPos { start:wpt.start, end:wpt.end, val }),
      Err(v) => Err(WithPos { start:wpt.start, end:wpt.end, val:v }),
    }
  ).collect();

  let ls = match rls {
    Err(WithPos {start, end, val}) => {
      println!("error[{}-{}]: {}", start, end,
               val.show_pretty(&s[start..end]));
      println!("line: {}", s);
      println!("      {}{}", " ".repeat(start), "^".repeat(end-start+1));
      return;
    },
    Ok(ls) => ls,
  };

  let rp = match reverse_polish(ls) {
    Err(UnpairedParenthesis(at)) => {
      println!("line: {}", s);
      println!("      {}{} {}",
               " ".repeat(at), "^".repeat(1),
               "unpaired parenthesis");
      return;
    },
    Ok(rp) => rp,
  };

  match evaluate(rp) {
    Err(s) => println!("error: {}", s),
    Ok(val) => println!("{}", val),
  }
}
