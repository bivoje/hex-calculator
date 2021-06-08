
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


#[derive(Debug, PartialEq, Eq)]
enum TokenKernel<'a> {
  Operator(&'a str),
  Number(bool, u32, &'a str), // (is_positive, radix, num)
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
      //TODO static ref RE_CMT :Regex = Regex::new(r#
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

#[test]
fn test_tokenizer_basic_parasing() {
  let mut t = tokenize("  \t0 mx78AD+");
  assert_eq!(t.next().unwrap().val, Number(true, 10, "0"));
  assert_eq!(t.next().unwrap().val, Number(false, 16, "78AD"));
  assert_eq!(t.next().unwrap().val, Operator("+"));
  assert_eq!(t.next(), None);
}

#[test]
fn test_tokenizer_operator_exception() {
  let x :Option<u32> = None;
  let mut t = tokenize("/(/1@+])^,$");
  assert_eq!(t.next().unwrap().val, Operator("/"));
  assert_eq!(t.next().unwrap().val, Parenthesis(true));
  assert_eq!(t.next().unwrap().val, Operator("/"));
  assert_eq!(t.next().unwrap().val, Number(true, 10, "1"));
  assert_eq!(t.next().unwrap().val, Operator("@+]"));
  assert_eq!(t.next().unwrap().val, Parenthesis(false));
  assert_eq!(t.next().unwrap().val, Operator("^,$"));
  assert_eq!(t.next(), None);
}

#[test]
fn test_tokenizer_numeric_bases() {
  let mut t = tokenize("bABxABmb11o11md11+m77");
  assert_eq!(t.next().unwrap().val, Number(true, 2, "AB"));
  assert_eq!(t.next().unwrap().val, Number(true, 16, "AB"));
  assert_eq!(t.next().unwrap().val, Number(false, 2, "11"));
  assert_eq!(t.next().unwrap().val, Number(true, 8, "11"));
  assert_eq!(t.next().unwrap().val, Number(false, 10, "11"));
  assert_eq!(t.next().unwrap().val, Operator("+"));
  assert_eq!(t.next().unwrap().val, Number(false, 10, "77"));
  assert_eq!(t.next(), None);
}

fn test_tokenizer_ignore_whitespaces() {
  // TODO test on various wsps characters?
  assert_eq!(
    tokenize("3+4+5+7+8").collect::<Vec<_>>(),
    tokenize("\t3 +    4 + \t5 + 7 \t+ 8   ").collect::<Vec<_>>());
  assert_eq!(
    tokenize("3*97-m3//(*b077073+++47___xFFE").collect::<Vec<_>>(),
    tokenize("3  * 97- m3 // (* b077073 +++47 ___ xFFE").collect::<Vec<_>>());
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

#[test]
fn test_lexer_numeric_base() {
  assert_eq!(lexer(Number(true, 2, "AB")), Err(InvalidNum));
  assert_eq!(lexer(Number(true, 16, "AB")), Ok(Num(171)));
  assert_eq!(lexer(Number(false, 2, "11")), Ok(Num(-3)));
  assert_eq!(lexer(Number(true, 8, "11")), Ok(Num(9)));
  assert_eq!(lexer(Number(false, 10, "11")), Ok(Num(-11)));
  assert_eq!(lexer(Operator("+")), Ok(Op(Add)));
  assert_eq!(lexer(Number(false, 10, "77")), Ok(Num(-77)));
  assert_eq!(lexer(Number(false, 9, "77")), Ok(Num(-70)));
  assert_eq!(lexer(Number(true, 4, "13")), Ok(Num(7)));
  assert_eq!(lexer(Number(true, 4, "14")), Err(InvalidNum));
  // TODO shouldn't buffer overflow treated separately?
  assert_eq!(lexer(Number(true, 16, "F_FFFF_FFFF_FFFF_FFFF_FFFF_FFFFF_FFFF_FFFF")), Err(InvalidNum));
}

#[test]
fn test_lexer_unknown_token() {
  assert_eq!(lexer(Unknown), Err(UnknownToken));
  assert_eq!(lexer(Operator("**")), Err(UnknownOperator));
  assert_eq!(lexer(Operator("+*+")), Err(UnknownOperator));
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

#[test]
fn test_reverse_polish_assoc_ltor() {
  assert_eq!(reverse_polish(vec![ // "mx3 + d84 + b001011 + 734"
    WithPos { start: 0, end1: 3, val: Num(-3) },
    WithPos { start: 4, end1: 5, val: Op(Add) },
    WithPos { start: 6, end1: 9, val: Num(84) },
    WithPos { start: 10, end1: 11, val: Op(Add) },
    WithPos { start: 12, end1: 19, val: Num(11) },
    WithPos { start: 20, end1: 21, val: Op(Add) },
    WithPos { start: 22, end1: 25, val: Num(734) }
  ]), Ok(vec![ // mx3 d84 + b001011 + 734 +
    WithPos { start: 0, end1: 3, val: Num(-3) },
    WithPos { start: 6, end1: 9, val: Num(84) },
    WithPos { start: 4, end1: 5, val: Op(Add) },
    WithPos { start: 12, end1: 19, val: Num(11) },
    WithPos { start: 10, end1: 11, val: Op(Add) },
    WithPos { start: 22, end1: 25, val: Num(734) },
    WithPos { start: 20, end1: 21, val: Op(Add) },
  ]));

  assert_eq!(reverse_polish(vec![ // "mx3 * d84 * b001011 * 734"
    WithPos { start: 0, end1: 3, val: Num(-3) },
    WithPos { start: 4, end1: 5, val: Op(Mul) },
    WithPos { start: 6, end1: 9, val: Num(84) },
    WithPos { start: 10, end1: 11, val: Op(Mul) },
    WithPos { start: 12, end1: 19, val: Num(11) },
    WithPos { start: 20, end1: 21, val: Op(Mul) },
    WithPos { start: 22, end1: 25, val: Num(734) }
  ]), Ok(vec![ // mx3 d84 + b001011 + 734 +
    WithPos { start: 0, end1: 3, val: Num(-3) },
    WithPos { start: 6, end1: 9, val: Num(84) },
    WithPos { start: 4, end1: 5, val: Op(Mul) },
    WithPos { start: 12, end1: 19, val: Num(11) },
    WithPos { start: 10, end1: 11, val: Op(Mul) },
    WithPos { start: 22, end1: 25, val: Num(734) },
    WithPos { start: 20, end1: 21, val: Op(Mul) },
  ]));
}

#[test]
fn test_reverse_polish_op_prec() {
  assert_eq!(reverse_polish(vec![ // "mx3 * d84 + b001011 - 734"
    WithPos { start: 0, end1: 3, val: Num(-3) },
    WithPos { start: 4, end1: 5, val: Op(Mul) },
    WithPos { start: 6, end1: 9, val: Num(84) },
    WithPos { start: 10, end1: 11, val: Op(Add) },
    WithPos { start: 12, end1: 19, val: Num(11) },
    WithPos { start: 20, end1: 21, val: Op(Sub) },
    WithPos { start: 22, end1: 25, val: Num(734) },
  ]), Ok(vec![ // mx3 d84 * b001011 734 - +
    WithPos { start: 0, end1: 3, val: Num(-3) },
    WithPos { start: 6, end1: 9, val: Num(84) },
    WithPos { start: 4, end1: 5, val: Op(Mul) },
    WithPos { start: 12, end1: 19, val: Num(11) },
    WithPos { start: 22, end1: 25, val: Num(734) },
    WithPos { start: 20, end1: 21, val: Op(Sub) },
    WithPos { start: 10, end1: 11, val: Op(Add) },
  ]));

  assert_eq!(reverse_polish(vec![ // "mx3 + d84 / b001011 * 734"
    WithPos { start: 0, end1: 3, val: Num(-3) },
    WithPos { start: 4, end1: 5, val: Op(Add) },
    WithPos { start: 6, end1: 9, val: Num(84) },
    WithPos { start: 10, end1: 11, val: Op(Div) },
    WithPos { start: 12, end1: 19, val: Num(11) },
    WithPos { start: 20, end1: 21, val: Op(Mul) },
    WithPos { start: 22, end1: 25, val: Num(734) },
  ]), Ok(vec![ // mx3 d84 b001011 / 734 * +
    WithPos { start: 0, end1: 3, val: Num(-3) },
    WithPos { start: 6, end1: 9, val: Num(84) },
    WithPos { start: 12, end1: 19, val: Num(11) },
    WithPos { start: 4, end1: 5, val: Op(Div) },
    WithPos { start: 22, end1: 25, val: Num(734) },
    WithPos { start: 20, end1: 21, val: Op(Mul) },
    WithPos { start: 10, end1: 11, val: Op(Add) },
  ]));
}

#[test]
fn test_reverse_polish_paren_pairing() {
  assert_eq!(reverse_polish(vec![
    // "() 3 - () 7 () " empty parenthesis pair is simply ignored
    WithPos { start: 0, end1: 1, val: Paren(true) },
    WithPos { start: 1, end1: 2, val: Paren(false) },
    WithPos { start: 3, end1: 4, val: Num(3) },
    WithPos { start: 5, end1: 6, val: Op(Sub) },
    WithPos { start: 7, end1: 8, val: Paren(true) },
    WithPos { start: 8, end1: 9, val: Paren(false) },
    WithPos { start: 10, end1: 11, val: Num(7) },
    WithPos { start: 12, end1: 13, val: Paren(true) },
    WithPos { start: 13, end1: 14, val: Paren(false) },
  ]), Ok(vec![ // 3 - 7
    WithPos { start: 3, end1: 4, val: Num(3) },
    WithPos { start: 10, end1: 11, val: Num(7) },
    WithPos { start: 5, end1: 6, val: Op(Sub) },
  ]));

  assert_eq!(reverse_polish(vec![
    // "((mx3) * ((d84) + b001011)) - 734"
    WithPos { start: 0, end1: 1, val: Paren(true) },
    WithPos { start: 1, end1: 2, val: Paren(true) },
    WithPos { start: 2, end1: 5, val: Num(-3) },
    WithPos { start: 5, end1: 6, val: Paren(false) },
    WithPos { start: 7, end1: 8, val: Op(Mul) },
    WithPos { start: 9, end1: 10, val: Paren(true) },
    WithPos { start: 10, end1: 11, val: Paren(true) },
    WithPos { start: 11, end1: 14, val: Num(84) },
    WithPos { start: 14, end1: 15, val: Paren(false) },
    WithPos { start: 16, end1: 17, val: Op(Add) },
    WithPos { start: 18, end1: 25, val: Num(11) },
    WithPos { start: 25, end1: 26, val: Paren(false) },
    WithPos { start: 26, end1: 27, val: Paren(false) },
    WithPos { start: 28, end1: 29, val: Op(Sub) },
    WithPos { start: 30, end1: 33, val: Num(734) },
  ]), Ok(vec![ // mx3 d84 b001011 + * 734 -
    WithPos { start: 2, end1: 5, val: Num(-3) },
    WithPos { start: 11, end1: 14, val: Num(84) },
    WithPos { start: 18, end1: 25, val: Num(11) },
    WithPos { start: 16, end1: 17, val: Op(Add) },
    WithPos { start: 7, end1: 8, val: Op(Mul) },
    WithPos { start: 30, end1: 33, val: Num(734) },
    WithPos { start: 28, end1: 29, val: Op(Sub) },
  ]));
}

#[test]
fn test_reverse_polish_paren_prec() {
  assert_eq!(reverse_polish(vec![ // "mx3 * (d84 + b001011) - 734"
    WithPos { start: 0, end1: 3, val: Num(-3) },
    WithPos { start: 4, end1: 5, val: Op(Mul) },
    WithPos { start: 6, end1: 7, val: Paren(true) },
    WithPos { start: 7, end1: 10, val: Num(84) },
    WithPos { start: 11, end1: 12, val: Op(Add) },
    WithPos { start: 13, end1: 20, val: Num(11) },
    WithPos { start: 20, end1: 21, val: Paren(false) },
    WithPos { start: 22, end1: 23, val: Op(Sub) },
    WithPos { start: 24, end1: 27, val: Num(734) },
  ]), Ok(vec![ // mx3 d84 b001011 + * 734 -
    WithPos { start: 0, end1: 3, val: Num(-3) },
    WithPos { start: 7, end1: 10, val: Num(84) },
    WithPos { start: 13, end1: 20, val: Num(11) },
    WithPos { start: 11, end1: 12, val: Op(Add) },
    WithPos { start: 4, end1: 5, val: Op(Mul) },
    WithPos { start: 24, end1: 27, val: Num(734) },
    WithPos { start: 22, end1: 23, val: Op(Sub) },
  ]));
}

#[test]
fn test_reverse_polish_unpaired_paren() {
  assert_eq!(reverse_polish(vec![
    WithPos { start: 0, end1:1, val: Paren(true) },
  ]), Err(WithPos {start: 0, end1: 1, val: UnpairedParenthesis}));

  assert_eq!(reverse_polish(vec![
    WithPos { start: 0, end1:1, val: Paren(false) },
  ]), Err(WithPos {start: 0, end1: 1, val: UnpairedParenthesis}));

  assert_eq!(reverse_polish(vec![ // 3+(1+(2*1)
    WithPos { start: 0, end1: 1, val: Num(3) },
    WithPos { start: 1, end1: 2, val: Op(Add) },
    WithPos { start: 2, end1: 3, val: Paren(true) },
    WithPos { start: 3, end1: 4, val: Num(1) },
    WithPos { start: 4, end1: 5, val: Op(Add) },
    WithPos { start: 5, end1: 6, val: Paren(true) },
    WithPos { start: 6, end1: 7, val: Num(2) },
    WithPos { start: 7, end1: 8, val: Op(Mul) },
    WithPos { start: 8, end1: 9, val: Num(1) },
    WithPos { start: 9, end1: 10, val: Paren(false) },
  ]), Err(WithPos {start: 2, end1: 3, val: UnpairedParenthesis}));

  assert_eq!(reverse_polish(vec![ // (1*2)+1)+3
    WithPos { start: 9, end1: 10, val: Paren(false) },
    WithPos { start: 8, end1: 9, val: Num(1) },
    WithPos { start: 7, end1: 8, val: Op(Mul) },
    WithPos { start: 6, end1: 7, val: Num(2) },
    WithPos { start: 5, end1: 6, val: Paren(true) },
    WithPos { start: 4, end1: 5, val: Op(Add) },
    WithPos { start: 3, end1: 4, val: Num(1) },
    WithPos { start: 1, end1: 2, val: Op(Add) },
    WithPos { start: 2, end1: 3, val: Paren(true) },
    WithPos { start: 0, end1: 1, val: Num(3) },
  ]), Err(WithPos {start: 7, end1: 8, val: UnpairedParenthesis}));
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

#[test]
fn test_evaluate() {
  let wp = |x :LexKernel| WithPos {start:0, end1:0, val:x};

  assert_eq!(evaluate(vec![ // mx3 d84 + b001011 + 734 +
    wp(Num(-3)), wp(Num(84)), wp(Op(Add)), wp(Num(11)),
    wp(Op(Add)), wp(Num(734)), wp(Op(Add)),
  ]), Ok(826));

  assert_eq!(evaluate(vec![ // "mx3 * d84 * b001011 * 734"
    wp(Num(-3)), wp(Num(84)), wp(Op(Mul)), wp(Num(11)),
    wp(Op(Mul)), wp(Num(734)), wp(Op(Mul)),
  ]), Ok(-2034648));

  assert_eq!(evaluate(vec![ // "mx3 d84 * b001011 734 - +"
    wp(Num(-3)), wp(Num(84)), wp(Op(Mul)), wp(Num(11)),
    wp(Num(734)), wp(Op(Sub)), wp(Op(Add)),
  ]), Ok(-975));

  assert_eq!(evaluate(vec![ // mx3 d84 b001011 / 734 * +
    wp(Num(-3)), wp(Num(84)), wp(Num(11)), wp(Op(Add)),
    wp(Op(Mul)), wp(Num(734)), wp(Op(Sub)),
  ]), Ok(-1019));

  // TODO more complex expressions?
}

#[test]
fn test_eval_malformed_expr() {
  // TODO is this really effectful?
  let wp = |x :LexKernel| WithPos {start:0, end1:0, val:x};
  let wp1 = |x :ErrorKind| WithPos {start:0, end1:0, val:x};
  assert_eq!(evaluate(vec![ // mx3 d84 b001011 / 734 *
    wp(Num(-3)), wp(Num(84)), wp(Num(11)), wp(Op(Add)),
    wp(Op(Mul)), wp(Num(734)),
  ]), Err(wp1(NotEnoughOperator)));

  assert_eq!(evaluate(vec![ // mx3 d84 / *
    wp(Num(-3)), wp(Num(84)), wp(Op(Add)), wp(Op(Mul)), 
  ]), Err(wp1(NotEnoughOperand)));

  assert_eq!(evaluate(vec![ // mx3 d84 b001011 / 734 *
  ]), Err(wp1(EmptyExpr)));
}

#[test]
fn test_eval_arithmatic_error() {
  let wp = |x :LexKernel| WithPos {start:0, end1:0, val:x};
  let wp1 = |x :ErrorKind| WithPos {start:0, end1:0, val:x};
  assert_eq!(evaluate(vec![ // mx3 1 1 - /
    wp(Num(-3)), wp(Num(1)), wp(Num(1)), wp(Op(Sub)), wp(Op(Div)),
  ]), Err(wp1(DivisionByZero)));

  assert_eq!(evaluate(vec![ // mx3 1 1 - /
    wp(Num(-3)), wp(Num(4)), wp(Num(1)), wp(Op(Add)), wp(Op(Div)),
  ]), Err(wp1(DivRem(-3,5))));
}


pub fn process_a_line(s :&str) -> Result<i64,Error> {
  let ls :Vec<_> =
    tokenize(s).map(|wpt| wpt.map2(lexer))
    .collect::<Result<Vec<_>,_>>()?;

  let rp = reverse_polish(ls)?;

  evaluate(rp)
}


#[test]
fn test_integral() {
  let s :&str = "(x00FF + 2 + mo1) / 2";
  assert_eq!(process_a_line(s), Ok(128));
}
