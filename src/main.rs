#![allow(dead_code, unused_mut, unused_variables)]

mod expr;

use expr::{WithPos, ErrorKind::*};

fn main() {
  //let s :&str = "x00FF + 2 + mo1 / 2";
  //let s :&str = "(x00FF + 2 + mo1) / 2";

  // Read
  let ss = match std::env::args().nth(1) {
    None => {
      println!("program needs 1 argument");
      return;
    },
    Some(s) => s,
  };

  // Eval
  let ret =  expr::process_a_line(&ss);

  // Print
  let WithPos {start, mut end, val:e} = match ret {
    Ok(x) => return println!("{}", x),
    Err(e) => e,
  };
  if end == 0 {end = ss.len()}

  let message = match e {
    // LexError
    UnknownOperator => "unknown operator. currently supported operators are '+', '-', '*', '/'".to_string(),

    InvalidNum => "invalid number representation".to_string(),

    // ParseError
    UnpairedParenthesis => "parenthesis unmatched".to_string(),

    // EvalError
    NotEnoughOperand => "not enough operand for an operator".to_string(), // TODO 'reverse_polish' could scramble the order
    DivisionByZero => "divided by 0".to_string(),
    DivRem(a,b) => format!("division '{} / {}' leaves remainder {}",
                           a, b, a % b), // TODO show more briefly
    
    // OverallError // TODO show differently?
    EmptyExpr => "the expression is empty".to_string(), // TODO wrap with "
    MalformedExpr => "malformed expression".to_string(),
  };

  println!("error[{}-{}]: {}", start, end as i32-1, message);
  println!("line| {}", ss);
  println!("      {}{}", " ".repeat(start), "^".repeat(end-start));
}
