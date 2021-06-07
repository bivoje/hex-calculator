#![allow(dead_code, unused_mut, unused_variables)]

mod expr;

use expr::{WithPos, Error, ErrorKind::*};

fn print_errmsg_at(s :&str, start :usize, end :usize, msg :&str) {
  println!("\x1b[31;1merror\x1b[0m[{}-{}]: {}", start, end, msg);
  println!("\x1b[34mline|\x1b[0m   {}", s);
  // TODO color the error range
  println!("\x1b[34m    |\x1b[0m   {}{}", " ".repeat(start), "^".repeat(end-start+1));
}

fn print_errmsg(s :&str, msg :&str) {
  println!("\x1b[31;1merror\x1b[0m: {}", msg);
}

fn print_error(s :&str, WithPos {start, end1, val:e} :Error) {

  // when problem is on the whole sentence, 'end1' is set to 0
  let end = if end1 == 0 {s.len()} else {end1 - 1};

  match e {
    // LexError
    UnknownOperator => print_errmsg_at(s, start, end,
      "unknown operator. currently supported operators are '+', '-', '*', '/'"),
    InvalidNum => print_errmsg_at(s, start, end,
      "invalid number representation"),

    // EvalError // TODO check parsing error/ malformed first
    DivisionByZero => print_errmsg_at(s, start, end, "divided by 0"),
    DivRem(a,b) => {
      let msg = format!("{} / {} = {} ... {}", a, b, a/b, a % b);
      print_errmsg_at(s, start, end, &msg);
    },

    // OverallError
    EmptyExpr => print_errmsg(s, "the expression is empty"),
    NotEnoughOperator => print_errmsg(s, "not enough operator"),
    NotEnoughOperand => print_errmsg(s, "not enough operand"),
    UnpairedParenthesis => print_errmsg_at(s, start, end,
      "parenthesis unmatched"),
  };

}

fn eval_print(s :&str) {
  match expr::process_a_line(s) {
    Ok(x) => println!("{}", x),
    Err(e) => print_error(s, e),
  };
}

fn main() {
  let s :&str = "x00FF + 2 + mo1 / 2";
  let s :&str = "(x00FF + 2 + mo1) / 2";

  let ss = match std::env::args().nth(1) {
    None => {
      println!("program needs 1 argument");
      return;
    },
    Some(s) => s,
  };

  eval_print(&ss);
}
