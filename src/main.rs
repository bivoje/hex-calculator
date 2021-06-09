#![allow(dead_code, unused_mut, unused_variables, unreachable_code)]

mod expr;

use expr::{WithPos, Error, ErrorKind::*};

fn print_errmsg_at(s :&str, start :usize, end :usize, msg :&str) {
  println!("\x1b[31;1merror\x1b[0m[{}-{}]: {}", start, end, msg);
  println!("\x1b[34mline|\x1b[0m   {}\x1b[43;30m{}\x1b[0m{}",
     &s[0..start], &s[start..end+1], &s[end+1..]);
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
    UnknownToken => print_errmsg_at(s, start, end,
      "unknown token starting with"),
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

fn print_num(x :i64, radix :u32) {
  match radix {
    2  => println!("b{:b}", x),
    8  => println!("o{:o}", x),
    10 => println!("{}"   , x),
    16 => println!("x{:X}", x),
    _ => panic!("{}", radix),
  }
}

fn main() {
  match std::env::args().nth(1) {
    None => println!("program needs at least 1 argument"),
    Some(s) if s == "-c" => {
      //let s :String = std::env::args().skip(2).collect();
      let ss :Vec<String> = std::env::args().skip(2).collect();
      let s = ss.join(" "); // TODO use itertools::intersperse?
      match expr::process_a_line(&s) {
        Ok((x,r)) => print_num(x,r),
        Err(e) => print_error(&s, e),
      };
    },
    Some(s) if s == "-i" => {
      loop {
        print!("> ");
        let mut s = String::new();
        let succ = std::io::stdin().read_line(&mut s);
        if succ.is_err() { println!("io error: {:?}", succ); break; } 
        if succ.ok() == Some(0) { println!(""); break; } // at EOF, ctrl-D

        match expr::process_a_line(&s) {
          Ok((x,r)) => print_num(x,r),
          Err(e) if e.val == EmptyExpr => println!("empty {:?} {:?}", e, s),
          Err(e) => print_error(&s[0..s.len()-1], e), // trim newline
        };
      }
    },
    Some(s) => println!("unrecognized option {}",  s),
  };
}
