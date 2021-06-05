#![allow(dead_code, unused_mut, unused_variables)]
#![feature(never_type)]

mod expr;

fn main() {
  //let s :&str = "x00FF + 2 + mo1 / 2";
  //let s :&str = "(x00FF + 2 + mo1) / 2";

  let ss = match std::env::args().nth(1) {
    None => {
      println!("program needs 1 argument");
      return;
    },
    Some(s) => s,
  };

  let s = &ss;
  expr::process_a_line(s);
}
