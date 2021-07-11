# hc

Simple calculator / radix converter for terminal.

Do you frequently convert hexadecimal numbers into decimal form or vice versa?
Perpetual needing for conduct arithmetic between numbers in various radix?
What is the hex address of 274 bytes off from `0x4086A32C`?
Such problems keep spring out while programming, especially when dealing with binary data.
Yet there is no this-is-it solution presented around us.

Some mentally calculate. Some uses `echo 'ibase=16; <hex>' | bc` to convert to decimal then scribble on paper. Some hacks `python -c 'print(<expr>)'`. But these are not so catchy. `python` is too heavy and may not be available on some systems. `bc` doesn't handle mixed radix in an expression. Of course, brain computational resource is too precious to spare on hex arithmetic while coding.

So, here `hc` is. A simple, lightweight, handy calculator that is capable of various redix.


## How to install

You must install `rustup` and `cargo` to build `rust` source code. Follow instruction [here](https://www.rust-lang.org/tools/install).

After that,
```sh
cargo install --git https://github.com/bivoje/hex-calculator.git
```
<!-- or `git clone https://github.com/bivoje/hex-calculator.bit;`cargo install --path hex-calculator` -->
will install executable `hc` on your system.


## Syntax

An `hc` expression consists of some components. Any whitespace (including newline) is ignored.

Empty expression (containing only whitespaces and/or comment) is considered an error to help locating bugs when `hc` is used in complex shell script.

### literal

```regex
(m)?(b|o|d|x)?([0-9A-F]+)
```
A number that is represented by 64-bit signed integer internally.
e.g. `-36 => m36 = md36`, `0xf3 => xF3 = o353 = b11110011`

Consists of 3 parts.

##### sign

```regex
(m)?
```
To avoid ambiguity and keep the parser simpler, I decided not to use unary operator `-` for negation. Instead, prefix `m` indicates the additive inverse of suffix literal.

##### radix

```regex
(b|o|d|x)?
```
Prefix `b`, `o`, `d`, `x` indicates binary, octal, decimal, hexadecimal radix, respectively. If omitted, radix defaults to decimal.

##### digit

```regex
(m)?(b|o|d|x)?([0-9A-F]+)
```
`hc` allows only upper-case letter as hexadecimal digit. This is intentional, to discriminate half-height letter prefixes (`m`, `b`, `o`, `d`, `x`) from full-height letter digits (`0`-`9`, `A`-`F`).

### operator

Currently, only four binary operators are implemented: `+`, `-`, `*`, `/` (listed from low precedence to high precedence).
Support for more diverse, multi-character **binary** operators is in plan. Suggestion or request is always welcome!

_Technical note: `hc` uses [reverse polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation) to evaluate expression. supporting unary (or even ternary) operator is a bit tricky._

### parenthesis

Use round brackets `(`, `)` as in normal arithmetic to override operator precedence.

### comment and result radix

```regex
@(b|o|d|x)?(\s.*)?$
```
`@` plays a double role in `hc` expression. It starts comment and may indicate in which radix to print the result.
e.g. `

##### comment

```regex
@(\s.*)?$
```
`@` comment acts similar to `#` comment of shell script or `//` comment of C language. But remember to put more than one whitespace before comment text begins. A non-space character right after `@` holds [special semantic](#####result-radix).

##### result radix

```regex
@(b|o|d|x)?
```
Put one of the radix indicator (`b`, `o`, `d`, `x`) right after `@` character.


## How to use

`hc` supports 2 modes. One-Shot and Interactive.

### One-Shot

```sh
$ hc 274 + x4086A32C @x
x4086A43E
```
Pass the expression to evaluate directly through command line argument to `hc`.
The result of the evaluation will be put to `stdout`.
You rarely need to quote the expression since the [syntax](#syntax) is designed to avoid semantics of shell script language.

This is useful when embedding `hc` result into other shell scripts, or calling from vim command mode:

- `tail -c +$(hc x8A76+43) dump.bin`: print file content after 35489 bytes.
- `:r! hc x8A76+43` on vim's command mode: put 35489 in place of the cursor.

### Interactive

```sh
$ hc -i
> 274 + x4086A32C @x this is comment
x4086A43E
> x4086A43E - x4086A32C @o
o422
> o422
274
> b1010 + o75 @ comments are ignored
71
> ^D
```
Run `hc` with `-i` option only.
The REPL session awaits you.
Quit by pressing `Ctrl-D`


## TODO list
- new line in an expression is completely ignored. let a newline (or a colon) separate expression and enable evaluating multiple expressions in one go. to be used like `seq $(hc xF7:xF7+8)`. can't use semicolon since it has semantic in shell script.
- negative result handling in radix other than decimal
- ignore empty expr in interactive mode
- current implementation of token is too big, need to demerge it into smaller pieces.
