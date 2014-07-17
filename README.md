# copypasteck

[![Build Status](https://travis-ci.org/huonw/copypasteck.png)](https://travis-ci.org/huonw/copypasteck)


A basic `rustc` lint plugin for checking for copy-paste
duplication. This will warn about `if` and `match` branches with
duplicated conditions or contents.

## Example

```toml
# Cargo.toml
[package]
name = "example"
version = "0.1.0"
authors = ["You <youremail@example.com>"]

[dependencies.copypasteck]
git = "https://github.com/huonw/copypasteck"
```

```rust
// src/main.rs
#![feature(phase)]

#[phase(plugin)] extern crate copypasteck;

fn main() {
    let a = 10i;
    if a > 5 {
        println!("hi");
    } else if a > 5 {
        println!("bye");
    }
}
```

```
$ cargo build
    Updating git repository `https://github.com/huonw/copypasteck`
   Compiling copypasteck v0.1.0 (https://github.com/huonw/copypasteck)
   Compiling example v0.1.0 (file:...)
.../src/main.rs:9:15: 9:20 warning: contents of `if` condition identical to previous condition; was there a copy-paste error?, #[warn(copy_paste)] on by default
.../src/main.rs:9     } else if a > 5 {
                                ^~~~~
.../src/main.rs:7:8: 7:13 note: previous condition here
.../src/main.rs:7     if a > 5 {
                         ^~~~~
```

## License

This is distributed under the same terms as Rust itself, dual MIT and
Apache. See `LICENSE-*`.
