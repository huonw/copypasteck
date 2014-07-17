#![feature(phase)]

#[phase(plugin)] extern crate copypasteck;

fn main() {
    let a = 16i;
    let b = if a > 15 {
        2u
    } else if a > 10 {
        3
    } else if a > 10 {
        1
    } else {
        1
    };
}
