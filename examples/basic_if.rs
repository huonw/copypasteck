#![feature(plugin)]

#[plugin] #[no_link] extern crate copypasteck;

fn main() {
    let a = 16;
    let _b = if a > 15 {
        2
    } else if a > 10 {
        3
    } else if a > 10 {
        1
    } else {
        1
    };
}
