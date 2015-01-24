#![feature(plugin)]

#[plugin] #[no_link] extern crate copypasteck;

fn main() {
    match 10 {
        1 => { () }
        2 => { () }
        _ if true => { 1; }
        _ if true => { 1; }
        _ => { {} }
    }
}
