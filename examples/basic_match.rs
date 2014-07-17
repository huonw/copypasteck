#![feature(phase)]

#[phase(plugin)] extern crate copypasteck;

fn main() {
    match 10i {
        1 => { () }
        2 => { () }
        _ if true => { 1i; }
        _ if true => { 1i; }
        _ => { {} }
    }
}
