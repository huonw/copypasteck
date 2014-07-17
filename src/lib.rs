#![feature(plugin_registrar, phase, globs)]

#[phase(plugin, link)] extern crate rustc;
extern crate syntax;

use std::cmp;
use std::gc::Gc;
use std::collections::HashMap;

use syntax::ast;
use syntax::codemap::{BytePos, Span};
use syntax::print::pprust;

use rustc::lint;
use rustc::lint::{Context, LintPass, LintArray};
use rustc::plugin::Registry;
use rustc::util::nodemap::NodeSet;

mod hasher;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_lint_pass(box CopyPaste { checked: NodeSet::new() });
}


declare_lint! {
    COPY_PASTE, Warn, "code chunks that may have been copy-pasted without being updated"
}
struct CopyPaste {
    checked: NodeSet,
}

impl LintPass for CopyPaste {
    fn get_lints(&self) -> LintArray {
        lint_array!(COPY_PASTE)
    }

    fn check_expr(&mut self, cx: &Context, e: &ast::Expr) {
        match e.node {
            ast::ExprIf(ref cond, ref block, trailing @ Some(_)) => {
                if !self.checked.contains(&e.id) {
                    check_if(cx,
                             &mut self.checked,
                             *cond, *block, trailing)
                }
            }
            ast::ExprMatch(_, ref arms) => check_match(cx, arms.as_slice()),
            _ => {}
        }
    }
}

type ExprMap = HashMap<u64, Vec<(Span, String)>>;

trait HashStringSpan {
    fn hash(&self) -> u64;
    fn string(&self) -> String;
    fn span(&self) -> Span;
}
impl HashStringSpan for ast::Block {
    fn hash(&self) -> u64 { hasher::hash_block(self) }
    fn string(&self) -> String { pprust::block_to_string(self) }
    fn span(&self) -> Span { self.span }
}
impl HashStringSpan for ast::Expr {
    fn hash(&self) -> u64 { hasher::hash_expr(self) }
    fn string(&self) -> String { pprust::expr_to_string(self) }
    fn span(&self) -> Span { self.span }
}
impl<'a> HashStringSpan for (&'a [Gc<ast::Pat>], Option<Gc<ast::Expr>>) {
    fn hash(&self) -> u64 {
        use std::hash::sip;
        use syntax::visit;
        let mut state = sip::SipState::new();
        {
            let mut visit = hasher::make(&mut state);
            for p in self.ref0().iter() {
                visit::walk_pat(&mut visit, *p, ())
            }
            match *self.ref1() {
                Some(e) => visit::walk_expr(&mut visit, e, ()),
                None => {}
            }
        }
        state.result()
    }
    fn span(&self) -> Span {
        let (low, high) = self.ref0().iter()
            .map(|p| { let BytePos(x) = p.span.lo; x })
            .min_max()
            .into_option()
            .expect("need at least one pattern");
        let high = match *self.ref1() {
            Some(e) => {
                let BytePos(x) = e.span.hi;
                cmp::max(high, x)
            }
            None => high
        };
        Span {
            lo: BytePos(low),
            hi: BytePos(high),
            expn_info: self.ref0()[0].span.expn_info
        }
    }
    fn string(&self) -> String {
        let mut s = self.ref0()
            .iter()
            .map(|p| pprust::pat_to_string(*p)).collect::<Vec<String>>().connect(" | ");
        match *self.ref1() {
            Some(e) => {
                s.push_str(" if ");
                s.push_str(e.string().as_slice())
            }
            None => {}
        }
        s
    }
}

fn check_and_insert<T: HashStringSpan>(cx: &Context, map: &mut ExprMap, val: &T,
                                       this_name: &str, other_name: &str) {
    // double hashing... whatever.
    let v = map.find_or_insert(val.hash(), vec![]);
    let s = val.string();
    for &(other_sp, ref other_s) in v.iter() {
        if *other_s == s && cx.current_level(COPY_PASTE) > lint::Allow {
            cx.span_lint(COPY_PASTE, val.span(),
                         format!("contents of {this} identical to previous {other}; was there a \
                                  copy-paste error?",
                                 this = this_name, other = other_name).as_slice());
            cx.sess().span_note(other_sp,
                                format!("previous {} here", other_name).as_slice());
        }
    }
    v.push((val.span(), s));
}


fn check_if(cx: &Context,
            checked: &mut NodeSet,
            mut cond: Gc<ast::Expr>,
            mut body: Gc<ast::Block>,
            mut next: Option<Gc<ast::Expr>>) {
    let mut conds = HashMap::new();
    let mut bodies = HashMap::new();
    loop {
        check_and_insert(cx, &mut conds, cond, "`if` condition", "condition");
        check_and_insert(cx, &mut bodies, body, "`if` branch", "branch");

        match next {
            // if ... { ... } else { ... }
            Some(e) => match e.node {
                ast::ExprBlock(body) => {
                    check_and_insert(cx, &mut bodies, body, "`else` branch", "branch");
                }
                ast::ExprIf(ref next_cond, ref next_body, ref next_next) => {
                    checked.insert(e.id);
                    cond = *next_cond;
                    body = *next_body;
                    next = *next_next;
                    continue
                }
                _ => unreachable!()
            },
            // if ... { ... } /* no else */
            None => {}
        }
        return
    }
}

fn check_match(cx: &Context,
               arms: &[ast::Arm]) {
    let mut conds = HashMap::new();
    let mut bodies = HashMap::new();
    for arm in arms.iter() {
        check_and_insert(cx, &mut conds, &(arm.pats.as_slice(), arm.guard),
                         "`match` pattern", "pattern");
        check_and_insert(cx, &mut bodies, arm.body, "`match` arm", "arm");
    }
}
