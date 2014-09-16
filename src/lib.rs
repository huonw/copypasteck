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
    reg.register_lint_pass(box CopyPaste { checked_ifs: NodeSet::new() });
}


declare_lint! {
    COPY_PASTE, Warn, "code chunks that may have been copy-pasted without being updated"
}
struct CopyPaste {
    // keep track of the `if`s we've seen, so we don't overwarn on the
    // non-first `if`s in `if ... else if ... else if ...` by running
    // the condition scanner on them more than once (that is, this
    // ensures that we only scan conditions from the first `if` in a
    // chain).
    checked_ifs: NodeSet,
}

impl LintPass for CopyPaste {
    fn get_lints(&self) -> LintArray {
        lint_array!(COPY_PASTE)
    }

    fn check_expr(&mut self, cx: &Context, e: &ast::Expr) {
        match e.node {
            ast::ExprIf(ref cond, ref block, trailing @ Some(_)) => {
                if !self.checked_ifs.contains(&e.id) {
                    check_if(cx,
                             &mut self.checked_ifs,
                             *cond, *block, trailing)
                }
            }
            ast::ExprMatch(_, ref arms) => check_match(cx, arms.as_slice()),
            _ => {}
        }
    }
}

type Map<T> = HashMap<u64, Vec<(Option<String>, T)>>;

/// Check if `val` has been seen before (warn if it has) and add it to
/// the map of such things.
///
/// Basic approach:
///
/// - hash `val` to get a fast but non-precise differentiator. Almost
///   things with different AST will have different hashes, and thus
///   don't need closer comparison.
/// - scan through anything with the same hash, comparing (lazily
///   computed) stringifications, to check equality perfectly (this
///   only compares source-level things, i.e. ignores resolution and
///   hygiene, but this is essentially what we're interested in, since
///   the errors will be source-level copy-pastes).
/// - emit a lint warning if any previous thing is identical (with a
///   note pointing to the previous match)
fn check_and_insert<T: SourceObject>(cx: &Context, seen: &mut Map<T>, val: T,
                                     this_name: &str, other_name: &str) {
    // double hashing... whatever: it's better than stringification
    // and linear search (probably... I haven't measured).
    let v = seen.find_or_insert(val.ast_hash(), vec![]);

    // lazily render to string, to save the stringification effort in
    // the common case of differing objects and hashes.
    let s = if v.is_empty() { None } else { Some(val.string()) };

    for &(ref mut other_s, ref other_val) in v.mut_iter() {
        // the other thing doesn't have its string representation
        // computed, so do it now.
        if other_s.is_none() {
            *other_s = Some(other_val.string())
        }

        if *other_s == s && cx.current_level(COPY_PASTE) > lint::Allow {
            cx.span_lint(COPY_PASTE, val.span(),
                         format!("contents of {this} identical to previous {other}; was there a \
                                  copy-paste error?",
                                 this = this_name, other = other_name).as_slice());
            cx.sess().span_note(other_val.span(),
                                format!("previous {} here", other_name).as_slice());
        }
    }
    v.push((s, val));
}

fn check_if(cx: &Context,
            checked: &mut NodeSet,
            mut cond: Gc<ast::Expr>,
            mut body: Gc<ast::Block>,
            mut next: Option<Gc<ast::Expr>>) {
    let mut conds = HashMap::new();
    let mut bodies = HashMap::new();

    // walk along the `if`-`else` chain, checking the conditions and
    // bodies against those we've seen already.
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
                    // register that we're looking at this thing, so
                    // it's not checked again, later.
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
        check_and_insert(cx, &mut conds, (arm.pats.as_slice(), arm.guard),
                         "`match` pattern", "pattern");
        check_and_insert(cx, &mut bodies, arm.body, "`match` arm", "arm");
    }
}



trait SourceObject {
    // get a uniquish number representing the AST of this thing; it
    // doesn't guarantee equality, but a difference guarantees
    // inequality.
    fn ast_hash(&self) -> u64;
    fn string(&self) -> String;
    fn span(&self) -> Span;
}

impl SourceObject for Gc<ast::Block> {
    fn ast_hash(&self) -> u64 { hasher::hash_block(&**self) }
    fn string(&self) -> String { pprust::block_to_string(&**self) }
    fn span(&self) -> Span { self.span }
}
impl SourceObject for Gc<ast::Expr> {
    fn ast_hash(&self) -> u64 { hasher::hash_expr(&**self) }
    fn string(&self) -> String { pprust::expr_to_string(&**self) }
    fn span(&self) -> Span { self.span }
}

// the 'condition' part of a `match` arm: `<pat> | <pat> | <pat> [if <expr>]`
impl<'a> SourceObject for (&'a [Gc<ast::Pat>], Option<Gc<ast::Expr>>) {
    fn ast_hash(&self) -> u64 {
        use std::hash::sip;
        use syntax::visit;

        // hash everything together into one pot.
        let mut state = sip::SipState::new();
        {
            let mut visit = hasher::make(&mut state);
            for p in self.ref0().iter() {
                visit::walk_pat(&mut visit, &**p)
            }
            match *self.ref1() {
                Some(e) => visit::walk_expr(&mut visit, &*e),
                None => {}
            }
        }
        state.result()
    }
    fn span(&self) -> Span {
        // get the smallest/largest byte position of the components of
        // the pattern/guard, this will work perfectly with a basic
        //
        //     Foo | Bar if baz
        //
        // but will likely fail horribly with patterns/guards
        // substituted in via macros (especially if the macro if from
        // a different file... :( ).
        let (low, high) = self.ref0().iter()
            .map(|p| { let BytePos(x) = p.span.lo; x })
            .min_max()
            .into_option()
            .expect("need at least one pattern");
        let (low, high) = match *self.ref1() {
            Some(e) => {
                let Span { lo: BytePos(x), hi: BytePos(y), .. } = e.span;
                (cmp::min(x, low), cmp::max(y, high))
            }
            None => (low, high)
        };

        Span {
            lo: BytePos(low),
            hi: BytePos(high),
            // again, probably not correct with macro expansion
            // (different patterns could come from different places),
            // but it's easy and works for the common case.
            expn_info: self.ref0()[0].span.expn_info
        }
    }
    fn string(&self) -> String {
        // Put all the patterns together with a `|`...
        let mut s = self.ref0()
            .iter()
            .map(|p| pprust::pat_to_string(&**p)).collect::<Vec<String>>().connect(" | ");

        // ...and add the guard, if it exists.
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
