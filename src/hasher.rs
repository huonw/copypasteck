// taken wholesale from rustc_back::svh

use syntax::ast;
use syntax::ast::*;
use syntax::codemap::Span;
use syntax::parse::token;
use syntax::print::pprust;
use syntax::visit;
use syntax::visit::{Visitor, FnKind};

use std::hash::Hash;
use std::hash::sip::SipState;

use self::SawAbiComponent::*;
use self::SawExprComponent::*;
use self::SawStmtComponent::*;

pub fn hash_expr(e: &ast::Expr) -> u64 {
    // FIXME: this should use SHA1, not SipHash. SipHash is not built to
    //        avoid collisions.
    let mut state = SipState::new();
    {
        let mut visit = make(&mut state);
        visit::walk_expr(&mut visit, e);
    }
    state.result()
}
pub fn hash_block(b: &ast::Block) -> u64 {
    // FIXME: this should use SHA1, not SipHash. SipHash is not built to
    //        avoid collisions.
    let mut state = SipState::new();
    {
        let mut visit = make(&mut state);
        visit::walk_block(&mut visit, b);
    }
    state.result()
}

pub struct StrictVersionHashVisitor<'a> {
    pub st: &'a mut SipState,
}

pub fn make<'a>(st: &'a mut SipState) -> StrictVersionHashVisitor<'a> {
    StrictVersionHashVisitor { st: st }
}

// To off-load the bulk of the hash-computation on deriving(Hash),
// we define a set of enums corresponding to the content that our
// crate visitor will encounter as it traverses the ast.
//
// The important invariant is that all of the Saw*Component enums
// do not carry any Spans, Names, or Idents.
//
// Not carrying any Names/Idents is the important fix for problem
// noted on PR #13948: using the ident.name as the basis for a
// hash leads to unstable SVH, because ident.name is just an index
// into intern table (i.e. essentially a random address), not
// computed from the name content.
//
// With the below enums, the SVH computation is not sensitive to
// artifacts of how rustc was invoked nor of how the source code
// was laid out.  (Or at least it is *less* sensitive.)

// This enum represents the different potential bits of code the
// visitor could encounter that could affect the ABI for the crate,
// and assigns each a distinct tag to feed into the hash computation.
#[deriving(Hash)]
enum SawAbiComponent<'a> {

    // FIXME (#14132): should we include (some function of)
    // ident.ctxt as well?
    SawIdent(token::InternedString),
    SawStructDef(token::InternedString),

    SawLifetimeRef(token::InternedString),
    SawLifetimeDef(token::InternedString),

    SawMod,
    SawViewItem,
    SawForeignItem,
    SawItem,
    SawDecl,
    SawTy,
    SawGenerics,
    SawFn,
    SawTyMethod,
    SawTraitMethod,
    SawStructField,
    SawVariant,
    SawExplicitSelf,
    SawPath,
    SawOptLifetimeRef,
    SawBlock,
    SawPat,
    SawLocal,
    SawArm,
    SawExpr(SawExprComponent<'a>),
    SawStmt(SawStmtComponent),
}

/// SawExprComponent carries all of the information that we want
/// to include in the hash that *won't* be covered by the
/// subsequent recursive traversal of the expression's
/// substructure by the visitor.
///
/// We know every Expr_ variant is covered by a variant because
/// `fn saw_expr` maps each to some case below.  Ensuring that
/// each variant carries an appropriate payload has to be verified
/// by hand.
///
/// (However, getting that *exactly* right is not so important
/// because the SVH is just a developer convenience; there is no
/// guarantee of collision-freedom, hash collisions are just
/// (hopefully) unlikely.)
#[deriving(Hash)]
pub enum SawExprComponent<'a> {

    SawExprLoop(Option<token::InternedString>),
    SawExprField(token::InternedString),
    SawExprTupField(uint),
    SawExprBreak(Option<token::InternedString>),
    SawExprAgain(Option<token::InternedString>),

    SawExprBox,
    SawExprVec,
    SawExprCall,
    SawExprMethodCall,
    SawExprTup,
    SawExprBinary(ast::BinOp),
    SawExprUnary(ast::UnOp),
    SawExprLit(ast::Lit_),
    SawExprCast,
    SawExprIf,
    SawExprWhile,
    SawExprMatch,
    SawExprClosure,
    SawExprBlock,
    SawExprAssign,
    SawExprAssignOp(ast::BinOp),
    SawExprIndex,
    SawExprRange,
    SawExprPath,
    SawExprAddrOf(ast::Mutability),
    SawExprRet,
    SawExprInlineAsm(&'a ast::InlineAsm),
    SawExprStruct,
    SawExprRepeat,
    SawExprParen,
    SawExprForLoop,
}

fn saw_expr<'a>(node: &'a Expr_) -> SawExprComponent<'a> {
    match *node {
        ExprBox(..)              => SawExprBox,
        ExprVec(..)              => SawExprVec,
        ExprCall(..)             => SawExprCall,
        ExprMethodCall(..)       => SawExprMethodCall,
        ExprTup(..)              => SawExprTup,
        ExprBinary(op, _, _)     => SawExprBinary(op),
        ExprUnary(op, _)         => SawExprUnary(op),
        ExprLit(ref lit)         => SawExprLit(lit.node.clone()),
        ExprCast(..)             => SawExprCast,
        ExprIf(..)               => SawExprIf,
        ExprWhile(..)            => SawExprWhile,
        ExprLoop(_, id)          => SawExprLoop(id.map(content)),
        ExprMatch(..)            => SawExprMatch,
        ExprClosure(..)          => SawExprClosure,
        ExprBlock(..)            => SawExprBlock,
        ExprAssign(..)           => SawExprAssign,
        ExprAssignOp(op, _, _)   => SawExprAssignOp(op),
        ExprField(_, id)         => SawExprField(content(id.node)),
        ExprTupField(_, id)      => SawExprTupField(id.node),
        ExprIndex(..)            => SawExprIndex,
        ExprRange(..)            => SawExprRange,
        ExprPath(..)             => SawExprPath,
        ExprAddrOf(m, _)         => SawExprAddrOf(m),
        ExprBreak(id)            => SawExprBreak(id.map(content)),
        ExprAgain(id)            => SawExprAgain(id.map(content)),
        ExprRet(..)              => SawExprRet,
        ExprInlineAsm(ref asm)   => SawExprInlineAsm(asm),
        ExprStruct(..)           => SawExprStruct,
        ExprRepeat(..)           => SawExprRepeat,
        ExprParen(..)            => SawExprParen,
        ExprForLoop(..)          => SawExprForLoop,

        // just syntactic artifacts, expanded away by time of SVH.
        ExprIfLet(..)            => unreachable!(),
        ExprWhileLet(..)         => unreachable!(),
        ExprMac(..)              => unreachable!(),
    }
}

/// SawStmtComponent is analogous to SawExprComponent, but for statements.
#[deriving(Hash)]
pub enum SawStmtComponent {
    SawStmtDecl,
    SawStmtExpr,
    SawStmtSemi,
}

fn saw_stmt(node: &Stmt_) -> SawStmtComponent {
    match *node {
        StmtDecl(..) => SawStmtDecl,
        StmtExpr(..) => SawStmtExpr,
        StmtSemi(..) => SawStmtSemi,
        StmtMac(..)  => unreachable!(),
    }
}

// Ad-hoc overloading between Ident and Name to their intern table lookups.
trait InternKey { fn get_content(self) -> token::InternedString; }
impl InternKey for Ident {
    fn get_content(self) -> token::InternedString { token::get_ident(self) }
}
impl InternKey for Name {
    fn get_content(self) -> token::InternedString { token::get_name(self) }
}
fn content<K:InternKey>(k: K) -> token::InternedString { k.get_content() }

impl<'a, 'v> Visitor<'v> for StrictVersionHashVisitor<'a> {

    fn visit_mac(&mut self, macro: &Mac) {
        // macro invocations, namely macro_rules definitions,
        // *can* appear as items, even in the expanded crate AST.

        if macro_name(macro).get() == "macro_rules" {
            // Pretty-printing definition to a string strips out
            // surface artifacts (currently), such as the span
            // information, yielding a content-based hash.

            // FIXME (#14132): building temporary string is
            // expensive; a direct content-based hash on token
            // trees might be faster. Implementing this is far
            // easier in short term.
            let macro_defn_as_string =
                pprust::to_string(|pp_state| pp_state.print_mac(macro, token::Paren));
            macro_defn_as_string.hash(self.st);
        } else {
            // It is not possible to observe any kind of macro
            // invocation at this stage except `macro_rules!`.
            panic!("reached macro somehow: {}",
                  pprust::to_string(|pp_state| pp_state.print_mac(macro, token::Paren)));
        }

        visit::walk_mac(self, macro);

        fn macro_name(macro: &Mac) -> token::InternedString {
            match &macro.node {
                &MacInvocTT(ref path, ref _tts, ref _stx_ctxt) => {
                    let s = path.segments.as_slice();
                    assert_eq!(s.len(), 1);
                    content(s[0].identifier)
                }
            }
        }
    }

    fn visit_struct_def(&mut self, s: &StructDef, ident: Ident,
                        g: &Generics, _: NodeId) {
        SawStructDef(content(ident)).hash(self.st);
        visit::walk_generics(self, g);
        visit::walk_struct_def(self, s)
    }

    fn visit_variant(&mut self, v: &Variant, g: &Generics) {
        SawVariant.hash(self.st);
        // walk_variant does not call walk_generics, so do it here.
        visit::walk_generics(self, g);
        visit::walk_variant(self, v, g)
    }

    fn visit_opt_lifetime_ref(&mut self, _: Span, l: &Option<Lifetime>) {
        SawOptLifetimeRef.hash(self.st);
        // (This is a strange method in the visitor trait, in that
        // it does not expose a walk function to do the subroutine
        // calls.)
        match *l {
            Some(ref l) => self.visit_lifetime_ref(l),
            None => ()
        }
    }

    // All of the remaining methods just record (in the hash
    // SipState) that the visitor saw that particular variant
    // (with its payload), and continue walking as the default
    // visitor would.
    //
    // Some of the implementations have some notes as to how one
    // might try to make their SVH computation less discerning
    // (e.g. by incorporating reachability analysis).  But
    // currently all of their implementations are uniform and
    // uninteresting.
    //
    // (If you edit a method such that it deviates from the
    // pattern, please move that method up above this comment.)

    fn visit_ident(&mut self, _: Span, ident: Ident) {
        SawIdent(content(ident)).hash(self.st);
    }

    fn visit_lifetime_ref(&mut self, l: &Lifetime) {
        SawLifetimeRef(content(l.name)).hash(self.st);
    }

    fn visit_lifetime_def(&mut self, l: &LifetimeDef) {
        SawLifetimeDef(content(l.lifetime.name)).hash(self.st);
    }

    // We do recursively walk the bodies of functions/methods
    // (rather than omitting their bodies from the hash) since
    // monomorphization and cross-crate inlining generally implies
    // that a change to a crate body will require downstream
    // crates to be recompiled.
    fn visit_expr(&mut self, ex: &Expr) {
        SawExpr(saw_expr(&ex.node)).hash(self.st); visit::walk_expr(self, ex)
    }

    fn visit_stmt(&mut self, s: &Stmt) {
        SawStmt(saw_stmt(&s.node)).hash(self.st); visit::walk_stmt(self, s)
    }

    fn visit_view_item(&mut self, i: &ViewItem) {
        // Two kinds of view items can affect the ABI for a crate:
        // exported `pub use` view items (since that may expose
        // items that downstream crates can call), and `use
        // foo::Trait`, since changing that may affect method
        // resolution.
        //
        // The simplest approach to handling both of the above is
        // just to adopt the same simple-minded (fine-grained)
        // hash that I am deploying elsewhere here.
        SawViewItem.hash(self.st); visit::walk_view_item(self, i)
    }

    fn visit_foreign_item(&mut self, i: &ForeignItem) {
        // FIXME (#14132) ideally we would incorporate privacy (or
        // perhaps reachability) somewhere here, so foreign items
        // that do not leak into downstream crates would not be
        // part of the ABI.
        SawForeignItem.hash(self.st); visit::walk_foreign_item(self, i)
    }

    fn visit_item(&mut self, i: &Item) {
        // FIXME (#14132) ideally would incorporate reachability
        // analysis somewhere here, so items that never leak into
        // downstream crates (e.g. via monomorphisation or
        // inlining) would not be part of the ABI.
        SawItem.hash(self.st); visit::walk_item(self, i)
    }

    fn visit_mod(&mut self, m: &Mod, _s: Span, _n: NodeId) {
        SawMod.hash(self.st); visit::walk_mod(self, m)
    }

    fn visit_decl(&mut self, d: &Decl) {
        SawDecl.hash(self.st); visit::walk_decl(self, d)
    }

    fn visit_ty(&mut self, t: &Ty) {
        SawTy.hash(self.st); visit::walk_ty(self, t)
    }

    fn visit_generics(&mut self, g: &Generics) {
        SawGenerics.hash(self.st); visit::walk_generics(self, g)
    }

    fn visit_fn(&mut self, fk: FnKind<'v>, fd: &'v FnDecl,
                b: &'v Block, s: Span, _: NodeId) {
        SawFn.hash(self.st); visit::walk_fn(self, fk, fd, b, s)
    }

    fn visit_ty_method(&mut self, t: &TypeMethod) {
        SawTyMethod.hash(self.st); visit::walk_ty_method(self, t)
    }

    fn visit_trait_item(&mut self, t: &TraitItem) {
        SawTraitMethod.hash(self.st); visit::walk_trait_item(self, t)
    }

    fn visit_struct_field(&mut self, s: &StructField) {
        SawStructField.hash(self.st); visit::walk_struct_field(self, s)
    }

    fn visit_explicit_self(&mut self, es: &ExplicitSelf) {
        SawExplicitSelf.hash(self.st); visit::walk_explicit_self(self, es)
    }

    fn visit_path(&mut self, path: &Path, _: ast::NodeId) {
        SawPath.hash(self.st); visit::walk_path(self, path)
    }

    fn visit_block(&mut self, b: &Block) {
        SawBlock.hash(self.st); visit::walk_block(self, b)
    }

    fn visit_pat(&mut self, p: &Pat) {
        SawPat.hash(self.st); visit::walk_pat(self, p)
    }

    fn visit_local(&mut self, l: &Local) {
        SawLocal.hash(self.st); visit::walk_local(self, l)
    }

    fn visit_arm(&mut self, a: &Arm) {
        SawArm.hash(self.st); visit::walk_arm(self, a)
    }
}
