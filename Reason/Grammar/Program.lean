import Lean
-- open Lean
#print Lean.Expr

inductive Lit
  | LInt   (a : Int)
  | LChar  (c : Char)
  | LFLoat (x : Float)
deriving Repr, BEq, Inhabited

structure Typε where
  name : String
  args : List Typε
deriving Repr, BEq, Inhabited

inductive Expr
  | lit (lit : Lit)
  | var (name : String)
  | app (func arg : Expr)
  | lam (name : String) (t : Typε) (body : Expr) -- no binder info for while
deriving Repr, BEq, Inhabited

structure ConstrDef where
  name   : String
  params : List Typε
deriving Repr, BEq, Inhabited

structure TypεDef where
  name   : String
  params : List String
  cnstrs : List ConstrDef

structure Term where
  name : String
  typε : Typε
  val  : Expr
deriving Repr, BEq, Inhabited

structure Program where
  entryPoint : Term -- main
  terms : List Term
  types : List Typε
deriving Repr, BEq, Inhabited
