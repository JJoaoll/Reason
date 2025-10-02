
inductive Expr where
  | lit ()
  | var (name : String)
deriving Repr, BEq, Inhabited

structure Typε where
  name : String
  args : List Typε
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
deriving Repr, DecidableEq, Inhabited
