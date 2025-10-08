
namespace Expr

inductive Literal
  | LInt (n : Int)
  | LFloat (x : Float)
  | LChar (c : Char)
  | LBool (b : Bool)
  | LString (s : String)
  -- | LVector ()
  -- | Deriv
deriving Repr, BEq

inductive UnOp
  | Not -- not True
  | Neg -- -3

  | IsLower
  | IsUpper

  | Get -- array
  | Put -- array
  -- |
deriving Repr, BEq

inductive BinOp
  | Add | Mul | Div | Rem
  | And | Or

  | Cat

  | LT | LEQ | EQ | GEQ | GT
  -- | NEQ --> parser guy saied he'll take care
deriving Repr, BEq

inductive Expr
  | ELit (lit : Literal)
  | EVar (name : String)
  | EUnOp (op : UnOp) (t : Expr)
  | EBinOp (op : BinOp) (l r : Expr)
  | EFunCall (funName : String) (Args : Array Expr)
  -- | ELam (binder : String) (type : Typε) (body : Block)
deriving Repr, BEq

end Expr
