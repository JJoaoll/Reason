import Reason.Grammar.Expr
import Reason.Grammar.Typε

open Expr Typε

namespace Sttm

mutual
structure Block where
  body : Array Sttm
deriving Repr, BEq

inductive Sttm
  | SConst (type : Typε) (name : String) (expr : Expr)
  | SInit (type : Typε) (name : String) (expr : Expr)
  | SDecl (type : Typε) (name : String)
  | SAtrib (name : String) (expr : Expr)
  | SFunCall (name : String) (args : Array Expr)
  | SBlock (b : Block)
  | SFor (init : Sttm) (cond : Expr) (iter : Sttm) (block : Block)
  | SWhile (cond : Expr) (block : Block)
  | SIf (cond : Expr) (thεn : Block) (elsε : Option Block)
  | SReturn (expr : Expr)
  -- | SMatch
deriving Repr, BEq
end


-- Adicione suas definições de Statement aqui

end Sttm
