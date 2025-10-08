import Reason.Grammar.Expr
import Reason.Grammar.Typε
import Reason.Grammar.Sttm

open Expr Typε Sttm

/-
Int sum(Int x, Int y, Int z) {


}
-/
structure Fun where
  rtrn_type : Typε
  name      : String
  params    : Array (Typε × String)
  block     : Block
deriving Repr, BEq

-- inductive TypeDef
--   | strucutre
--   | variant



-- structure Program where
--   funs : Array Fun


--   -- types : List Typε
-- deriving Repr, BEq, Inhabited
