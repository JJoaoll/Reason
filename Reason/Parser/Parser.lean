import Std.Internal.Parsec
import Std.Internal.Parsec.String

open Std.Internal Parsec.String Std.Internal.Parsec

#print String.Pos
#print String.Iterator.next
#print String.next

@[default_instance]
instance : Input String.Iterator Char String.Pos where
  pos it     := it.pos
  next it    := it.next
  curr it    := it.curr
  hasNext it := it.hasNext
  next' it   := it.next'
  curr' it   := it.curr'

@[default_instance]
instance : Coe String String.Iterator where
  coe str := { s := str, i := 0 }

#check Parsec
#eval digits.attempt (String.Iterator.mk "3332123" 0)
#eval digits.run "3332123"
#eval digits.attempt (String.Iterator.mk "asbasd" 0)
#eval digits.run "aabasd" <|> digits.run "33"
#check digits
#check (satisfy (fun _c : Char => true)).attempt ("" : String.Iterator)

#eval (Std.Internal.Parsec.satisfy (Char.isDigit) : Parsec String.Iterator Char) "abc"
#eval (Std.Internal.Parsec.satisfy (Char.isAlpha) : Parsec String.Iterator Char) "abc"
#eval (Std.Internal.Parsec.satisfy (Char.isAlpha) : Parsec String.Iterator Char).attempt "abc"

#check (Std.Internal.Parsec.satisfy (fun _ => true) : Parsec String.Iterator Char)

-- #check Lean.Parser.many
-- #eval Lean.Parser.ident.info
/-

lets learn this shit


-/
