import Reason.Grammar.Typε

import Reason.Parser.Parser
import Std.Internal.Parsec
import Std.Internal.Parsec.String

open Std.Internal Parsec.String Std.Internal.Parsec
open Typε.Typε Typε

def pputAfter (val : α) (p : Parser β) : Parser α := do
  let _ <- p
  return val

infixl:30 "<$" => pputAfter

-- p.attempt?
def choice : List (Parser α) -> Parser α
  | [] => failure
  | p::ps => p <|> choice ps

def lexeme (p : Parser α) : Parser α := do {
  let x <- p
  ws
  return x
}
-- symbol
-- sc
def separatedBy (p : Parser α) (c : Char) : Parser $ Array α := attempt do {
  let x <- p
  let xs <- many $ attempt do
    lexeme $ skipChar c
    lexeme p

  -- let xs <- many $ lexeme p <* lexeme (skipChar c)
  -- let xs <- many $ lexeme p <* lexeme (skipChar c)

  return xs.insertIdx 0 x
} <|> pure #[]

infixl:30 "sepBy" => separatedBy
-- notation p " sepBy " c => separatedBy p c

#eval #[3,1,2].insertIdx 0 44
#eval #[].set! 0 44
#eval #[].insertIdx 0 4


-- notation
-- between
-- ...

--#endregion -- <<< AQUI: Fim da Regra 1


--#region Parser Definitions

def plexeme : Parser α → Parser α  := sorry
def psymbol : Parser α → Parser α  := sorry
def pkeyword : Parser α → Parser α  := sorry

/-
  @literals@
-/

def pinteger : Parser Int := (digits : Parser Int) <|> do {
  skipChar '-'
  let num : Int <- digits

  return -num
}

open OfScientific in
def pfloat : Parser Float := do {
  let num <- pinteger
  skipChar '.'

  let num' <- optional digits
  let num' := num'.getD 0

  let n' :=
    num'
    |> Nat.toDigits 10
    |> List.length

  let together := s!"{num.natAbs}{num'}" |> String.toNat!
  let res := ofScientific together True n'

  return match num.sign with
    | -1 => -res
    | _  => res
}

def pchar' : Parser Char := do {
  skipChar '\''

  let c <- satisfy (· != '\'')

  skipChar '\''

  return c
}

#eval pchar'.run "\'a\'"
#eval pchar'.run "\'-\'"
#eval pchar'.run "\'3\'"
#eval pchar'.run "\'_\'"
#eval pchar'.run "\'_\'"
#eval pchar'.run "\'y\'"


def pstring' : Parser String := do {
  skipChar '\"'
  let str <- many $ satisfy (· != '\"')
  skipChar '\"'

  return { data := str.toList }
}

def pbool : Parser Bool :=
  choice [
    True  <$ skipString "true"
  , False <$ skipString "false"
  ]

#eval pbool.run "True"
#eval pbool.run "False"

#eval pstring'.run "\"\""
#eval pstring'.run "\" a  aa\""

#eval 3.
#eval toString 4
#eval s!"{4}"
#eval pfloat.run "a"
#eval pfloat.run "a"
#eval pfloat.run "a"
#eval pfloat.run "3.1415"
#eval pfloat.run "3."
#eval pfloat.run "33.000"
#eval pfloat.run "33.00"
#eval pfloat.run "33.0"

#eval (16).toDigits 01235123
#eval pinteger.run "- 3"
#eval pinteger.run "-3"
#eval pinteger.run "42"
#eval pinteger.run "0"


#eval (pinteger sepBy ',').run "1,2,3,4 ,123 ,12 1 ,12 12 12 12"
#eval (pinteger sepBy '/').run "333/1/23213/"
#eval (pinteger sepBy '/').run "/333/1/23213/"
