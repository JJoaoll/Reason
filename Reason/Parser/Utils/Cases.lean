import Reason.Grammar.Typε
import Reason.Parser.Utils.General

import Reason.Parser.Parser
import Std.Internal.Parsec
import Std.Internal.Parsec.String

open Std.Internal Parsec.String Std.Internal.Parsec
open Typε.Typε Typε

/- Counts the numbers of lines before certain Pos -/
def countNewlinesUntil (s : String) (pos : String.Pos) : Nat :=
  let substr := s.extract ⟨0⟩ pos
  substr.foldl (fun count char => if char == '\n' then count + 1 else count) 0

def psnake_case : Parser String := do {
  let c <- satisfy Char.isLower
  let cs <- many
    $ digit
   <|> satisfy (fun c => c.isLower || c == '_')


  return { data := c::cs.toList }
}

#eval psnake_case.run "abcAFWQEDQWD_"
#eval psnake_case.run "sum_atory:AFWQEDQWD_"
#eval psnake_case.run "s34123m_atory:AFWQEDQWD_"

def pcamelCase : Parser String := do {
  let c <- satisfy Char.isLower
  let cs <- many
    $ digit <|> asciiLetter

  return { data := c::cs.toList }
}

#eval pcamelCase.run "abcAFWQEDQWD_"
#eval pcamelCase.run "sum_atory:AFWQEDQWD_"
#eval pcamelCase.run "s34123m_atory:AFWQEDQWD_"


def pPascalCase : Parser String := sorry


#eval
  let txt :=
"
asdas

sdasd
Sdasdsa
o

ERROR

asdasiodjas

"
countNewlinesUntil txt (txt.posOf 'R')
  -- countNewlinesUntil
