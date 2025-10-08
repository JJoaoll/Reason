import Reason.Grammar.Typε

import Reason.Parser.Parser
import Std.Internal.Parsec
import Std.Internal.Parsec.String

open Std.Internal Parsec.String Std.Internal.Parsec
open Typε.Typε Typε

def pTypε : Parser Typε :=
  TInt <$ pstring "Int"
  -- <|> pstring "Float" <|>
  -- pstring "Char" <|> pstring "String" <|>
  -- pstring "Bool"


#eval pTypε "Int"
