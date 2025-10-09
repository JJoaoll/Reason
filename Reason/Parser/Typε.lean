import Reason.Grammar.Typε

import Reason.Parser.Utils.General
import Reason.Parser.Parser
import Std.Internal.Parsec
import Std.Internal.Parsec.String

open Std.Internal Parsec.String Std.Internal.Parsec
open Typε.Typε Typε

def pTypε : Parser Typε :=
  choice [
    TInt    <$ pstring "Int"
  , TFloat  <$ pstring "Float"
  , TChar   <$ pstring "Char"
  , TString <$ pstring "String"
  , TBool   <$ pstring "Bool"
  ]

#eval pTypε.run "Int"
#eval pTypε.run "Bool"
#eval pTypε.run "Float"
