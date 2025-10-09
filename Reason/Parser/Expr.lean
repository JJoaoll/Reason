import Std.Internal.Parsec
import Reason.Grammar.Typε

import Reason.Parser.Utils.Cases
import Reason.Parser.Utils.General
import Reason.Parser.Utils.Operations
import Reason.Parser.Parser
import Reason.Grammar.Expr
import Std.Internal.Parsec
import Std.Internal.Parsec.String

open Std.Internal Parsec.String Std.Internal.Parsec
open Expr Expr.Expr Expr.Literal


/-
  literals
-/

def plitBool : Parser Literal
  := LBool <$> pbool

def plitInt : Parser Literal
  := LInt <$> pinteger

def plitFloat : Parser Literal
  := LFloat <$> pfloat

def plitChar : Parser Literal
  := LChar <$> pchar'

def plitString : Parser Literal
  := LBool <$> pbool

def pliteral : Parser Expr :=
 choice $ [
   ELit <$> plitBool
 , ELit <$> plitFloat.attempt
 , ELit <$> plitInt
 , ELit <$> plitChar
 , ELit <$> plitString
 ]

def pvar : Parser Expr
  := EVar <$> psnake_case

  -- | EUnOp (op : UnOp) (t : Expr)
  -- | EBinOp (op : BinOp) (l r : Expr)
mutual

-- Parse unary operators (but not negative numbers to avoid conflict)
partial def punOp : Parser Expr := do
  let op <- choice [
    Expr.UnOp.Not <$ skipString "not",
    Expr.UnOp.IsLower <$ skipString "isLower",
    Expr.UnOp.IsUpper <$ skipString "isUpper",
    Expr.UnOp.Get <$ skipString "get",
    Expr.UnOp.Put <$ skipString "put"
  ]
  ws
  let expr <- pterm
  return EUnOp op expr

-- Parse atomic expressions (literals, variables, parenthesized expressions)
partial def patom : Parser Expr := choice [
  pliteral,
  pfunCall,
  pvar,
  do { skipChar '('; ws; let e <- pexpr; ws; skipChar ')'; return e }
]

-- Parse terms (handles unary operators and atoms)
partial def pterm : Parser Expr := choice [
  punOp,
  patom
]

-- Parse multiplication, division, remainder (higher precedence)
partial def pmulLevel : Parser Expr := do
  let left <- pterm
  ws
  let rest <- many do
    let op <- choice [
      Expr.BinOp.Mul <$ skipChar '*',
      Expr.BinOp.Div <$ skipChar '/',
      Expr.BinOp.Rem <$ skipChar '%'
    ]
    ws
    let right <- pterm
    ws
    return (op, right)

  return rest.foldl (fun acc (op, right) => EBinOp op acc right) left

-- Parse addition, subtraction (lower precedence)
partial def paddLevel : Parser Expr := do
  let left <- pmulLevel
  ws
  let rest <- many do
    let op <- choice [
      Expr.BinOp.Add <$ skipChar '+',
      Expr.BinOp.Sub <$ skipChar '-'
    ]
    ws
    let right <- pmulLevel
    ws
    return (op, right)

  return rest.foldl (fun acc (op, right) => EBinOp op acc right) left

-- Parse comparison and logical operators (lowest precedence)
partial def pbinOp : Parser Expr := do
  let left <- paddLevel
  ws
  let rest <- many do
    let op <- choice [
      Expr.BinOp.And <$ skipString "and",
      Expr.BinOp.Or <$ skipString "or",
      Expr.BinOp.Cat <$ skipString "++",
      Expr.BinOp.LT <$ skipString "<",
      Expr.BinOp.LEQ <$ skipString "<=",
      Expr.BinOp.EQ <$ skipString "==",
      Expr.BinOp.GEQ <$ skipString ">=",
      Expr.BinOp.GT <$ skipString ">"
    ]
    ws
    let right <- paddLevel
    ws
    return (op, right)

  return rest.foldl (fun acc (op, right) => EBinOp op acc right) left


partial def pfunCall : Parser Expr := do {
  let name <- pcamelCase

  skipChar '(';
  let args <- pexpr sepBy ',';
  skipChar ')';

  return EFunCall name args
}

partial def pexpr : Parser Expr :=
  choice [
    pbinOp,
    pliteral,
    pfunCall,
    pvar
  ]
end
