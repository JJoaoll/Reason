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

mutual -- some AI and some Not

-- Parse unary operators (but not negative numbers to avoid conflict)
partial def punOp : Parser Expr := do
  let op <- choice [
    .Not     <$ skipString "not",
    .IsLower <$ skipString "isLower",
    .IsUpper <$ skipString "isUpper",
    -- .Get     <$ skipString "get",
    -- .Put     <$ skipString "put"
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
      .Mul <$ skipChar '*',
      .Div <$ skipChar '/',
      .Rem <$ skipChar '%'
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
      .Add <$ skipChar '+',
      .Sub <$ skipChar '-'
    ]
    ws
    let right <- pmulLevel
    ws
    return (op, right)

  return rest.foldl (fun acc (op, right) => EBinOp op acc right) left

-- Parse comparison and logical operators (lowest precedence)
-- TODO: missing NEQ
partial def pop : Parser Expr := do
  let left <- paddLevel
  ws
  let rest <- many do
    let op <- choice [
      .And <$ skipString "and",
      .Or  <$ skipString "or",
      .Cat <$ skipString "++",
      .LT  <$ skipString "<",
      .LEQ <$ skipString "<=",
      .EQ  <$ skipString "==",
      .GEQ <$ skipString ">=",
      .GT  <$ skipString ">"
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
    pop,
    pliteral,
    pfunCall,
    pvar
  ]
end

/-
  tests
-/

#eval pexpr.run "a_b_ada"
#eval pexpr.run "faaaa ()"
#eval pexpr.run "f(x, 3 + y) * 3 + 1"
#eval pexpr.run "false"
#eval pexpr.run "true and false"
