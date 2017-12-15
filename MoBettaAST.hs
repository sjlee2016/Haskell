module MoBettaAST where

-- Declare the abstract syntax of MoBetta
-- Shared between parser and interpreter
-- All types here derive Show for debugging.

type Program = [Statement]

data Statement
  = Assign String AExpr
  | Skip
  | Print AExpr
  | Msg String
  | Read String
  | If BExpr Statement Statement
  | While BExpr Statement
  | Block [Statement]
  deriving (Show)

data BExpr
  = Reln Comp AExpr AExpr
  | BoolConst Bool
  | BBin BinBOp BExpr BExpr
  | BUn UnBOp BExpr
  deriving (Show)

data BinBOp = And | Or deriving (Show,Eq)
data UnBOp = Not deriving (Show,Eq)

data Comp
  = Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Equal
  | NEqual
  deriving (Show,Eq)

-- Refactored to reduce the number of cases in evaluation.
data AExpr
  = Var String
  | IntConst Integer
  | ABin BinAOp AExpr AExpr
  | AUn UnAOp AExpr
  deriving (Show)

data BinAOp = Add | Sub | Mul | Div | Mod deriving (Show,Eq)

data UnAOp = Neg deriving (Show, Eq)
