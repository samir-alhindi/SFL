
module AST where

import Text.Parsec

data TopLevel = TL_Stmt Stmt | TL_Function Function | TL_Class Class

data Stmt =
      Print Expr
    | If SourcePos Expr Stmt
    | IfElse SourcePos Expr Stmt Stmt
    | Block [Stmt]
    deriving Show

data Class = Class String [Constructer] SourcePos
data Constructer = Constructer String [String] deriving (Show, Eq)

data Function = Function String [String] Expr

data Expr =
      Number Double
    | Boolean Bool
    | Name SourcePos String
    | Name' String
    | LetExpr String Expr Expr
    | Binary SourcePos BinOpp Expr Expr
    | Unary SourcePos UnaryOpp Expr
    | StringExpr String
    | Ternary SourcePos Expr Expr Expr
    | Lambda [String] Expr
    | Call SourcePos Expr [Expr]
    | Call' Expr [Expr]
    | List [Expr]
    | Match SourcePos Expr [(Expr,Expr)] Expr
    | Destructer String [String]
    | Hack Expr
    deriving (Show, Eq)


data BinOpp = Plus | Minus | Multiply | Divide | Mod
  | And | Or
  | Greater | Less | GreaterEqual | LessEqual | DoubleEquals | NotEquals
  | Curry | Bind
  | Cons | Concat
  deriving (Show, Eq)

data UnaryOpp = Negation | Not | Head | Tail deriving (Show, Eq)