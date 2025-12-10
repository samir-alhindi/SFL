
module AST where

import Text.Parsec

data Declaration =
  TL_Function Function |
  TL_Class Class |
  TL_Enumeration Enumeration |
  TL_Import Import

data Class = Class String [Constructer] SourcePos

data Constructer = Constructer String [String] deriving (Show, Eq)

data Function = Function String [String] Expr

data Enumeration = Enumeration String [String] SourcePos

data Import = Import String

data Expr =
      Number Double
    | Boolean Bool
    | Name SourcePos String
    | LetExpr String Expr Expr
    | Binary SourcePos BinOpp Expr Expr
    | Unary SourcePos UnaryOpp Expr
    | Character Char
    | Ternary SourcePos Expr Expr Expr
    | Lambda [String] Expr
    | Call SourcePos Expr [Expr]
    | List [Expr]
    | Match SourcePos Expr [(Pattern,Expr)] Expr
    deriving (Show, Eq)
  
data Pattern =
    ExprPattern Expr
  | DestructerPattern Destructer
  | ListPattern (String, String)
  deriving (Show, Eq)

data Destructer = Destructer String [String] deriving (Show, Eq)

data BinOpp = Plus | Minus | Multiply | Divide | Mod
  | And | Or
  | Greater | Less | GreaterEqual | LessEqual | DoubleEquals | NotEquals
  | Curry | Bind
  | Cons | Concat
  deriving (Show, Eq)

data UnaryOpp = Negation | Not | Head | Tail deriving (Show, Eq)