
module AST where

import Text.Parsec

data Stmt =
      Print Expr
    | If SourcePos Expr Stmt
    | IfElse SourcePos Expr Stmt Stmt
    | Function String [String] Expr
    | Block [Stmt]
    | ClassDeclre String [Constructer] SourcePos
    deriving Show

data Constructer = Constructer String [String] deriving (Show, Eq)

data Expr =
      Number Double
    | Boolean Bool
    | Name SourcePos String
    | LetExpr String Expr Expr
    | Binary SourcePos BinOpp Expr Expr
    | Unary SourcePos UnaryOpp Expr
    | StringExpr String
    | Ternary SourcePos Expr Expr Expr
    | Lambda [String] Expr
    | Call SourcePos Expr [Expr]
    | List [Expr]
    | Match Expr [(Expr,Expr)] Expr
    | Hack Expr
    deriving (Show, Eq)


data BinOpp = Plus | Minus | Multiply | Divide | Mod
  | And | Or
  | Greater | Less | GreaterEqual | LessEqual | DoubleEquals | NotEquals
  | Bind
  | Cons
  deriving (Show, Eq)

data UnaryOpp = Negation | Not | Head | Tail deriving (Show, Eq)