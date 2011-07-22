module Syntax.AST where

import Syntax.Types

data Identifier = Identifier String
                | STableKey Integer
                  deriving (Eq)
instance Show Identifier where
  show (Identifier s) = s
  show (STableKey i) = "(symbolTableKey " ++ show i ++ ")"

data Constant = Constant Integer
              deriving (Eq)
instance Show Constant where
  show (Constant i) = show i

data Expr = Const  Integer
          | Ident  Identifier
          | Assign Identifier Expr
          | L_OR   Expr Expr
          | L_AND  Expr Expr
          | Equal  Expr Expr
          | NEqual Expr Expr
          | Gt     Expr Expr
          | Lt     Expr Expr
          | Ge     Expr Expr
          | Le     Expr Expr
          | Plus   Expr Expr
          | Minus  Expr Expr
          | Mul    Expr Expr
          | Div    Expr Expr
          | UMinus Expr
          | ExprList Expr Expr
          | FunCall Identifier [Expr]
          deriving (Eq)

data Decl = Decl Syntax.Types.Type [Identifier]

data Stmt = EmptyStmt
          | Expression Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Return (Maybe Expr)
          | Declaration Decl
          | Compound [Stmt]

data ParamDecl = ParamDecl [(Type, Identifier)]

data FuncDecl = FuncDecl Type Identifier ParamDecl FuncBody

data FuncBody = Body [Stmt]

data Program = ExDecl Decl
             | Func FuncDecl

data CTranslUnit = CTU [Program]
