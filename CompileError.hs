module CompileError where

import Text.Parsec.Error
import Symbol

data CompileError = PError ParseError
                  | SError [CompileLog]
instance Show CompileError where
  show (PError pe) = "Parse Error: " ++ show pe
  show (SError cl) = show cl

data CompileLog = Err SemanticError
                | Warn Warning
                deriving(Eq)
instance Show CompileLog where
  show (Err se) = "Error : " ++ show se
  show (Warn w) = "Warning : " ++ show w

data SemanticError = ReDecl String
                   | ReDeclDiffType String
                   | TypeError String
                   | UndeclVar String
                   | FunCallWithVar String
                   | InvalidNumOfParam Integer Integer
                   deriving(Eq)

instance Show SemanticError where
  show (ReDecl s) = "redeclaration of " ++ s
  show (UndeclVar s) = s ++ " undeclarad variable"
  show (FunCallWithVar s) = s ++ "is not a function"
  show (TypeError s) = "TypeError : " ++ s
  show (ReDeclDiffType s) = "ReDeclDiffTYpe : " ++ s
  show (InvalidNumOfParam i1 i2) = "different number of parameter"

data Warning = ParamShadow String
             | CallUndefinedFunc String
             deriving (Eq)

instance Show Warning where
  show (CallUndefinedFunc s) = "call undefined function " ++ s
  show (ParamShadow s)       = "declaration of " ++ s ++" shadows a parameter"
dupError :: [CompileLog] -> String -> [CompileLog]
dupError log str = log ++ [Err $ ReDecl str]
