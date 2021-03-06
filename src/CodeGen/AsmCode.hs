module CodeGen.AsmCode where

import Syntax.AST
import Symbol
import Data.Map as M
import Data.Maybe

type Label = String

data Code = COp Op
          | WithLabel Label Op
          | OnlyLabel Label
          | EmptyCode
          | Comment String
          deriving (Eq)

emitOp0 :: String -> Code
emitOp0 name = COp $ Op0 name

emitOp1 :: String -> Location -> Code
emitOp1 name loc = COp $ Op1 name loc

emitOp2 :: String -> Location -> Location -> Code
emitOp2 name loc1 loc2 = COp $ Op2 name loc1 loc2

mov :: Location -> Location -> Code
mov = emitOp2 "mov"

cmp :: Location -> Location -> Code
cmp = emitOp2 "cmp"

jmp :: Location -> Code
jmp = emitOp1 "jmp"

je :: Location -> Code
je = emitOp1 "je"

jne :: Location -> Code
jne = emitOp1 "jne"

instance Show Code where
  show (COp op) = show op
  show (OnlyLabel l) = l ++ ":\n"
  show (WithLabel l op) = l ++ show op
  show EmptyCode = "\n"
  show (Comment c) = "; " ++ c ++ "\n"

data Op = Op0 String
        | Op1 String Location
        | Op2 String Location Location
        deriving (Eq)

instance Show Op where
  show (Op0 name) = "\t" ++ name ++ "\n"
  show (Op1 name loc) = "\t" ++ name ++ " " ++ show loc ++ "\n"
  show (Op2 name loc1 loc2) = "\t" ++ name ++ " " ++ show loc1 ++ ", " ++ show loc2 ++ "\n"

data Register = Esp
              | Ebp
              | Eax
              deriving (Eq)

instance Show Register where
  show Esp = "esp"
  show Ebp = "ebp"
  show Eax = "eax"

data Location = Reg Register
              | Ref Register Integer
              | Ex  String
              | GlobalRef String
              | Label String
              | Const Integer
              deriving (Eq)

instance Show Location where
  show (Reg reg) = show reg
  show (Ref r i) = "[" ++ show r ++ toStr i ++ "]"
  show (Ex s)    = s
  show (GlobalRef s) = '[' : s ++ "]"
  show (Label s) = s
  show (CodeGen.AsmCode.Const i) = show i

toStr :: Integer -> String
toStr i
  | i >= 0 = '+' : show i
  | otherwise = show i

takeName :: SymbolTable -> Identifier -> String
takeName _ (Identifier s) = s
takeName stable (STableKey k)  = let (SVar sym) = fromJust (M.lookup k stable)
                                 in vname sym
