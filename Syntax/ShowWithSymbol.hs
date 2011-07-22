module Syntax.ShowWithSymbol where

import Syntax.Types
import Syntax.AST
import Symbol
import SemanticChecker
import Data.Maybe
import Data.Map as M

findSymTable s [] = error "cannot find symbol table"
findSymTable s ((name,table):xs)
  | s == name = table
  | otherwise = findSymTable s xs

showCTU :: GlobalSymTable -> [(String,SymbolTable)] -> CTranslUnit -> String
showCTU gtable slist (CTU plist) = foldl f "" plist
  where f s p = s ++ "\n" ++ showProgram gtable slist p

showProgram :: GlobalSymTable -> [(String,SymbolTable)] -> Program -> String
showProgram gtable slist (ExDecl (Decl t ilist)) =
  "(" ++ show t ++ " " ++ unwords (Prelude.map f ilist) ++ ")"
    where f id = show id ++ ":0"
showProgram gtable slist (Func f@(FuncDecl _ (Identifier i) _ _)) = showWithSym gtable (findSymTable i slist) f

class ShowWithSymbol a where
  showWithSym :: GlobalSymTable -> SymbolTable -> a -> String

instance ShowWithSymbol FuncDecl where
  showWithSym gtable stable (FuncDecl t (Identifier i) p b) =
    "((" ++ show t ++ " " ++ i ++ ":0)"
    ++ "(" ++ showWithSym gtable stable p ++ ")\n"
    ++ "(\n" ++ showWithSym gtable stable b ++ ")\n"

instance ShowWithSymbol ParamDecl where
  showWithSym gtable stable (ParamDecl lst) = unwords $ Prelude.map f lst
    where f (t,STableKey i) = let (Just (SVar sym)) = M.lookup i stable
                              in "(" ++ show t ++ " " ++ vname sym ++ ":"
                                 ++ show (level sym) ++ ":"
                                 ++ show (vadr sym) ++ ")"

instance ShowWithSymbol FuncBody where
  showWithSym gtable stable (Body st) = unlines (Prelude.map f st)
    where f stmt = showStmtWithSym gtable stable stmt "    "

showStmtWithSym :: GlobalSymTable -> SymbolTable -> Stmt -> String -> String
showStmtWithSym gtable stable EmptyStmt pre = pre ++ "(EMPTY)"
showStmtWithSym gtable stable (Expression expr) pre = pre ++ showWithSym gtable stable expr
showStmtWithSym gtable stable (If expr th el) pre = pre ++ "(IF " ++ showWithSym gtable stable expr ++ "\n"
                                                    ++ showStmtWithSym gtable stable th (pre ++ "    ") ++ "\n"
                                                    ++ showStmtWithSym gtable stable el (pre ++ "    ") ++ "\n"
                                                    ++ pre ++ ")"
showStmtWithSym gtable stable (While expr th) pre = pre ++ "(WHILE " ++ showWithSym gtable stable expr ++ "\n"
                                                    ++ showStmtWithSym gtable stable th (pre ++ "    ")
showStmtWithSym gtable stable (Return Nothing) pre = pre ++ "(RETURN)"
showStmtWithSym gtable stable (Return (Just expr)) pre = pre ++ "(RETURN " ++ showWithSym gtable stable expr ++ ")"
showStmtWithSym gtable stable (Declaration decl) pre = pre ++ showWithSym gtable stable decl
showStmtWithSym gtable stable (Compound lst) pre = pre ++ "(\n"
                                                   ++ init (unlines (Prelude.map (\s -> showStmtWithSym gtable stable s pre) lst)) ++ "\n"
                                                   ++ pre ++ ")"

showExprWithSymAux :: GlobalSymTable -> SymbolTable -> String -> Expr -> Expr -> String
showExprWithSymAux gtable stable s e1 e2 = "(" ++ s ++ " " ++ showWithSym gtable stable e1
                                           ++ " " ++ showWithSym gtable stable e2 ++ ")"

instance ShowWithSymbol Expr where
  showWithSym gtable stable (Const i) = show i
  showWithSym gtable stable (Ident i) = showWithSym gtable stable i
  showWithSym gtable stable (Assign i e) = "(= " ++ showWithSym gtable stable i ++ " "
                                           ++ showWithSym gtable stable e ++ ")"
  showWithSym gtable stable (L_OR e1 e2)   = showExprWithSymAux gtable stable "||" e1 e2
  showWithSym gtable stable (L_AND e1 e2)  = showExprWithSymAux gtable stable "&&" e1 e2
  showWithSym gtable stable (Equal e1 e2)  = showExprWithSymAux gtable stable "==" e1 e2
  showWithSym gtable stable (NEqual e1 e2) = showExprWithSymAux gtable stable "!=" e1 e2
  showWithSym gtable stable (Gt e1 e2)     = showExprWithSymAux gtable stable ">"  e1 e2
  showWithSym gtable stable (Lt e1 e2)     = showExprWithSymAux gtable stable "<"  e1 e2
  showWithSym gtable stable (Ge e1 e2)     = showExprWithSymAux gtable stable ">=" e1 e2
  showWithSym gtable stable (Le e1 e2)     = showExprWithSymAux gtable stable "<=" e1 e2
  showWithSym gtable stable (Plus e1 e2)   = showExprWithSymAux gtable stable "+"  e1 e2
  showWithSym gtable stable (Minus e1 e2)  = showExprWithSymAux gtable stable "-"  e1 e2
  showWithSym gtable stable (Mul e1 e2)    = showExprWithSymAux gtable stable "*"  e1 e2
  showWithSym gtable stable (Div e1 e2)    = showExprWithSymAux gtable stable "/"  e1 e2
  showWithSym gtable stable (UMinus e1)    = "(-" ++ showWithSym gtable stable e1 ++ ")"
  showWithSym gtable stable (FunCall i lst) = "(FUNCALL " ++ show i ++ ":0 "
                                                ++ unwords (Prelude.map (showWithSym gtable stable) lst) ++ ")"
  showWithSym gtable stable (ExprList e1 e2) = "(" ++ showWithSym gtable stable e1 ++ ","
                                                 ++ showWithSym gtable stable e2 ++ ")"

instance ShowWithSymbol Identifier where
  showWithSym gtable stable (Identifier s) = s ++ ":0"
  showWithSym gtable stable (STableKey i) =
    let (SVar sym) = fromJust $ M.lookup i stable
    in vname sym ++ ":" ++ show (level sym)

instance ShowWithSymbol Decl where
  showWithSym gtable stable (Decl t ids) =
    "(" ++ show t ++ unwords (Prelude.map f ids) ++ ")"
      where f (STableKey i) = let (SVar sym) = fromJust $ M.lookup i stable
                              in " " ++ vname sym ++ ":" ++ show (level sym) ++ ":" ++ show (vadr sym)
            f (Identifier s) = s
