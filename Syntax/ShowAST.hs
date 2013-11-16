module Syntax.ShowAST where

import Syntax.AST

showExpr :: String -> Expr -> Expr -> String
showExpr s e1 e2 = "(" ++ s ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"

instance Show Expr where
  show (Const i) = show i
  show (Ident s) = show s
  show (Assign i e) = "(= " ++ show i ++ " " ++ show e ++ ")"
  show (L_OR e1 e2)   = showExpr "||" e1 e2
  show (L_AND e1 e2)  = showExpr "&&" e1 e2
  show (Equal e1 e2)  = showExpr "==" e1 e2
  show (NEqual e1 e2) = showExpr "!=" e1 e2
  show (Gt e1 e2)     = showExpr ">"  e1 e2
  show (Lt e1 e2)     = showExpr "<"  e1 e2
  show (Ge e1 e2)     = showExpr ">=" e1 e2
  show (Le e1 e2)     = showExpr "<=" e1 e2
  show (Plus e1 e2)   = showExpr "+"  e1 e2
  show (Minus e1 e2)  = showExpr "-"  e1 e2
  show (Mul e1 e2)    = showExpr "*"  e1 e2
  show (Div e1 e2)    = showExpr "/"  e1 e2
  show (UMinus e1) = "(- " ++ show e1 ++ ")"
  show (FunCall i lst) = "(FUNCALL " ++ show i ++ " "
                         ++ unwords (map show lst) ++ ")"
  show (ExprList e1 e2) = "(" ++ show e1 ++ "," ++ show e2 ++ ")"

instance Show Program where
  show = showP

instance Show CTranslUnit where
  show (CTU pl) = unlines (map ((++ "\n") . show) pl)

showP :: Program -> String
showP (ExDecl decl) = showDecl decl ++ "\n"
showP (Func f) = showFunc f ++ "\n"

showDecl :: Decl -> String
showDecl (Decl Int ids) = "(Declaration type:int " ++ unwords (map f ids) ++ ")"
  where f id = show id ++ " "

showFunc :: FuncDecl -> String
showFunc (FuncDecl t i p b) =
  "((" ++ show t ++ " " ++ show i ++ ") " ++ "(" ++ showParamDecl p ++ ")\n"
  ++ "(\n" ++ showBody b ++ "))\n"

showParamDecl :: ParamDecl -> String
showParamDecl (ParamDecl lst) = unwords (map f lst)
  where f (t,i) = "(" ++ show t ++ " " ++ show i ++ ")"

showBody :: FuncBody -> String
showBody (Body st) = unlines (map f st)
  where f st = showStmt st "  "

showStmt :: Stmt -> String -> String
showStmt EmptyStmt pre = pre ++ "(EMPTY)"
showStmt (Expression expr) pre = pre ++ show expr
showStmt (If expr th el) pre = pre ++ "(IF " ++ show expr ++ "\n"
                               ++ showStmt th (pre ++ "  ") ++ "\n"
                               ++ showStmt el (pre ++ "  ") ++ "\n"
                               ++ pre ++ ")"
showStmt (While expr th) pre = pre ++ "(WHILE " ++ show expr ++ "\n"
                               ++ showStmt th (pre ++ "  ")
showStmt (Return Nothing) pre = pre ++ "(RETURN)"
showStmt (Return (Just expr)) pre = pre ++ "(RETURN " ++ show expr ++ ")"
showStmt (Declaration decl) pre = pre ++ showDecl decl
showStmt (Compound lst) pre =
  pre ++ "(\n" ++ init (unlines (map (`showStmt` pre) lst)) ++ "\n" ++ pre ++ ")"
