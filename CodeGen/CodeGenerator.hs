module CodeGen.CodeGenerator where

import Control.Monad (foldM)
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Syntax.AST as AS
import Symbol
import CompileError
import CodeGen.AsmCode as AC
import CodeGen.CompilationState

topLevelCodeGeneration :: [CompileLog]
                          -> [(String,SymbolTable)]
                          -> GlobalSymTable
                          -> CTranslUnit
                          -> [Code]
topLevelCodeGeneration cl slist gtable (CTU plist) =
  makeExternalFuncCode cl ++ snd (foldl f (emptyState, []) plist)
  where f (cs, codes) p =
          let (nc, _) = genProgram cs slist gtable p
          in (modifyLabel (countLabel cs) emptyState, codes ++ nc)

makeExternalFuncCode :: [CompileLog] -> [Code]
makeExternalFuncCode = foldl f []
  where f codes (Warn (CallUndefinedFunc s)) = codes ++ [COp (Op1 "EXTERN" (Ex s))]
        f codes _                            = codes

genProgram :: CompilationState
                   -> [(String,SymbolTable)]
                   -> GlobalSymTable
                   -> Program
                   -> ([Code],CompilationState)
genProgram cs _ gtable (ExDecl decl) = (Prelude.map f (ci decl), cs)
    where f s = let sym = fromJust $ M.lookup s gtable
                in COp (Op0 ("COMMON " ++ s ++ " " ++ show (sizeOf sym)))
          ci (Decl _ ids) = Prelude.map (takeName (table cs)) ids
genProgram cs sl _ (Func fdecl@(FuncDecl _ (Identifier s) _ _)) =
  runState (genFunc fdecl) (modifyTable (lookupS s sl) cs)
  where lookupS _ [] = error "no"
        lookupS s ((str,table):xs) = if s == str then table else lookupS s xs

genFunc :: FuncDecl -> State CompilationState [Code]
genFunc (FuncDecl _ (Identifier s) _ body) = do
  modify (modifyFuncName s)
  _ <- genFuncBody body
  code' <- genFunc' s body
  return $ Comment ("function " ++ s) : code'

genFunc' :: String -> FuncBody -> State CompilationState [Code]
genFunc' name body = do
  code <- genFuncBody body
  cs' <- get
  return $
    [COp $ Op1 "GLOBAL" (Ex name),
     OnlyLabel name,
     COp $ Op1 "push" (Reg Ebp),
     COp $ Op2 "mov" (Reg Ebp) (Reg Esp),
     COp $ Op2 "sub" (Reg Esp) (AC.Const (maxAlloc cs'))]
    ++ code
    ++ [OnlyLabel (name ++ "ret"),
        COp $ Op2 "mov" (Reg Esp) (Reg Ebp),
        COp $ Op1 "pop" (Reg Ebp),
        COp $ Op0 "ret", EmptyCode]

genFuncBody :: FuncBody -> State CompilationState [Code]
genFuncBody (Body stmts) = foldM f [] stmts
  where f code stmt = do
          c <- genStmt stmt
          return $ code ++ c

genStmt :: Stmt -> State CompilationState [Code]
genStmt EmptyStmt = return []
genStmt (Expression expr) = genExpr expr

genStmt (Return Nothing) = do
  cs <- get
  return $ [COp $ Op1 "jmp" (Label (functionName cs ++ "ret"))]
genStmt (Return (Just expr)) = do
  cs <- get
  code <- genExpr expr
  return $ code ++ [COp $ Op1 "jmp" (Label (functionName cs ++ "ret"))]

genStmt (Declaration (Decl _ il)) = do
  cs <- get
  put (modifyLoc cs (negate (minL cs il)))
  return []
  where minL cs = foldl (\i k -> f cs i k) 0
        f cs i (STableKey k) = let (SVar sym) = fromJust $ M.lookup k (table cs)
                               in min i (vadr sym)
        f _ i _ = i

genStmt (Compound stmts) = foldM f [] stmts
  where f code stmt = do
          c <- genStmt stmt
          return $ code ++ c

genStmt (While e s) = do
  c <- genExpr e
  c1 <- genStmt s
  cs <- get
  modify (addLabelCount 2)
  return $ [OnlyLabel (genLabel cs 0)]
    ++ c
    ++ [COp $ Op2 "cmp" (Reg Eax) (AC.Const 0),
        COp $ Op1 "je" (Label (genLabel cs 1))]
    ++ c1
    ++ [COp $ Op1 "jmp" (Label (genLabel cs 0)),
        OnlyLabel (genLabel cs 1)]

genStmt (If expr s1 s2) = do
  cs <- get
  modify (addLabelCount 2)
  c <- genExpr expr
  c1 <- genStmt s1
  c2 <- genStmt s2
  return $ c ++ [emitOp2 "cmp" (Reg Eax) (AC.Const 0)]
    ++ [emitOp1 "je" (Label (genLabel cs 0))]
    ++ c1
    ++ [emitOp1 "jmp" (Label (genLabel cs 1)),
        OnlyLabel (genLabel cs 0)]
    ++ c2
    ++ [OnlyLabel (genLabel cs 1)]

genExpr :: Expr -> State CompilationState [Code]
genExpr (AS.Const i) = do
  return [COp $ Op2 "mov" (Reg Eax) (AC.Const i)]
genExpr (Ident (Identifier s)) = do
  return [COp $ Op2 "mov" (Reg Eax) (GlobalRef s)]
genExpr (Ident (STableKey i)) = do
  cs <- get
  let (SVar sym) = fromJust $ M.lookup i (table cs)
  return [COp $ Op2 "mov" (Reg Eax) (Ref Ebp (vadr sym))]
genExpr (Assign (Identifier s) expr) = do
  rcode <- genExpr expr
  return (rcode ++ [COp $ Op2 "mov" (GlobalRef s) (Reg Eax)])
genExpr (Assign (STableKey i) expr) = do
  cs <- get
  let (SVar sym) = fromJust $ M.lookup i (table cs)
  rcode <- genExpr expr
  return (rcode ++ [COp $ Op2 "mov" (Ref Ebp (vadr sym)) (Reg Eax)])
genExpr (UMinus expr) = do
  code <- genExpr expr
  return $ code ++ [COp $ Op1 "neg" (Reg Eax)]

-- TODO: Consolidate with the L_OR case.
genExpr (L_AND e1 e2) = do
  cs <- get
  let label = genLabel cs 0
  modify (allocateLoc SInt)
  modify (addLabelCount 1)
  c1 <- genExpr e1
  c2 <- genExpr e2
  return $ [COp $ Op2 "mov dword" (Ref Ebp $ top cs) (AC.Const 0)]
    ++ c1
    ++ [COp $ Op2 "cmp" (Reg Eax) (AC.Const 0),
        COp $ Op1 "je" (Label label)]
    ++ c2
    ++ [COp $ Op2 "cmp" (Reg Eax) (AC.Const 0),
        COp $ Op1 "je" (Label label),
        COp $ Op2 "mov dword" (Ref Ebp $ top cs) (AC.Const 1),
        OnlyLabel label,
        COp $ Op2 "mov" (Reg Eax) (Ref Ebp $ top cs)]

genExpr (L_OR e1 e2) = do
  cs <- get
  let label = genLabel cs 0
  modify (allocateLoc SInt)
  modify (addLabelCount 1)
  c1 <- genExpr e1
  c2 <- genExpr e2
  return $ [COp $ Op2 "mov dword" (Ref Ebp $ top cs) (AC.Const 1)]
    ++ c1
    ++ [COp $ Op2 "cmp" (Reg Eax) (AC.Const 0)]
    ++ [COp $ Op1 "jne" (Label label)]
    ++ c2
    ++ [COp $ Op2 "cmp" (Reg Eax) (AC.Const 0)]
    ++ [COp $ Op1 "jne" (Label label)]
    ++ [COp $ Op2 "mov dword" (Ref Ebp $ top cs) (AC.Const 0)]
    ++ [OnlyLabel label]
    ++ [COp $ Op2 "mov" (Reg Eax) (Ref Ebp $ top cs)]

-- TODO: Consolidate these
genExpr (Plus l r) = do
  code <- generateRSL l r
  cs' <- get
  modify releaseLoc
  return $ code ++ [COp $ Op2 "add" (Reg Eax) (Ref Ebp $ top cs')]
genExpr (Minus l r) = do
  code <- generateRSL l r
  cs' <- get
  modify releaseLoc
  return $ code ++ [COp $ Op2 "sub" (Reg Eax) (Ref Ebp $ top cs')]
genExpr (Mul l r) = do
  code <- generateRSL l r
  cs' <- get
  modify releaseLoc
  return $ code ++ [COp $ Op2 "imul" (Reg Eax) (Ref Ebp $ top cs')]
genExpr (Div l r) = do
  code <- generateRSL l r
  cs' <- get
  modify releaseLoc
  return $ code ++ [COp $ Op2 "idiv dword" (Reg Eax) (Ref Ebp $ top cs')]

genExpr (Equal e1 e2) = generateCmp e1 e2 "sete"
genExpr (NEqual e1 e2) = generateCmp e1 e2 "setne"
genExpr (Gt e1 e2) = generateCmp e1 e2 "setg"
genExpr (Lt e1 e2) = generateCmp e1 e2 "setl"
genExpr (Ge e1 e2) = generateCmp e1 e2 "setge"
genExpr (Le e1 e2) = generateCmp e1 e2 "setle"

genExpr (ExprList e1 e2) = do
  code <- genExpr e1
  code' <- genExpr e2
  return $ code ++ code'

genExpr (FunCall (Identifier s) elist) = do
  code <- f (reverse elist)
  return $ code
    ++ [COp $ Op1 "call" (Ex s),
        COp $ Op2 "add" (Reg Esp) (AC.Const (4 * toInteger (length elist)))]
  where f [] = return []
        f (expr:xs) = do
          ec <- genExpr expr
          ecc <- f xs
          return $ ec ++ [COp $ Op1 "push" (Reg Eax)] ++ ecc

generateRSL :: Expr -> Expr -> State CompilationState [Code]
generateRSL e1 e2 = do
  rcode <- genExpr e2
  modify (allocateLoc SInt)
  cs' <- get
  let hcode = [COp $ Op2 "mov" (Ref Ebp $ top cs') (Reg Eax)]
  lcode <- genExpr e1
  return $ rcode ++ hcode ++ lcode

generateCmp :: Expr -> Expr -> String -> State CompilationState [Code]
generateCmp e1 e2 s = do
  code <- generateRSL e1 e2
  cs <- get
  modify releaseLoc
  return $ code
    ++ [COp $ Op2 "cmp" (Reg Eax) (Ref Ebp $ top cs),
        COp $ Op1 s (Ex "al"),
        COp $ Op2 "movzx" (Reg Eax) (Ex "al")]
