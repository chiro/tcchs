module CodeGen.CodeGenerator where

import Control.Monad (foldM)
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Syntax.AST
import Symbol
import CompileError
import CodeGen.AsmCode
import CodeGen.CompilationState

topLevelCodeGeneration :: [CompileLog]
                          -> [(String,SymbolTable)]
                          -> GlobalSymTable
                          -> CTranslUnit
                          -> [Code]
topLevelCodeGeneration cl slist gtable (CTU plist) =
  makeExternalFuncCode cl ++ snd (foldl f (emptyState, []) plist)
  where f (cs, codes) p =
          let (nc, _) = codeGenerationP cs slist gtable p
          in (modifyLabel (countLabel cs) emptyState, codes ++ nc)

makeExternalFuncCode :: [CompileLog] -> [Code]
makeExternalFuncCode = foldl f []
  where f codes (Warn (CallUndefinedFunc s)) = codes ++ [COp (Op1 "EXTERN" (Ex s))]
        f codes _                            = codes

codeGenerationP :: CompilationState
                   -> [(String,SymbolTable)]
                   -> GlobalSymTable
                   -> Program
                   -> ([Code],CompilationState)
codeGenerationP cs _ gtable (ExDecl decl) = (Prelude.map f (ci decl), cs)
    where f s = let sym = fromJust $ M.lookup s gtable
                in COp (Op0 ("COMMON " ++ s ++ " " ++ show (sizeOf sym)))
          ci (Decl _ ids) = Prelude.map (takeName (table cs)) ids
codeGenerationP cs sl _ (Func fdecl@(FuncDecl _ (Identifier s) _ _)) =
  runState (generateFunc fdecl) (modifyTable (lookupS s sl) cs)
  where lookupS _ [] = error "no"
        lookupS s ((str,table):xs) = if s == str then table else lookupS s xs

generateFunc :: FuncDecl -> State CompilationState [Code]
generateFunc (FuncDecl _ (Identifier s) _ body) = do
  modify (modifyFuncName s)
  _ <- generateFuncBody body
  code' <- generateFunc' s body
  return $ Comment ("function " ++ s) : code'

generateFunc' :: String -> FuncBody -> State CompilationState [Code]
generateFunc' name body = do
  code <- generateFuncBody body
  cs' <- get
  return $
    [COp $ Op1 "GLOBAL" (Ex name),
     OnlyLabel name,
     COp $ Op1 "push" (Reg Ebp),
     COp $ Op2 "mov" (Reg Ebp) (Reg Esp),
     COp $ Op2 "sub" (Reg Esp) (CodeGen.AsmCode.Const (maxAlloc cs'))]
    ++ code
    ++ [OnlyLabel (name ++ "ret"),
        COp $ Op2 "mov" (Reg Esp) (Reg Ebp),
        COp $ Op1 "pop" (Reg Ebp),
        COp $ Op0 "ret", EmptyCode]

generateFuncBody :: FuncBody -> State CompilationState [Code]
generateFuncBody (Body stmts) = foldM f [] stmts
  where f code stmt = do
          c <- emitStatement stmt
          return $ code ++ c

emitStatement :: Stmt -> State CompilationState [Code]
emitStatement EmptyStmt = return []
emitStatement (Expression expr) = emitExpr expr

emitStatement (Return Nothing) = do
  cs <- get
  return $ [COp $ Op1 "jmp" (Label (functionName cs ++ "ret"))]
emitStatement (Return (Just expr)) = do
  cs <- get
  code <- emitExpr expr
  return $ code ++ [COp $ Op1 "jmp" (Label (functionName cs ++ "ret"))]

emitStatement (Declaration (Decl _ il)) = do
  cs <- get
  put (modifyLoc cs (negate (minL cs il)))
  return []
  where minL cs = foldl (\i k -> f cs i k) 0
        f cs i (STableKey k) = let (SVar sym) = fromJust $ M.lookup k (table cs)
                               in min i (vadr sym)
        f _ i _ = i

emitStatement (Compound stmts) = foldM f [] stmts
  where f code stmt = do
          c <- emitStatement stmt
          return $ code ++ c

emitStatement (While e s) = do
  c <- emitExpr e
  c1 <- emitStatement s
  cs <- get
  modify (addLabelCount 2)
  return $ [OnlyLabel (genLabel cs 0)]
    ++ c
    ++ [COp $ Op2 "cmp" (Reg Eax) (CodeGen.AsmCode.Const 0),
        COp $ Op1 "je" (Label (genLabel cs 1))]
    ++ c1
    ++ [COp $ Op1 "jmp" (Label (genLabel cs 0)),
        OnlyLabel (genLabel cs 1)]

emitStatement (If expr s1 s2) = do
  cs <- get
  modify (addLabelCount 2)
  c <- emitExpr expr
  c1 <- emitStatement s1
  c2 <- emitStatement s2
  return $ c ++ [emitOp2 "cmp" (Reg Eax) (CodeGen.AsmCode.Const 0)]
    ++ [emitOp1 "je" (Label (genLabel cs 0))]
    ++ c1
    ++ [emitOp1 "jmp" (Label (genLabel cs 1)),
        OnlyLabel (genLabel cs 0)]
    ++ c2
    ++ [OnlyLabel (genLabel cs 1)]

class CodeGenerator a where
  codeGeneration :: CompilationState -> a -> ([Code],CompilationState)

emitExpr :: Expr -> State CompilationState [Code]
emitExpr (Syntax.AST.Const i) = do
  return [COp $ Op2 "mov" (Reg Eax) (CodeGen.AsmCode.Const i)]
emitExpr (Ident (Identifier s)) = do
  return [COp $ Op2 "mov" (Reg Eax) (GlobalRef s)]
emitExpr (Ident (STableKey i)) = do
  cs <- get
  let (SVar sym) = fromJust $ M.lookup i (table cs)
  return [COp $ Op2 "mov" (Reg Eax) (Ref Ebp (vadr sym))]
emitExpr (Assign (Identifier s) expr) = do
  rcode <- emitExpr expr
  return (rcode ++ [COp $ Op2 "mov" (GlobalRef s) (Reg Eax)])
emitExpr (Assign (STableKey i) expr) = do
  cs <- get
  let (SVar sym) = fromJust $ M.lookup i (table cs)
  rcode <- emitExpr expr
  return (rcode ++ [COp $ Op2 "mov" (Ref Ebp (vadr sym)) (Reg Eax)])
emitExpr (UMinus expr) = do
  code <- emitExpr expr
  return $ code ++ [COp $ Op1 "neg" (Reg Eax)]

-- TODO: Consolidate with the L_OR case.
emitExpr (L_AND e1 e2) = do
  cs <- get
  let label = genLabel cs 0
  modify (allocateLoc SInt)
  modify (addLabelCount 1)
  c1 <- emitExpr e1
  c2 <- emitExpr e2
  return $ [COp $ Op2 "mov dword" (Ref Ebp $ top cs) (CodeGen.AsmCode.Const 0)]
    ++ c1
    ++ [COp $ Op2 "cmp" (Reg Eax) (CodeGen.AsmCode.Const 0),
        COp $ Op1 "je" (Label label)]
    ++ c2
    ++ [COp $ Op2 "cmp" (Reg Eax) (CodeGen.AsmCode.Const 0),
        COp $ Op1 "je" (Label label),
        COp $ Op2 "mov dword" (Ref Ebp $ top cs) (CodeGen.AsmCode.Const 1),
        OnlyLabel label,
        COp $ Op2 "mov" (Reg Eax) (Ref Ebp $ top cs)]

emitExpr (L_OR e1 e2) = do
  cs <- get
  let label = genLabel cs 0
  modify (allocateLoc SInt)
  modify (addLabelCount 1)
  c1 <- emitExpr e1
  c2 <- emitExpr e2
  return $ [COp $ Op2 "mov dword" (Ref Ebp $ top cs) (CodeGen.AsmCode.Const 1)]
    ++ c1
    ++ [COp $ Op2 "cmp" (Reg Eax) (CodeGen.AsmCode.Const 0)]
    ++ [COp $ Op1 "jne" (Label label)]
    ++ c2
    ++ [COp $ Op2 "cmp" (Reg Eax) (CodeGen.AsmCode.Const 0)]
    ++ [COp $ Op1 "jne" (Label label)]
    ++ [COp $ Op2 "mov dword" (Ref Ebp $ top cs) (CodeGen.AsmCode.Const 0)]
    ++ [OnlyLabel label]
    ++ [COp $ Op2 "mov" (Reg Eax) (Ref Ebp $ top cs)]

-- TODO: Consolidate these
emitExpr (Plus l r) = do
  code <- generateRSL l r
  cs' <- get
  modify releaseLoc
  return $ code ++ [COp $ Op2 "add" (Reg Eax) (Ref Ebp $ top cs')]
emitExpr (Minus l r) = do
  code <- generateRSL l r
  cs' <- get
  modify releaseLoc
  return $ code ++ [COp $ Op2 "sub" (Reg Eax) (Ref Ebp $ top cs')]
emitExpr (Mul l r) = do
  code <- generateRSL l r
  cs' <- get
  modify releaseLoc
  return $ code ++ [COp $ Op2 "imul" (Reg Eax) (Ref Ebp $ top cs')]
emitExpr (Div l r) = do
  code <- generateRSL l r
  cs' <- get
  modify releaseLoc
  return $ code ++ [COp $ Op2 "idiv dword" (Reg Eax) (Ref Ebp $ top cs')]

emitExpr (Equal e1 e2) = generateCmp e1 e2 "sete"
emitExpr (NEqual e1 e2) = generateCmp e1 e2 "setne"
emitExpr (Gt e1 e2) = generateCmp e1 e2 "setg"
emitExpr (Lt e1 e2) = generateCmp e1 e2 "setl"
emitExpr (Ge e1 e2) = generateCmp e1 e2 "setge"
emitExpr (Le e1 e2) = generateCmp e1 e2 "setle"

emitExpr (ExprList e1 e2) = do
  code <- emitExpr e1
  code' <- emitExpr e2
  return $ code ++ code'

emitExpr (FunCall (Identifier s) elist) = do
  code <- f (reverse elist)
  return $ code
    ++ [COp $ Op1 "call" (Ex s),
        COp $ Op2 "add" (Reg Esp) (CodeGen.AsmCode.Const (4 * toInteger (length elist)))]
  where f [] = return []
        f (expr:xs) = do
          ec <- emitExpr expr
          ecc <- f xs
          return $ ec ++ [COp $ Op1 "push" (Reg Eax)] ++ ecc

generateRSL :: Expr -> Expr -> State CompilationState [Code]
generateRSL e1 e2 = do
  rcode <- emitExpr e2
  modify (allocateLoc SInt)
  cs' <- get
  let hcode = [COp $ Op2 "mov" (Ref Ebp $ top cs') (Reg Eax)]
  lcode <- emitExpr e1
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
