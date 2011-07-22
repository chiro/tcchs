module CodeGen.CodeGenerator where

import Data.Map as M
import Data.Maybe
import Syntax.AST
import Symbol
import CompileError
import SemanticChecker
import CodeGen.AsmCode
import CodeGen.CompilationState
import Debug.Trace

topLevelCodeGeneration :: [CompileLog] -> [(String,SymbolTable)] -> GlobalSymTable -> CTranslUnit -> [Code]
topLevelCodeGeneration cl slist gtable (CTU plist) = makeExternalFuncCode cl ++ snd (foldl f (emptyState,[]) plist)
  where f (state,codes) p = let (nc,state') = codeGenerationP state slist gtable p
                            in (modifyLabel (countLabel state) emptyState,codes ++ nc)

makeExternalFuncCode :: [CompileLog] -> [Code]
makeExternalFuncCode = foldl f []
  where f codes (Warn (CallUndefinedFunc s)) = codes ++ [COp (Op1 "EXTERN" (Ex s))]
        f codes _                            = codes

codeGenerationP :: CompilationState -> [(String,SymbolTable)] -> GlobalSymTable -> Program -> ([Code],CompilationState)
codeGenerationP state sl gtable (ExDecl decl) = (Prelude.map f (ci decl),state)
    where f s = let sym = fromJust $ M.lookup s gtable
                in COp (Op0 ("COMMON " ++ s ++ " " ++ show (sizeOf sym)))
          ci (Decl t ids) = Prelude.map (takeName (table state)) ids
codeGenerationP state sl gtable (Func fdecl@(FuncDecl _ (Identifier s) _ body)) = codeGeneration (modifyTable (lookupS s sl) state) fdecl
                             where lookupS s [] = error "no"
                                   lookupS s ((str,table):xs) = if s == str
                                                                then table
                                                                else lookupS s xs

class CodeGenerator a where
  codeGeneration :: CompilationState -> a -> ([Code],CompilationState)

instance CodeGenerator FuncDecl where
  codeGeneration state fdecl@(FuncDecl _ (Identifier s) _ body) =
    let (codes,state') = codeGeneration (modifyFuncName s state) body
    in (Comment ("function " ++ s) : generateFuncCode fdecl state' codes,state')

generateFuncCode :: FuncDecl -> CompilationState -> [Code] -> [Code]
generateFuncCode (FuncDecl _ (Identifier s) _ body) state codes =
  let (codes,state') = codeGeneration state body
  in [COp $ Op1 "GLOBAL" (Ex s),OnlyLabel s,COp $ Op1 "push" (Reg Ebp),
      COp $ Op2 "mov" (Reg Ebp) (Reg Esp),COp $ Op2 "sub" (Reg Esp) (CodeGen.AsmCode.Const (maxAlloc state'))]
     ++ codes
     ++ [OnlyLabel (s ++ "ret"),COp $ Op2 "mov" (Reg Esp) (Reg Ebp),COp $ Op1 "pop" (Reg Ebp),
         COp $ Op0 "ret", EmptyCode]

instance CodeGenerator FuncBody where
  codeGeneration state (Body sl) = foldl f ([],state) sl
    where f (codes,state) s = let (nc,state') = codeGeneration state s
                              in (codes++nc,state')

instance CodeGenerator Stmt where
  codeGeneration state EmptyStmt = ([],state)
  codeGeneration state (Expression expr) = codeGeneration state expr
  codeGeneration state (Return Nothing) = ([COp $ Op1 "jmp" (Label (functionName state ++ "ret"))],state)
  codeGeneration state (Return (Just expr)) =
    let (codes,state') = codeGeneration state expr
    in (codes++[COp $ Op1 "jmp" (Label (functionName state ++ "ret"))],state')
  codeGeneration state (Declaration decl@(Decl _ il)) = ([],modifyLoc state (negate (minL il)))
    where minL = foldl f 0
          f i (STableKey k) = let (SVar sym) = fromJust $ M.lookup k (table state)
                              in min i (vadr sym)
  codeGeneration state (Compound slist) = foldl f ([],state) slist
    where f (codes,state) stmt = let (code,state') = codeGeneration state stmt
                                 in (codes++code,state')

  codeGeneration state (If expr s1 s2) = let (c,state') = codeGeneration (addLabelCount 2 state) expr
                                         in let (c1,state'') = codeGeneration state' s1
                                            in let (c2,state''') = codeGeneration state'' s2
                                               in (c ++ [emitOp2 "cmp" (Reg Eax) (CodeGen.AsmCode.Const 0)]
                                                   ++ [emitOp1 "je" (Label (genLabel state 0))]
                                                   ++ c1
                                                   ++ [emitOp1 "jmp" (Label (genLabel state 1))]
                                                   ++ [OnlyLabel (genLabel state 0)]
                                                   ++ c2
                                                   ++ [OnlyLabel (genLabel state 1)],
                                                   state''')

  codeGeneration state (While e s) = let (c,state') = codeGeneration state e
                                     in let (c1,state'') = codeGeneration state' s
                                        in ([OnlyLabel (genLabel state'' 0)] ++ c
                                            ++ [COp $ Op2 "cmp" (Reg Eax) (CodeGen.AsmCode.Const 0),
                                                COp $ Op1 "je" (Label (genLabel state'' 1))]
                                            ++ c1 ++ [COp $ Op1 "jmp" (Label (genLabel state'' 0)),OnlyLabel (genLabel state'' 1)],addLabelCount 2 state'')

instance CodeGenerator Expr where
  codeGeneration state (Syntax.AST.Const i) = ([COp $ Op2 "mov" (Reg Eax) (CodeGen.AsmCode.Const i)],state)
  codeGeneration state (Ident (Identifier s)) = ([COp $ Op2 "mov" (Reg Eax) (GlobalRef s)],state)
  codeGeneration state (Ident (STableKey i)) = let (SVar sym) = fromJust $ M.lookup i (table state)
                                               in ([COp $ Op2 "mov" (Reg Eax) (Ref Ebp (vadr sym))],state)
  codeGeneration state (Assign (Identifier s) expr) = let (rcode,state') = codeGeneration state expr
                                                      in (rcode ++ [COp $ Op2 "mov" (GlobalRef s) (Reg Eax)],state')
  codeGeneration state (Assign (STableKey i) expr) = let (SVar sym) = fromJust $ M.lookup i (table state)
                                                     in let (rcode,state') = codeGeneration state expr
                                                        in (rcode ++ [COp $ Op2 "mov" (Ref Ebp (vadr sym)) (Reg Eax)],state')
  codeGeneration state (UMinus expr) = let (codes,state') = codeGeneration state expr
                                       in (codes++[COp $ Op1 "neg" (Reg Eax)],state')

  codeGeneration state (L_AND e1 e2) = let state' = allocateLoc SInt state
                                      in let (c1,state'') = codeGeneration (addLabelCount 1 state') e1
                                         in let (c2,state''') = codeGeneration state'' e2
                                            in ([COp $ Op2 "mov dword" (Ref Ebp $ top state) (CodeGen.AsmCode.Const 0)]
                                               ++ c1
                                               ++ [COp $ Op2 "cmp" (Reg Eax) (CodeGen.AsmCode.Const 0)]
                                               ++ [COp $ Op1 "je" (Label (genLabel state 0))]
                                               ++ c2
                                               ++ [COp $ Op2 "cmp" (Reg Eax) (CodeGen.AsmCode.Const 0)]
                                               ++ [COp $ Op1 "je" (Label (genLabel state 0))]
                                               ++ [COp $ Op2 "mov dword" (Ref Ebp $ top state) (CodeGen.AsmCode.Const 1)]
                                               ++ [OnlyLabel (genLabel state 0)]
                                               ++ [COp $ Op2 "mov" (Reg Eax) (Ref Ebp $ top state)],state''')

  codeGeneration state (L_OR e1 e2) = let state' = allocateLoc SInt state
                                      in let (c1,state'') = codeGeneration (addLabelCount 1 state') e1
                                         in let (c2,state''') = codeGeneration state'' e2
                                            in ([COp $ Op2 "mov dword" (Ref Ebp $ top state) (CodeGen.AsmCode.Const 1)]
                                               ++ c1
                                               ++ [COp $ Op2 "cmp" (Reg Eax) (CodeGen.AsmCode.Const 0)]
                                               ++ [COp $ Op1 "jne" (Label (genLabel state 0))]
                                               ++ c2
                                               ++ [COp $ Op2 "cmp" (Reg Eax) (CodeGen.AsmCode.Const 0)]
                                               ++ [COp $ Op1 "jne" (Label (genLabel state 0))]
                                               ++ [COp $ Op2 "mov dword" (Ref Ebp $ top state) (CodeGen.AsmCode.Const 0)]
                                               ++ [OnlyLabel (genLabel state 0)]
                                               ++ [COp $ Op2 "mov" (Reg Eax) (Ref Ebp $ top state)],state''')

  codeGeneration state (Plus e1 e2) = let (codes,state') = generateRSL state e1 e2
                                      in (codes ++ [COp $ Op2 "add" (Reg Eax) (Ref Ebp $ top state')],releaseLoc state')
  codeGeneration state (Minus e1 e2) = let (codes,state') = generateRSL state e1 e2
                                     in (codes ++ [COp $ Op2 "sub" (Reg Eax) (Ref Ebp $ top state')],releaseLoc state')
  codeGeneration state (Mul e1 e2) = let (codes,state') = generateRSL state e1 e2
                                     in (codes ++ [COp $ Op2 "imul" (Reg Eax) (Ref Ebp $ top state')],releaseLoc state')
  codeGeneration state (Div e1 e2) = let (codes,state') = generateRSL state e1 e2
                                     in (codes ++ [COp $ Op0 "cdq" , COp $ Op1 "idiv dword" (Ref Ebp $ top state')],releaseLoc state')

  codeGeneration state (Equal e1 e2) = generateCmp state e1 e2 "sete"
  codeGeneration state (NEqual e1 e2) = generateCmp state e1 e2 "setne"
  codeGeneration state (Gt e1 e2) = generateCmp state e1 e2 "setg"
  codeGeneration state (Lt e1 e2) = generateCmp state e1 e2 "setl"
  codeGeneration state (Ge e1 e2) = generateCmp state e1 e2 "setge"
  codeGeneration state (Le e1 e2) = generateCmp state e1 e2 "setle"

  codeGeneration state (FunCall (Identifier s) elist) = let (codes,state') = f (reverse elist) state
                                                        in (codes ++ [COp $ Op1 "call" (Ex s)]
                                                            ++ [COp $ Op2 "add" (Reg Esp) 
                                                                (CodeGen.AsmCode.Const (4 * toInteger (length elist)))],state')
                                                          where f [] st = ([],st)
                                                                f (expr:xs) st = let (ec,st') = codeGeneration st expr
                                                                                 in let (ecc,st'') = f xs st'
                                                                                    in (ec ++ [COp $ Op1 "push" (Reg Eax)] ++ ecc,st'')

  codeGeneration state (ExprList e1 e2) = let (codes,state') = codeGeneration state e1
                                          in let (codes',state'') = codeGeneration state' e2
                                             in (codes ++ codes',state'')
  codeGeneration state expr = ([],state)

generateCmp :: CompilationState -> Expr -> Expr -> String -> ([Code],CompilationState)
generateCmp state e1 e2 s = let (codes,state') = generateRSL state e1 e2
                            in (codes ++ [COp $ Op2 "cmp" (Reg Eax) (Ref Ebp $ top state'),
                                          COp $ Op1 s (Ex "al"),
                                          COp $ Op2 "movzx" (Reg Eax) (Ex "al")],releaseLoc state')

generateRSL :: CompilationState -> Expr -> Expr -> ([Code],CompilationState)
generateRSL state e1 e2 = let (rcode,state') = codeGeneration state e2
                            in let state'' = allocateLoc SInt state'
                               in let hcode = [COp $ Op2 "mov" (Ref Ebp $ top state'') (Reg Eax)]
                                  in let (lcode,state''') = codeGeneration state'' e1
                                     in (rcode ++ hcode ++ lcode,state''')