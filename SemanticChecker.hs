module SemanticChecker where

import Syntax.Types
import Syntax.AST
import Symbol
import CompileError
import Stack as S
import Debug.Trace
import Data.List
import Data.Map as M
import Data.Maybe


createGlobalSTable :: CTranslUnit -> (GlobalSymTable,[CompileLog])
createGlobalSTable (CTU plist) = collectGlobalSyms M.empty [] plist

collectGlobalSyms :: GlobalSymTable -> [CompileLog] -> [Program] -> (GlobalSymTable,[CompileLog])
collectGlobalSyms stable log [] = (stable,log)
collectGlobalSyms stable log (Func fdecl:xs) = let sym@(SFunc fobj) = makeFuncSym fdecl
                                               in if member (fname fobj) stable
                                                  then collectGlobalSyms stable (dupError log (fname fobj)) xs
                                                  else collectGlobalSyms (M.insert (fname fobj) sym stable) log xs

collectGlobalSyms stable log (ExDecl decl@(Decl t ilist):xs) =
  let (ntable,log') = sub stable log ilist
  in collectGlobalSyms ntable log' xs
    where sub t l [] = (t,l)
          sub t l (Identifier x:xs) = if member x t
                                        then sub t (dupError l x) xs
                                        else sub (M.insert x (makeGlobalVarSym Syntax.Types.Int x) t) l xs

makeFuncSym :: FuncDecl -> Symbol
makeFuncSym (FuncDecl tp (Identifier name) (ParamDecl ps) _) =
                SFunc FuncObj { fname = name,
                                 params = snd $ mp ps,
                                 paramType = fst $ mp ps,
                                 returnType = convT tp, 
                                 localVarSize = -1 }
                  where mp [] = ([],[])
                        mp ((t,Identifier nm):xs) = (convT t : fst (mp xs),
                                                     nm : snd (mp xs))

makeGlobalVarSym :: Type -> String -> Symbol
makeGlobalVarSym t s = SVar VarObj { vname = s, vadr = 0,
                                     level = 0, dataType = convT t }


type SStack = S.Stack (Integer,String,STKey)

emptyStack :: SStack
emptyStack = S.empty

popCurrentLevel :: Integer -> SStack -> SStack
popCurrentLevel l [] = []
popCurrentLevel l s@((lev,_,_):xs)
  | lev >= l = popCurrentLevel l xs
  | otherwise = s

data CollectSymbolState = CSS { stack :: SStack,
                                stable :: SymbolTable,
                                clog :: [CompileLog],
                                lev :: Integer };

instance Show CollectSymbolState where
  show css = "symbolTable :" ++ show (stable css) ++ "\n compile log:"++ show (clog css) ++ "\n"

initState :: CollectSymbolState
initState = CSS { stack = emptyStack, stable = emptyTable, clog = [], lev = 0}

append :: [a] -> (CollectSymbolState,a) -> (CollectSymbolState,[a])
append l (css,a) = (css,l ++ [a])

modifyLevel :: CollectSymbolState -> Integer -> CollectSymbolState
modifyLevel css l = CSS { stack = popCurrentLevel (l+1) $ stack css, stable = stable css, clog = clog css, lev = l}

addLog :: CollectSymbolState -> CompileLog -> CollectSymbolState
addLog css l = CSS { stable = stable css,
                       stack = stack css,
                       lev = lev css,
                       clog = clog css ++ [l] }

findSymbolinStack :: String -> SStack -> (Integer,Integer)
findSymbolinStack s [] = (-1,-1)
findSymbolinStack s st@((l,x,i):xs) =
  if s == x
  then (l,i)
  else findSymbolinStack s xs

ok :: Integer -> String -> CollectSymbolState -> Maybe CompileLog
ok l s css
  | l == 1 = case findSymbolinStack s $ stack css of
               (-1,-1) -> Nothing
               _  -> Just $ Err (ReDecl s)
  | otherwise = case findSymbolinStack s $ stack css of
                  (1,_)  -> Just $ Warn (ParamShadow s)
                  (x,_) | x == l    -> Just $ Err (ReDecl s)
                        | otherwise -> Nothing

calcAdr :: SymbolTable -> SStack -> Integer -> Integer
calcAdr stable st lev
  | lev == 1  = foldl fp 8 st
  | otherwise = foldl f  (negate 4) st
    where fp osize (l,n,k) = osize + sizeOf (Data.Maybe.fromJust (M.lookup k stable))
          f  osize (l,n,k) | l == 1 = osize
                           | otherwise = osize - sizeOf (Data.Maybe.fromJust $ M.lookup k stable)

insertSymbol :: Type -> Identifier -> CollectSymbolState -> (CollectSymbolState,Bool)
insertSymbol t (Identifier s) css =
  let sym = SVar VarObj { vname = s, dataType = convT t, level = lev css, vadr = calcAdr (stable css) (stack css) (lev css)}
  in let ntable = appendSymbol sym (stable css)
     in case ok (lev css) s css of
       Nothing -> (CSS { stack = push (stack css) (lev css,s,toInteger (M.size ntable) - 1),
                        clog = clog css,
                        lev = lev css,
                        stable = ntable},
                   True)
       Just x  -> (CSS { stack = push (stack css) (lev css,s,toInteger (M.size ntable) - 1),
                         clog = clog css ++ [x],
                         lev = lev css,
                         stable = ntable},
                   True)

----- toplevel
createSymbolTable :: GlobalSymTable -> CTranslUnit -> (([(String,SymbolTable)],[CompileLog]),CTranslUnit)
createSymbolTable gtable (CTU plist) = let (css, pl) = foldl' collect (([],[]),[]) plist
                                       in (css, CTU pl)
                                         where collect (state,program) ed@(ExDecl _) = (state,program++[ed])
                                               collect ((ss,cl),program) (Func fdecl@(FuncDecl _ (Identifier s) _ _)) =
                                                 let (state',fdecl') = collectSymbol gtable initState fdecl
                                                 in ((ss ++ [(s,stable state')],cl ++ clog state'),program ++ [Func fdecl'])

-----
class CreatingSymTable a where
  collectSymbol :: GlobalSymTable -> CollectSymbolState -> a -> (CollectSymbolState,a)

instance CreatingSymTable FuncDecl where
  collectSymbol gtable css (FuncDecl t i param body) =
    let (css',param') = collectSymbol gtable (modifyLevel css 1) param
    in let (css'',body') = collectSymbol gtable (modifyLevel css' 2) body
       in (CSS { stack = emptyStack, stable = stable css'', clog = clog css'', lev = 0 }, FuncDecl t i param' body')

instance CreatingSymTable ParamDecl where
  collectSymbol gtable css (ParamDecl pdec) =
    let (css',pdec') = foldl' f (css,[]) pdec
    in (CSS { stack = stack css', stable = stable css', clog = clog css', lev = 0},ParamDecl pdec')
      where f (state,dl) p@(t,i@(Identifier s)) =
              let (state', success) = insertSymbol t i state
              in if success
                   then (state',dl ++ [(t,STableKey (toInteger (M.size $ stable state') - 1))])
                   else (state',dl ++ [p])

instance CreatingSymTable FuncBody where
  collectSymbol gtable css (Body slist) =
    let (css',slist') = foldl' collect (css,[]) slist
    in (CSS { stack = emptyStack, stable = stable css', clog = clog css', lev = 0}, Body slist')
      where collect (state,sl) s = let (state',s') = collectSymbol gtable state s
                                   in (state',sl ++ [s'])

instance CreatingSymTable Stmt where
  collectSymbol gtable css EmptyStmt = (css,EmptyStmt)
  collectSymbol gtable css (Return Nothing) = (css,Return Nothing)
  collectSymbol gtable css (Return (Just x)) =
    let (css',expr) = collectSymbol gtable css x
    in (css',Return $ Just expr)

  collectSymbol gtable css (Expression expr) =
    let (css',expr') = collectSymbol gtable css expr
    in (css', Expression expr')

  collectSymbol gtable css (If expr stmt1 stmt2) = -- trace ("stack in if : " ++ (show $ stack css)) $
    let (css', expr') = collectSymbol gtable css expr
    in let (css'', stmt1') = collectSymbol gtable (modifyLevel css' (lev css + 1)) stmt1
       in let (css''', stmt2') = collectSymbol gtable (modifyLevel css'' (lev css + 1)) stmt2
          in (css''',If expr' stmt1' stmt2')

  collectSymbol gtable css (While expr stmt) =
    let (css', expr') = collectSymbol gtable css expr
    in let (css'', stmt') = collectSymbol gtable (modifyLevel css' (lev css + 1)) stmt
       in (css'', While expr' stmt')

  collectSymbol gtable css (Compound sl) =
    let (css',sl') = foldl' f (css,[]) sl -- (Compound [Stmt])
    in (css',Compound sl')
      where f (state,l) stmt =
              let (state',stmt') = collectSymbol gtable state stmt
              in (modifyLevel state' $ lev css,l ++ [stmt'])

  collectSymbol gtable css (Declaration decl) =
    let (css',decl') = collectSymbol gtable css decl
    in (css',Declaration decl')

instance CreatingSymTable Decl where
  collectSymbol gtable css d@(Decl t il) =
    let (css',il') = foldl' f (css,[]) il
    in (css',Decl t il')
      where f (state,ilist) i@(Identifier s) =
              let (state',success) = insertSymbol t i state
              in if success
                 then (state',ilist ++ [STableKey (toInteger (M.size $ stable state') - 1)])
                 else (state',ilist ++ [i])

instance CreatingSymTable Expr where
  collectSymbol gtable css c@(Const _)  = (css,c)

  collectSymbol gtable css (UMinus expr) =
    let (css',expr') = collectSymbol gtable css expr
    in (css',UMinus expr')

  collectSymbol gtable css e@(Ident (Identifier s)) =
    case findSymbolinStack s (stack css) of
      (-1,-1) -> case M.lookup s gtable of
                   Nothing  -> (addLog css $ Err $ UndeclVar s,e)
                   Just sym -> (css,e)
      (l,k)   -> trace "found" $ (css,Ident (STableKey k))

  collectSymbol gtable css f@(FunCall (Identifier s) p) =
    case M.lookup s gtable of
      Nothing -> let (css',pl) = foldl aux (css,[]) p
                 in (addLog css $ Warn $ CallUndefinedFunc s,FunCall (Identifier s) pl)
      Just (SVar vobj) -> (addLog css $ Err $ FunCallWithVar s,f)
      Just sym@(SFunc fobj) | checkParam css fobj p -> let (css',pl) = foldl aux (css,[]) p
                                                       in (css',FunCall (Identifier s) pl)
                            | otherwise -> (addLog css $ Err $ InvalidNumOfParam (toInteger $ length $ params fobj) (toInteger $ length p),f)
      where aux (state,plist) p = let (state',p') = collectSymbol gtable state p
                                  in (state',plist ++ [p'])
            checkParam css fobj p = length (params fobj) == length p

  collectSymbol gtable css e@(Assign id@(Identifier s) expr)=
    let (css',expr') = collectSymbol gtable css expr
    in case findSymbolinStack s (stack css) of
         (-1,-1) -> case M.lookup s gtable of
                      Nothing  -> (addLog css' $ Err $ UndeclVar s,Assign id expr')
                      Just sym -> (css',Assign id expr')
         (l,k)   -> (css',Assign (STableKey k) expr')

  collectSymbol gtable css (ExprList e1 e2) = collectSymbolAux gtable css ExprList e1 e2
  collectSymbol gtable css (L_OR e1 e2)     = collectSymbolAux gtable css L_OR e1 e2
  collectSymbol gtable css (L_AND e1 e2)    = collectSymbolAux gtable css L_AND e1 e2
  collectSymbol gtable css (Equal e1 e2)    = collectSymbolAux gtable css Equal e1 e2
  collectSymbol gtable css (NEqual e1 e2)   = collectSymbolAux gtable css NEqual e1 e2
  collectSymbol gtable css (Gt e1 e2)       = collectSymbolAux gtable css Gt e1 e2
  collectSymbol gtable css (Lt e1 e2)       = collectSymbolAux gtable css Lt e1 e2
  collectSymbol gtable css (Ge e1 e2)       = collectSymbolAux gtable css Ge e1 e2
  collectSymbol gtable css (Le e1 e2)       = collectSymbolAux gtable css Le e1 e2
  collectSymbol gtable css (Plus e1 e2)     = collectSymbolAux gtable css Plus e1 e2
  collectSymbol gtable css (Minus e1 e2)    = collectSymbolAux gtable css Minus e1 e2
  collectSymbol gtable css (Mul e1 e2)      = collectSymbolAux gtable css Mul e1 e2
  collectSymbol gtable css (Div e1 e2)      = collectSymbolAux gtable css Div e1 e2

collectSymbolAux :: GlobalSymTable -> CollectSymbolState -> (Expr -> Expr -> Expr) -> Expr -> Expr -> (CollectSymbolState,Expr)
collectSymbolAux gtable css f e1 e2 =
  let (css',e1') = collectSymbol gtable css e1
  in let (css'', e2') = collectSymbol gtable css' e2
     in (css'', f e1' e2')


-- calculate total size of variable in each function
calcVarSize :: GlobalSymTable -> CTranslUnit -> [(String,SymbolTable)] -> GlobalSymTable
calcVarSize gtable (CTU plist) slist = foldl f gtable plist
  where f gt e@(ExDecl _) = gt
        f gt f@(Func (FuncDecl t (Identifier i) _ _)) =
                   let (Just (SFunc sym)) = M.lookup i gt
                   in M.insert i (SFunc FuncObj { fname = fname sym,
                                                  params = params sym,
                                                  paramType = paramType sym,
                                                  returnType = returnType sym,
                                                  localVarSize = M.fold aux 0 (snd . head $ Data.List.filter
                                                                               (\(name,table) -> name == fname sym) slist) })
                      gtable
                        where aux sym osize = osize + sizeOf sym

