module CodeGen.CompilationState where

import Symbol

data CompilationState = CompState { topAlloc :: Integer,
                                    table    :: SymbolTable,
                                    maxAlloc :: Integer,
                                    countLabel :: Integer,
                                    allocList :: [SType],
                                    functionName :: String }
                      deriving (Show,Eq)

emptyState :: CompilationState
emptyState = CompState {
  topAlloc = 0,
  table = emptyTable,
  maxAlloc = 0,
  countLabel = 0,
  allocList = [],
  functionName = "" }

allocateLoc :: SType -> CompilationState -> CompilationState
allocateLoc t state = CompState {
  maxAlloc   = max (maxAlloc state) (sizeOfType t + topAlloc state),
  table      = table state,
  topAlloc   = sizeOfType t + topAlloc state,
  countLabel = countLabel state,
  allocList  = t : allocList state,
  functionName = functionName state }

releaseLoc  :: CompilationState -> CompilationState
releaseLoc state = CompState {
  maxAlloc   = maxAlloc state,
  table      = table state,
  topAlloc   = topAlloc state - sizeOfType (head $ allocList state),
  countLabel = countLabel state,
  allocList  = tail $ allocList state,
  functionName = functionName state }

modifyLoc  :: CompilationState -> Integer -> CompilationState
modifyLoc state i = CompState {
  maxAlloc   = max (maxAlloc state) i,
  table      = table state,
  topAlloc   = i,
  countLabel = countLabel state,
  allocList  = allocList state,
  functionName = functionName state }

modifyLabel :: Integer -> CompilationState -> CompilationState
modifyLabel l state = CompState {
  maxAlloc = maxAlloc state,
  table    = table state,
  topAlloc = topAlloc state,
  countLabel = l,
  allocList = [],
  functionName = functionName state }

addLabelCount :: Integer -> CompilationState -> CompilationState
addLabelCount i state = modifyLabel (countLabel state + i) state

modifyFuncName :: String -> CompilationState -> CompilationState
modifyFuncName name state = CompState {
  maxAlloc = maxAlloc state,
  table = table state,
  topAlloc = topAlloc state,
  countLabel = countLabel state,
  allocList = allocList state,
  functionName = name }

genLabel :: CompilationState -> Integer -> String
genLabel state i = "L" ++ functionName state ++ show (i + countLabel state)

modifyTable :: SymbolTable -> CompilationState -> CompilationState
modifyTable stable state = CompState {
  maxAlloc = maxAlloc state,
  table = stable,
  topAlloc = topAlloc state,
  countLabel = countLabel state,
  allocList = allocList state,
  functionName = functionName state }

top :: CompilationState -> Integer
top state = negate (topAlloc state)
