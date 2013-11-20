module Symbol where

import Syntax.Types
import Data.Map as M

data SType = SVoid
           | SInt
           | SUndefined
           deriving (Show, Eq)

data Kind = KVar
          | KFunc
          | KUndefFunc
          | KParam
          | KFresh
          deriving (Show, Eq)

data VarObj = VarObj {
  vname :: String,
  vadr :: Integer,
  level :: Integer,
  dataType :: SType }
             deriving (Eq, Show)

data FuncObj = FuncObj {
  fname :: String,
  params :: [String],
  paramType :: [SType],
  returnType :: SType }
             deriving (Eq, Show)

data Symbol = SFunc FuncObj
            | SVar VarObj
            deriving (Eq, Show)

type SymbolTable = M.Map Integer Symbol
type STKey = Integer

sizeOf :: Symbol -> Integer
sizeOf (SVar var) = sizeOfType (dataType var)
sizeOf _ = -1

sizeOfType :: SType -> Integer
sizeOfType SInt = 4
sizeOfType SVoid = error "why do you need size of void"
sizeOfType _ = -1

type GlobalSymTable = M.Map String Symbol

appendSymbol :: Symbol -> SymbolTable -> SymbolTable
appendSymbol sym table = M.insert (toInteger $ M.size table) sym table

convT :: Type -> SType
convT Int = SInt
convT Void = SVoid

emptyTable :: SymbolTable
emptyTable = M.empty
