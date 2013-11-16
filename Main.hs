module Main where

import Data.Map as M
import Debug.Trace
import qualified Text.Parsec.Prim as TPP
import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)


import Syntax.AST (CTranslUnit)
import Symbol
import Parser (translUnit)
import SemanticChecker (createGlobalSTable, createSymbolTable)
import CompileError
import CodeGen.CodeGenerator (topLevelCodeGeneration)
import CodeGen.AsmCode (Code)
import Utils


main :: IO ()
main = do
  args <- getArgs
  file <- readFile (head args)
  let code = parse file >>= semanticCheck >>= compile
  case code of
    Left err -> print err
    Right codes ->
      writeFile (fileNameWithoutExtension (head args) ++ ".asm") $ concatMap show codes

compile :: ([CompileLog],GlobalSymTable,[(String,SymbolTable)],CTranslUnit)
           -> Either CompileError [Code]
compile (cl,gst,sl,ctl) = trace (unlines $ Prelude.map show cl) $
                          Right $ topLevelCodeGeneration cl sl gst ctl

parse :: String -> Either CompileError CTranslUnit
parse s = case TPP.parse translUnit "" s of
  Left err -> Left $ PError err
  Right x -> Right x

semanticCheck :: CTranslUnit ->
                 Either CompileError ([CompileLog],GlobalSymTable,[(String,SymbolTable)],CTranslUnit)
semanticCheck ctu = let (gtable,log) = createGlobalSTable ctu in
  case log of
    []  -> let ((sl,cl),ctl) = createSymbolTable gtable ctu in
      if containError cl
      then Left $ SError cl
      else Right (cl,gtable,sl,ctl)
    _   -> Left $ SError log

containError :: [CompileLog] -> Bool
containError [] = False
containError (x:xs) =
  case x of
    (Err _) -> True
    _       -> containError xs
