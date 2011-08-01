module Main where

import Text.Parsec.String
import Text.Parsec.Error (ParseError)
import System
import Debug.Trace
import Syntax.AST (CTranslUnit)
import Symbol
import Parser (translUnit)
import SemanticChecker (createGlobalSTable, createSymbolTable)
import CompileError
import CodeGen.CodeGenerator (topLevelCodeGeneration)
import Data.Map as M

main :: IO ()
main = do args <- getArgs
          ast <- parseFromFile translUnit (head args)
          case ast of
            Left err -> print err
            Right ctu -> do sres <- semanticCheck ctu
                            case sres of
                              Left err -> print err
                              Right (cl,gst,sl,ctl) -> do putStrLn $ concatMap show (topLevelCodeGeneration cl sl gst ctl)
                                                          trace (unlines $ Prelude.map show cl) $ return ()

semanticCheck :: CTranslUnit -> IO (Either [CompileLog] ([CompileLog],GlobalSymTable,[(String,SymbolTable)],CTranslUnit))
semanticCheck ctu = do let (gtable,log) = createGlobalSTable ctu
                       case log of
                         []  -> do let ((sl,cl),ctl) = createSymbolTable gtable ctu
                                   if containError cl
                                     then return (Left cl)
                                     else return (Right (cl,gtable,sl,ctl))
                         _   -> return (Left log)

containError :: [CompileLog] -> Bool
containError [] = False
containError (x:xs) =
  case x of
    (Err _) -> True
    _       -> containError xs
