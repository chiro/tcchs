module Main where

import Text.Parsec.String
import Text.Parsec.Error (ParseError)
import System
import Debug.Trace
import Syntax.AST (CTranslUnit)
import Syntax.ShowWithSymbol
import Symbol
import Parser
import SemanticChecker
import CompileError
import CodeGen.CodeGenerator
import CodeGen.AsmCode
import Data.Map as M

main :: IO ()
main = do args <- getArgs
          ast <- parseFromFile translUnit (head args)
          sres <- semanticCheck ast
          return ()

semanticCheck :: Either ParseError CTranslUnit -> IO (Maybe (GlobalSymTable,
                                                             [(String,SymbolTable)],
                                                             CTranslUnit))
semanticCheck (Left err) = print err >> return Nothing
semanticCheck (Right ctu) = do let (gtable,log) = createGlobalSTable ctu
                               case log of
                                 []  -> createSymbols gtable ctu
                                 _   -> putStrLn "compile errors" >> print log >> return Nothing

createSymbols :: GlobalSymTable -> CTranslUnit -> IO (Maybe (GlobalSymTable,
                                                             [(String,SymbolTable)],
                                                             CTranslUnit))
createSymbols gtable ctu = do
                              let ((sl,cl),ctl) = createSymbolTable gtable ctu
                              if containError cl
                                then do trace (unlines $ Prelude.map show cl) $ return Nothing
                                else do -- putStrLn (showCTU gtable sl ctl)
                                        let code = showCodes (topLevelCodeGeneration cl sl gtable ctl)
                                        putStrLn code
                                        trace (unlines $ Prelude.map show cl) $ return Nothing
                                        return $ Just (gtable,sl,ctl)

containError :: [CompileLog] -> Bool
containError [] = False
containError (x:xs) =
  case x of
    (Err _) -> True
    _       -> containError xs

showTable :: [(String,SymbolTable)] -> String
showTable [] = ""
showTable ((x,table):xs) = x ++ " : " ++ show table ++ "\n"
                           ++ showTable xs

showGlobalSym :: [(String,Symbol)] -> String
showGlobalSym [] = ""
showGlobalSym ((s,sym):xs) = "name: " ++ s ++ " " ++ show sym ++ "\n"
                             ++ showGlobalSym xs