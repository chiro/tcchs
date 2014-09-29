{-# LANGUAGE FlexibleContexts #-}
module Parser(translUnit) where

import Control.Monad.Identity

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)

import Syntax.Types
import Syntax.AST

alpha :: String
alpha = ['a' .. 'z'] ++ ['A' .. 'Z']

alnum :: String
alnum = alpha ++ ['0' .. '9']

tinyCStyle :: P.GenLanguageDef String a Identity
tinyCStyle = javaStyle
             { P.nestedComments = False
             , P.reservedNames = ["if", "else", "while", "int",
                                  "return", "void"]
             , P.reservedOpNames = ["*", "/", "+", "-", ">", "<",
                                    ">=", "=<", "==", "!=", "&&", "||", "="]
             , P.identStart = oneOf alpha
             , P.identLetter = oneOf (alnum ++ "_")
             }

lexer :: P.TokenParser()
lexer = P.makeTokenParser tinyCStyle

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

mNatural :: Parser Integer
mNatural = P.natural lexer

semi :: Parser String
semi = P.semi lexer

mIdentifier :: Parser String
mIdentifier = P.identifier lexer

reserved :: String -> Parser ()
reserved     = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp   = P.reservedOp lexer

brace :: Parser a -> Parser a
brace = between (symbol "(") (symbol ")")

constant :: Parser Constant
constant = do p <- mNatural
              return (Constant p)
           <?> "constant"

identifier :: Parser Identifier
identifier = do name <- mIdentifier
                return (Identifier name)
             <?> "Identifier"

primaryExpr :: Parser Expr
primaryExpr = do (Constant c) <- constant
                 return (Const c)
              <|> do p <- identifier
                     return (Ident p)
              <|> brace expression
              <?> "primary expression"

argumentExpList :: Parser [Expr]
argumentExpList = assignExpr `sepBy` symbol ","
                  <?> "arg_list"

postfixExpr :: Parser Expr
postfixExpr = try ( do q <- identifier
                       p <- brace argumentExpList
                       return (FunCall q p) )
               <|> primaryExpr
               <?> "postfix expression"

unaryExpr :: Parser Expr
unaryExpr = postfixExpr
             <|> do _ <- char '-'
                    p <- unaryExpr
                    return (UMinus p)
             <?> "unary expression"

logicalOrArithExpr :: Parser Expr
logicalOrArithExpr = buildExpressionParser table unaryExpr <?> "expression"

table :: [[Operator String () Identity Expr]]
table = [[op "*"  Mul   AssocLeft , op "/"  Div    AssocLeft]
        ,[op "+"  Plus  AssocLeft , op "-"  Minus  AssocLeft]
        ,[op "<=" Le    AssocLeft , op ">=" Ge     AssocLeft,
          op "<"  Lt    AssocLeft , op ">"  Gt     AssocLeft]
        ,[op "==" Equal AssocLeft , op "!=" NEqual AssocLeft]
        ,[op "&&" L_AND AssocLeft]
        ,[op "||" L_OR  AssocLeft]
        ]
  where op s f = Infix (do{ reservedOp s; return f} <?> "operator")

assignExpr :: Parser Expr
assignExpr = try (do p <- identifier
                     _ <- symbol "="
                     q <- assignExpr
                     return (Assign p q))
              <|> logicalOrArithExpr
              <?> "assign expression"

expression :: Parser Expr
expression = do q <- assignExpr `sepBy1` symbol ","
                return (f q)
  where f (x:[]) = x
        f (x:xs) = ExprList x (f xs)
        f [] = error "Bug!"

statement :: Parser Stmt
statement = (symbol ";" >> return EmptyStmt)
            <|> do reserved "while"
                   predicate <- brace expression
                   st <- statement
                   return (While predicate st)
            <|> do reserved "return"
                   p <- optionMaybe expression
                   _ <- semi
                   return (Return p)
            <|> do { reserved "if";
                    q  <- between (symbol "(") (symbol ")") expression;
                    th <- statement;
                    do { reserved "else";
                         el <- statement;
                         return (If q th el);
                         }
                    <|> return (If q th EmptyStmt);
                    }
            <|> liftM Declaration declaration
            <|> do p <- expression
                   _ <- semi
                   return (Expression p)
            <|> do _ <- symbol "{"
                   p <- many statement
                   _ <- symbol "}"
                   return (Compound p)
            <?> "statement"

declaration :: Parser Decl
declaration = do reserved "int"
                 p <- identifier `sepBy` symbol ","
                 _ <- semi
                 return (Decl Int p)
              <?> "declaration"

paramDeclAux :: Parser Identifier
paramDeclAux = do reserved "int"
                  identifier

paramDecl :: Parser ParamDecl
paramDecl = do pl <- paramDeclAux `sepBy` symbol ","
               return (ParamDecl (map (\p -> (Int,p)) pl))
            <?> "parameter declaration"

functionDefinition :: Parser FuncDecl
functionDefinition = do typ <- typeDecl
                        name <- identifier
                        q <- brace paramDecl
                        stmt <- between (symbol "{") (symbol "}")
                                (many statement)
                        return (FuncDecl typ name q (Body stmt))
                     <?> "function definition"

typeDecl :: Parser Type
typeDecl = (reserved "int" >> return Int)
           <|> (reserved "void" >> return Void)
           <?> "typeDecl"

program :: Parser Program
program = try (do p <- declaration
                  return (ExDecl p) )
          <|> do p <- functionDefinition
                 return (Func p)
          <?> "program"

translUnit :: Parser CTranslUnit
translUnit = do whiteSpace
                pl <- many program
                return (CTU pl)
             <?> "translUnit"
