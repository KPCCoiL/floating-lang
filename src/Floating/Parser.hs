{-# LANGUAGE StrictData #-}
module Floating.Parser where

import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.Parsec.Language

data ExprAST = Literal Integer
             | Current
             | Negate ExprAST
             | Power ExprAST ExprAST
             | Mul ExprAST ExprAST
             | Div ExprAST ExprAST
             | Add ExprAST ExprAST
             | Sub ExprAST ExprAST

lexer = Token.makeTokenParser emptyDef
intLit = Token.decimal lexer
inParens = Token.parens lexer

type ExprParser = Parsec String () ExprAST
type ExprOperator = Operator String () Identity ExprAST

codeLine :: ExprParser
codeLine = expression <* eof

expression :: ExprParser
expression = buildExpressionParser operatorTable singleton
            <?> "whole expression"

singleton :: ExprParser
singleton = spaces *> (inParens expression <|> currentNum <|> literal) <* spaces

literal :: ExprParser
literal = Literal <$> intLit <?> "integer literal"

currentNum :: ExprParser
currentNum = do
  char '$' <?> "$ literal"
  return Current

operatorTable :: [[ExprOperator]]
operatorTable = [ [ prefix "-" Negate, prefix "+" id ]
                , [ binary "^" Power AssocRight ]
                , [ binary "*" Mul AssocLeft, binary "/" Div AssocLeft ]
                , [ binary "+" Add AssocLeft, binary "-" Sub AssocLeft ]
                ]

binary :: String -> (ExprAST -> ExprAST -> ExprAST) -> Assoc -> ExprOperator
binary op fun assoc = Infix (string op *> return fun) assoc

prefix :: String -> (ExprAST -> ExprAST) -> ExprOperator
prefix op fun = Prefix $ string op *> return fun

parseLine :: Int -> String -> Either ParseError ExprAST
parseLine lineNum line = parse codeLine ("Line " ++ show lineNum) line
