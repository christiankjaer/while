module Parser where

import CFG

-- Almost just the while language from the parsec haskell tutorial

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
    emptyDef
        { Token.identStart = letter
        , Token.identLetter = alphaNum
        , Token.reservedNames = [ "if"
                                , "then"
                                , "else"
                                , "while"
                                , "do"
                                , "skip"
                                ]
        , Token.reservedOpNames = [":=", "+", "-", "*", "<", "=", "!"]
        }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

parseString :: String -> Stmt
parseString str =
    case parse whileParser "" str of
      Left e -> error $ show e
      Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement = parens statement
    <|> sequenceOfStmt

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = do
    list <- sepBy1 statement' semi
    let f [] = BasicStmt Nop
        f [s] = s
        f (s:ss) = SeqStmt s (f ss)
    return $ f list

statement' :: Parser Stmt
statement' = ifStmt <|> whileStmt <|> skipStmt <|> assignStmt

ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond <- expr
    reserved "then"
    pos <- statement
    reserved "else"
    neg <- statement
    return $ IfStmt cond pos neg

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    e <- expr
    reserved "do"
    s <- statement
    return $ WhileStmt e s

assignStmt :: Parser Stmt
assignStmt = do
    var <- identifier
    reservedOp ":="
    e <- expr
    return $ BasicStmt (Assign var e)

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return (BasicStmt Nop)

expr :: Parser Exp
expr = buildExpressionParser operators term

operators = [ [ Prefix (reservedOp "!" >> return Not) ]
            , [ Infix (reservedOp "*" >> return Times) AssocLeft ]
            , [ Infix (reservedOp "+" >> return Plus) AssocLeft
              , Infix (reservedOp "-" >> return Minus) AssocLeft
              ]
            , [ Infix (reservedOp "<" >> return Less) AssocNone
              , Infix (reservedOp "=" >> return Equal) AssocNone
              ]
            ]

term = parens expr <|> fmap VarExp identifier <|> fmap Lit integer
