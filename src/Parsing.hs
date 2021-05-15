module Parsing where

import Language

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import Data.Functor.Identity ( Identity )
import Text.Parsec.String ( Parser )
import Text.Parsec.Expr
import qualified Data.Map as M
import Control.Monad.State as S
import Control.Monad.Identity
import Text.Read ( readMaybe )

style :: GenLanguageDef String u Identity
style = javaStyle
                {
                    P.reservedOpNames = [";", ":=", "=", "+", "-", "*", "/", "!", "&&", "||", "==", "!="]
                    , P.reservedNames = ["def", "while", "print", "input", "do", "skip", "if", "then", "else", "return"]
                    , P.caseSensitive  = True
                }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser style
parens, braces  :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer
braces      = P.braces lexer
identifier :: Parser String
identifier  = P.identifier lexer
reserved :: String -> Parser ()
reserved    = P.reserved lexer
integer :: Parser Integer
integer = P.integer lexer
operator :: Parser String
operator = P.operator lexer
reservedOp :: String -> ParsecT String u Identity ()
reservedOp = P.reservedOp lexer

commaSep, commaSep1, semiSep, semiSep1 :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
semiSep = P.semiSep lexer
semiSep1 = P.semiSep1 lexer

-- | Table lists operators and associativities. 
table = 
    [
        [
            Prefix negative --, Prefix positive
        ] ,
        [
            Prefix logicalNot
        ] ,
        [
            Infix times AssocLeft, Infix divide AssocLeft
        ] ,
        [
            Infix plus AssocLeft, Infix minus AssocLeft
        ] ,
        [
            Infix lt AssocLeft , Infix gt AssocLeft ,
            Infix leq AssocLeft , Infix geq AssocLeft
        ] ,
        [
            Infix equals AssocLeft , Infix notEquals AssocLeft
        ] ,
        [
            Infix logicalAnd AssocLeft
        ] ,
        [
            Infix logicalOr AssocLeft
        ]
    ]

negative, logicalNot :: ParsecT String u Identity (Expression -> Expression)
negative = reservedOp "-" >> return(UnaryOp UnaryMinus)
-- positive = reservedOp "+" >> return id
logicalNot = reservedOp "!" >> return (UnaryOp Not)

plus, minus, times, divide, lt, gt, leq, geq, equals, notEquals :: Parser (Expression -> Expression -> Expression)
plus = reservedOp "+" >> return (BinaryOp Plus)
minus = reservedOp "-" >> return (BinaryOp Minus)
times = reservedOp "*" >> return (BinaryOp Times)
divide = reservedOp "/" >> return (BinaryOp Divide)

lt = reservedOp "<" >> return (BinaryOp Lt)
gt = reservedOp ">" >> return (BinaryOp Gt)
leq = reservedOp "<=" >> return (BinaryOp Leq)
geq = reservedOp ">=" >> return (BinaryOp Geq)
equals = reservedOp "==" >> return (BinaryOp Equals)
notEquals = reservedOp "!=" >> return (BinaryOp NotEquals)

logicalAnd, logicalOr :: Parser (Expression -> Expression -> Expression)
logicalAnd = reservedOp "&&" >> return (BinaryOp LogicalAnd)
logicalOr = reservedOp "||" >> return (BinaryOp LogicalOr)


-- TODO ! Can remove try from function by factorising parser correctly
expression, variable, number, term :: Parser Expression
expression = buildExpressionParser table term
term = parens expression <|> try function <|> variable <|> number
variable = Variable <$> identifier
number = Number <$> integer
function = do
    name <- identifier
    Function name <$> argList

argList :: Parser [Expression]
argList = parens $ sepBy expression (char ',' )

statement = braces block
        <|> ifThenElse
        <|> printer
        <|> assign
        <|> functionAssign
        <|> input 
        <|> whileDo 

block = chainl1 statement semicolon

semicolon :: Parser (Statement -> Statement -> Statement)
semicolon = reservedOp ";" >> return Semicolon

printer :: Parser Statement
printer = reserved "print" >> Print <$> expression

input :: Parser Statement
input = reserved "input" >> Input <$> identifier

functionAssign :: Parser Statement
functionAssign = do
    reserved "def"
    name <- identifier
    args <- parens (commaSep identifier)
    body <- braces block
    return $ FunctionAssignment name args body

whileDo :: Parser Statement 
whileDo = do 
    reserved "while"
    expr <- expression 
    reserved "do" 
    body <- statement 
    return $ WhileDo expr body 

assign :: Parser Statement
assign = do
    name <- identifier
    reservedOp ":="
    expr <- expression
    return $ Assignment name expr

ifThenElse :: Parser Statement
ifThenElse = do
    reserved "if"
    test <- expression
    reserved "then"
    block1 <- block
    reserved "else"
    block2 <- block
    return $ IfThenElse test block1 block2

parseExpr :: String -> Expression
parseExpr input = case parse expression "expr" input of
    Left err -> error (show err)
    Right ast -> ast

parseBlock :: String -> Statement
parseBlock input = case parse block "block" input of
    Left err -> error (show err)
    Right ast -> ast