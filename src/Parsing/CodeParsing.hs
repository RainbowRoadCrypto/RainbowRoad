{-# LANGUAGE OverloadedStrings #-}

module Parsing.CodeParsing
    where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import qualified Control.Monad.Combinators.Expr as Expr
import Types.Types (IR(..), Operator(..), Literal(..), Parameter(..), Type(..), VariableName(VariableName), FunctionName(FunctionName))


type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

stringLiteral :: Parser String  
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

booleanLiteral :: Parser Bool
booleanLiteral = (True <$ symbol "true") <|> (False <$ symbol "false") 

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check) <?> "identifier"
  where
    p = T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

reservedWords :: [Text]
reservedWords = ["if", "else", "while", "var", "function", "true", "false", "return"]

literal :: Parser IR
literal = IRLiteral <$> (IntLit <$> try (L.signed sc (lexeme L.decimal))
                    <|> StringLit <$> stringLiteral
                    <|> BoolLit <$> booleanLiteral)

operation :: Parser IR
operation = try operationParser <|> literal <|> variableDecl <|> ifStmt <|> whileLoop <|> functionDecl

variableDecl :: Parser IR
variableDecl = do
  _ <- symbol "var" 
  nameText <- identifier  
  let name = VariableName nameText
  _ <- symbol "="
  IRVariableDecl name TypeInt . Just <$> expression

ifStmt :: Parser IR
ifStmt = do
    _ <- symbol "if"
    cond <- expression
    thenBranch <- braces (many expression) 
    elseBranch <- optional (symbol "else" *> braces (many expression))
    return $ IRIf cond thenBranch elseBranch

whileLoop :: Parser IR  
whileLoop = do
    _ <- symbol "while"
    cond <- expression
    body <- braces (many expression)
    return $ IRWhile cond body

functionDecl :: Parser IR
functionDecl = do
  _ <- symbol "function"
  nameText <- identifier  
  let name = FunctionName nameText -- Correctly wrap the Text in the FunctionName newtype
  paramsText <- parens (sepBy identifier (symbol ","))   
  let params = map VariableName paramsText -- Wrap each identifier Text in VariableName
  body <- braces (many expression)
  let parameters = map (`Parameter` TypeInt) params
  return $ IRFunction name parameters body

expression :: Parser IR
expression = try operation <|> literal <|> variableDecl <|> ifStmt <|> whileLoop <|> functionDecl

term :: Parser IR
term = parens expression <|> literal <|> variableDecl

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

operationParser :: Parser IR
operationParser = Expr.makeExprParser term operatorTable

operatorTable :: [[Expr.Operator Parser IR]]
operatorTable =
    [ [ Expr.Prefix (do{ op <- OpSubtract <$ symbol "-"; return $ \operand -> IROperation op [operand] }) ]
    , [ Expr.InfixL (do{ op <- OpMultiply <$ symbol "*"; return $ \left right -> IROperation op [left, right] })
      , Expr.InfixL (do{ op <- OpDivide <$ symbol "/"; return $ \left right -> IROperation op [left, right] }) ]
    , [ Expr.InfixL (do{ op <- OpAdd <$ symbol "+"; return $ \left right -> IROperation op [left, right] })
      , Expr.InfixL (do{ op <- OpSubtract <$ symbol "-"; return $ \left right -> IROperation op [left, right] }) ]
    , [ Expr.InfixN (do{ op <- OpEqual <$ symbol "=="; return $ \left right -> IROperation op [left, right] })
      , Expr.InfixN (do{ op <- OpNotEqual <$ symbol "!="; return $ \left right -> IROperation op [left, right] })
      , Expr.InfixN (do{ op <- OpGreaterThan <$ symbol ">"; return $ \left right -> IROperation op [left, right] })
      , Expr.InfixN (do{ op <- OpLessThan <$ symbol "<"; return $ \left right -> IROperation op [left, right] }) ]
    , [ Expr.InfixN (do{ op <- OpAnd <$ symbol "&&"; return $ \left right -> IROperation op [left, right] }) ]
    , [ Expr.InfixN (do{ op <- OpOr <$ symbol "||"; return $ \left right -> IROperation op [left, right] }) ]
    ]