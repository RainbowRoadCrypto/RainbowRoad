{-# LANGUAGE OverloadedStrings #-}

module Ast.ASTToRainbowToPLONK 
  ( lexer
  , identifier
  , parens
  , commaSep
  , typeAnnotation
  , statement
  , variableDecl
  , expression
  , literal
  , intLiteral
  , stringLiteral
  , boolLiteral
  , variableAssign
  , functionCall
  , block
  , astStatements
  , ifStatement
  , blockStatement
  , astParser
  , typeToIR
  , typeToRainbow)
  where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, mapMaybe)
import Types.Types as Types
import RainbowCode.Rainbow as RainbowCode
import Encoding.EncodingService as Encoding
import Data.Functor (($>))
import qualified Data.Map.Strict as Map

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef

identifier :: Parser Text
identifier = T.pack <$> Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

typeAnnotation :: Parser Text
typeAnnotation = T.pack <$> (Token.colon lexer *> Token.identifier lexer)

statement :: Parser ASTNode
statement = try variableDecl
        <|> try variableAssign
        <|> try functionCall
        <|> try ifStatement  -- Assuming you have an `ifStatement` parser defined
        <|> blockStatement  -- Assuming `block` should be treated as a statement too
        -- Add other statement types here

-- Completing the ... with missing parsers
variableDecl :: Parser ASTNode
variableDecl = do
  name <- VariableName <$> identifier
  typ <- TypeName <$> typeAnnotation
  initializer <- optionMaybe (Token.symbol lexer "=" *> expression)
  return $ VariableDecl name typ initializer

-- Implementing a generic expression parser
expression :: Parser ASTNode
expression = try literal <|> variableAssign <|> functionCall
  -- Extend with more expressions as needed

literal :: Parser ASTNode
literal = LiteralNode <$> (intLiteral <|> stringLiteral <|> boolLiteral)

intLiteral :: Parser LiteralType
intLiteral = LNumber . fromInteger <$> Token.integer lexer

stringLiteral :: Parser LiteralType
stringLiteral = LString . T.pack <$> Token.stringLiteral lexer

boolLiteral :: Parser LiteralType
boolLiteral = LBoolean <$> (Token.symbol lexer "true" $> True <|> Token.symbol lexer "false" $> False)

variableAssign :: Parser ASTNode
variableAssign = do
  varName <- VariableName <$> identifier
  Token.symbol lexer "="
  VariableAssign varName <$> expression

functionCall :: Parser ASTNode
functionCall = do
  fName <- FunctionName <$> identifier
  args <- parens $ commaSep expression
  return $ FunctionCallNode fName args

block :: Parser [ASTNode]
block = Token.braces lexer (many statement)

astStatements :: Parser [ASTNode]
astStatements = many statement

ifStatement :: Parser ASTNode
ifStatement = do
  Token.symbol lexer "if"
  condition <- parens expression
  thenBlock <- blockStatement
  elseBlock <- optionMaybe (Token.symbol lexer "else" *> blockStatement)
  return $ IfNode condition [thenBlock] ((: []) <$> elseBlock)

blockStatement :: Parser ASTNode
blockStatement = BlockNode <$> block

astParser :: Parser ASTNode
astParser = Token.whiteSpace lexer *> (BlockNode <$> astStatements) <* eof

typeToIR :: TypeName -> Type
typeToIR (TypeName typ) = case T.unpack typ of
  "int" -> TypeInt
  "string" -> TypeString
  "bool" -> TypeBool
  "array" -> TypeArray TypeUnknown  -- Example for an array, assuming a more complex type analysis is needed
  "object" -> TypeObject []  -- Example for an object, which would require property type analysis
  _ -> TypeUnknown

typeToRainbow :: Type -> RainbowCode.RainbowType
typeToRainbow typ = case typ of
  TypeInt -> RainbowCode.IntType
  TypeString -> RainbowCode.StringType
  TypeBool -> RainbowCode.BoolType
  -- Handle complex types or fallback to a default type, if necessary.
  -- This could involve logging a warning or throwing an error for unsupported types.
  TypeArray _ -> RainbowCode.IntType  -- Example fallback
  TypeObject _ -> RainbowCode.StringType  -- Example fallback
  _ -> RainbowCode.IntType  -- Assuming IntType as a generic fallback


