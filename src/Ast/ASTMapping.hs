{-# LANGUAGE OverloadedStrings #-}

module Ast.ASTMapping
    ( CommonAST(..)
    , mapToCommonAST
    , commonASTToPlaintext
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Types.Types (ASTNode(..), LoopType(..), OperationType(..), LiteralType(..), VariableName(..), FunctionName(..), TypeName(..))

data CommonAST
    = CommonFunction Text [CommonAST]
    | CommonVariable Text CommonAST
    | CommonReturn CommonAST
    | CommonIf CommonAST [CommonAST] (Maybe [CommonAST])
    | CommonLoop LoopType CommonAST [CommonAST]
    | CommonCall Text [CommonAST]
    | CommonOperation OperationType [CommonAST]
    | CommonLiteral LiteralType
    | CommonAssignment Text CommonAST
    | CommonBlock [CommonAST]
    | CommonComment Text
    | CommonClass Text [CommonAST]
    | CommonMethod Text [CommonAST]
    | CommonConstructor [CommonAST]
    | CommonProperty Text CommonAST
    | CommonImport Text (Maybe Text)
    | CommonExport Text
    deriving (Show, Eq)

mapToCommonAST :: ASTNode -> CommonAST
mapToCommonAST astNode = case astNode of
    FunctionNode (FunctionName name) _ body ->
        CommonFunction name (map mapToCommonAST body)
    VariableDecl (VariableName name) _ maybeExpr ->
        CommonVariable name (maybe (CommonLiteral LNull) mapToCommonAST maybeExpr)
    VariableAssign (VariableName name) expr ->
        CommonAssignment name (mapToCommonAST expr)
    OperationNode op args ->
        CommonOperation op (map mapToCommonAST args)
    LiteralNode lit ->
        CommonLiteral lit
    IfNode cond thenBranch maybeElseBranch ->
        CommonIf (mapToCommonAST cond) (map mapToCommonAST thenBranch) (fmap (map mapToCommonAST) maybeElseBranch)
    WhileNode loopType cond body ->
        CommonLoop loopType (mapToCommonAST cond) (map mapToCommonAST body)
    BlockNode statements ->
        CommonBlock (map mapToCommonAST statements)
    FunctionCallNode (FunctionName name) args ->
        CommonCall name (map mapToCommonAST args)
    ClassNode (TypeName name) body ->
        CommonClass name (map mapToCommonAST body)
    MethodNode (FunctionName name) _ body ->
        CommonMethod name (map mapToCommonAST body)
    ReturnNode maybeExpr ->
        CommonReturn (maybe (CommonLiteral LNull) mapToCommonAST maybeExpr)
    CommentNode text ->
        CommonComment text

-- Assuming commonASTToPlaintext uses the go helper function for recursion:
commonASTToPlaintext :: CommonAST -> Text
commonASTToPlaintext = go 0 where
  go level ast = case ast of
    -- Other cases omitted for brevity
    CommonLoop loopType cond body ->
      indent level <> displayLoopType loopType <> " (" <> go level cond <> ") {\n" <>
      T.intercalate "\n" (map (go (level + 1)) body) <> "\n" <>
      indent level <> "}"
    CommonOperation op args ->
      indent level <> "(" <> T.intercalate (" " <> displayOperationType op <> " ") (map (go level) args) <> ")"
    CommonLiteral lit ->
      indent level <> displayLiteralType lit
    -- Update other cases as necessary

  indent = indentation

displayLoopType :: LoopType -> Text
displayLoopType loopType = case loopType of
  ForLoop -> "for"
  WhileLoop -> "while"
  DoWhileLoop -> "do-while"

displayOperationType :: OperationType -> Text
displayOperationType opType = case opType of
  OpArithmetic op -> op
  OpLogical op -> op
  OpComparison op -> op
  OpBitwise op -> op

displayLiteralType :: LiteralType -> Text
displayLiteralType litType = case litType of
  LString s -> "\"" <> s <> "\""
  LNumber n -> T.pack $ show n
  LBoolean b -> if b then "true" else "false"
  LNull -> "null"
  LTuple ts -> "(" <> T.intercalate ", " (map displayLiteralType ts) <> ")"
  LArray as -> "[" <> T.intercalate ", " (map displayLiteralType as) <> "]"
  LObject os -> "{" <> T.intercalate ", " (map (\(k,v) -> k <> ": " <> displayLiteralType v) os) <> "}"

indentation :: Int -> Text
indentation level = T.replicate (level * 2) " "