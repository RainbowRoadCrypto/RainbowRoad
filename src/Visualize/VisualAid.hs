{-# LANGUAGE OverloadedStrings #-}

module Visualize.VisualAid where

import Data.Text (Text)
import qualified Data.Text as T
import Types.Types (ASTNode(..), LiteralType(..), OperationType(..), FunctionName(..), VariableName(..), ASTParameter(..))

-- Helper function to convert an OperationType to its textual representation
visualizeOp :: OperationType -> Text
visualizeOp op = case op of
    OpArithmetic "+" -> "+"
    OpArithmetic "-" -> "-"
    OpArithmetic "*" -> "*"
    OpArithmetic "/" -> "/"
    OpLogical "&&" -> "&&"
    OpLogical "||" -> "||"
    OpComparison "==" -> "=="
    OpComparison "!=" -> "!="
    OpComparison ">" -> ">"
    OpComparison ">=" -> ">="
    OpComparison "<" -> "<"
    OpComparison "<=" -> "<="
    -- Add other operation cases as needed
    _ -> "Unsupported operation"

-- Function to visualize AST structure
visualizeAST :: ASTNode -> Text
visualizeAST ast = case ast of
    FunctionNode (FunctionName name) params body ->
        "Function: " <> name <> "(" <> T.intercalate ", " (map (\(ASTParameter (VariableName paramName) _) -> paramName) params) <> ") {" <> T.intercalate ", " (map visualizeAST body) <> "}"
    VariableDecl (VariableName name) _ expr ->
        "Var: " <> name <> " = " <> maybe "undefined" visualizeAST expr
    OperationNode op args ->
        T.intercalate " " (map visualizeAST args) <> " " <> visualizeOp op
    LiteralNode lit ->
        "Literal: " <> visualizeLiteral lit
    -- Add pattern matches for other constructors as needed
    _ -> "Unsupported AST node"

-- Helper function to visualize literals
visualizeLiteral :: LiteralType -> Text
visualizeLiteral lit = case lit of
    LString str -> "\"" <> str <> "\""
    LNumber num -> T.pack (show num)
    LBoolean bool -> T.pack (show bool)
    -- Handle other literals as needed

-- Example usage of visualization functions
exampleUsage :: IO ()
exampleUsage = do
    let ast = OperationNode (OpArithmetic "+") [LiteralNode (LNumber 1), LiteralNode (LNumber 2)]
    putStrLn $ T.unpack $ visualizeAST ast
