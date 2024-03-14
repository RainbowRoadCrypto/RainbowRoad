{-# LANGUAGE OverloadedStrings #-}

module Ast.Processing
  ( astNodeHexMap
  , hexToAstNodeMap
  , reverseMap
  , reversedProgLangEncoding
  , reversedSmartContractEncoding
  , decodeHexCode
  , encodeASTNodeToHex
  , decodeHexToASTNodeType
  ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Types.Types (ASTNode(..), LiteralType(..), FunctionName(..), VariableName(..), TypeName(..))
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

-- Hexadecimal code mappings for AST node types
astNodeHexMap :: Map.Map Text Text
astNodeHexMap = Map.fromList
  [ ("FunctionNode", "#FF5733")
  , ("VariableDecl", "#33FF57")
  , ("ClassNode", "#3357FF")
  , ("LiteralNode", "#FF33A5")
  ]

-- Decoding map from Hex to AST Node type
hexToAstNodeMap :: Map.Map Text Text
hexToAstNodeMap = reverseMap astNodeHexMap

-- Reverse the map for decoding
reverseMap :: Map.Map Text Text -> Map.Map Text Text
reverseMap = Map.fromList . map (\(a, b) -> (b, a)) . Map.toList

-- Define programming language and smart contract encodings
progLangEncoding :: Map.Map Text Text
progLangEncoding = Map.fromList
  [ ("function", "#ff0000")
  , ("var", "#00ff00")
  , ("if", "#0000ff")
  ]

smartContractEncoding :: Map.Map Text Text
smartContractEncoding = Map.fromList
  [ ("contract", "#abcdef")
  , ("event", "#fedcba")
  ]

-- Prepare reversed maps
reversedProgLangEncoding :: Map.Map Text Text
reversedProgLangEncoding = reverseMap progLangEncoding

reversedSmartContractEncoding :: Map.Map Text Text
reversedSmartContractEncoding = reverseMap smartContractEncoding

-- Decode hex code to textual representation
decodeHexCode :: Text -> Maybe Text
decodeHexCode hexCode =
  Map.lookup hexCode hexToAstNodeMap <|>
  Map.lookup hexCode reversedProgLangEncoding <|>
  Map.lookup hexCode reversedSmartContractEncoding

-- Encodes an ASTNode to its corresponding Hex representation
encodeASTNodeToHex :: ASTNode -> Text
encodeASTNodeToHex node = fromMaybe "#Unknown" $ case node of
  FunctionNode{} -> Map.lookup "FunctionNode" astNodeHexMap
  VariableDecl{} -> Map.lookup "VariableDecl" astNodeHexMap
  ClassNode{} -> Map.lookup "ClassNode" astNodeHexMap
  LiteralNode{} -> Map.lookup "LiteralNode" astNodeHexMap
  _ -> Just "Unknown"

-- Decodes a Hex code back to an ASTNode type
decodeHexToASTNodeType :: Text -> Text
decodeHexToASTNodeType hex = fromMaybe "Unknown" $ decodeHexCode hex
