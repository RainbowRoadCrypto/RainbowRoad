{-# LANGUAGE OverloadedStrings #-}

module Encoding.SmartContractEncoding
    ( SmartContractElement
    , HexCode
    , encodeSmartContractColors
    , hexToSmartContractMap
    , getSmartContractHexCode
    , decodeSmartContractHexCode)
     where

import qualified Data.Map as Map
import Data.Text (Text, unpack, pack)

type SmartContractElement = Text
type HexCode = Text

encodeSmartContractColors :: Map.Map SmartContractElement HexCode
encodeSmartContractColors = Map.fromList
    [ ("function", "#E238EC"), ("event", "#D462FF"), ("modifier", "#C45AEC"), ("mapping", "#BA55D3")
    , ("address", "#A74AC7"), ("public", "#B048B5"), ("private", "#B666D2"), ("internal", "#7E587E")
    , ("external", "#583759"), ("payable", "#4E5180"), ("contract", "#7575CF"), ("emit", "#6667AB")
    , ("require", "#6F2DA8"), ("assert", "#6A0DAD"), ("revert", "#6C2DC7"), ("delegatecall", "#822EFF")
    , ("call", "#5453A6"), ("send", "#4E387E"), ("transfer", "#571B7E"), ("view", "#4B0150")
    , ("pure", "#36013F"), ("storage", "#2E1A47"), ("memory", "#461B7E"), ("constructor", "#4B0082")
    , ("fallback", "#663399"), ("interface", "#6A287E"), ("library", "#8B008B"), ("selfdestruct", "#800080")
    , ("keccak256", "#9932CC"), ("sha256", "#9400D3"), ("ecrecover", "#8D38C9"), ("msg.sender", "#A23BEC")
    , ("msg.value", "#B041FF"), ("block.timestamp", "#842DCE"), ("block.number", "#7A5DC7"), ("tx.origin", "#7F38EC")
    , ("gasleft", "#9D00FF"), ("new", "#8E35EF"), ("super", "#893BFF"), ("solidity", "#9370DB")
    , ("assembly", "#8467D7"), ("immutable", "#9172EC"), ("indexed", "#9E7BFF")
    ]

-- Reverse mapping from HexCode to SmartContractElement for decoding
hexToSmartContractMap :: Map.Map HexCode SmartContractElement
hexToSmartContractMap = Map.fromList [(b, a) | (a, b) <- Map.toList encodeSmartContractColors]

getSmartContractHexCode :: SmartContractElement -> Maybe HexCode
getSmartContractHexCode = flip Map.lookup encodeSmartContractColors

decodeSmartContractHexCode :: HexCode -> Maybe SmartContractElement
decodeSmartContractHexCode = flip Map.lookup hexToSmartContractMap

-- Example usage demonstrating the encoding and decoding functions
exampleUsage :: IO ()
exampleUsage = do
    let elements = ["function", "modifier", "immutable"]
    putStrLn "Encoding Smart Contract Elements to Hex Codes:"
    mapM_ (\e -> putStrLn $ unpack e ++ ": " ++ maybe "Not Found" unpack (getSmartContractHexCode e)) elements

    putStrLn "\nDecoding Hex Codes to Smart Contract Elements:"
    mapM_ (\hex -> putStrLn $ hex ++ ": " ++ maybe "Not Mapped" unpack (decodeSmartContractHexCode (pack hex))) ["#E238EC", "#C45AEC", "#9172EC"]