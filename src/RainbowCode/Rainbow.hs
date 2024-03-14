{-# LANGUAGE OverloadedStrings #-}

module RainbowCode.Rainbow 
  ( convertCharacterMap
  , convertOpCodeMap
  , combinedEncoding
  , reverseCombinedEncoding
  , encodeToRainbowCode
  , decodeFromRainbowCode
  , RainbowType(..)
  )
  where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Encoding.CharacterEncoding as CharacterEncoding
import qualified Encoding.ProgLangEncoding as ProgLangEncoding
import qualified Encoding.SmartContractEncoding as SmartContractEncoding
import qualified Encoding.OpCodeEncoding as OpCodeEncoding

data RainbowType = IntType | StringType | BoolType deriving (Show, Eq)

-- Adjust convertCharacterMap to use encodeCharacter for each character conversion
convertCharacterMap :: Map Char Text -> Map Text Text
convertCharacterMap = Map.foldrWithKey convertCharacter Map.empty
  where
    convertCharacter k v acc = case CharacterEncoding.encodeCharacter k of
      Just encodedChar -> Map.insert (T.pack [k]) v acc
      Nothing -> acc

convertOpCodeMap :: Map Text Text -> Map Text Text
convertOpCodeMap = Map.foldrWithKey convertOpCode Map.empty
  where
    convertOpCode k v acc = case OpCodeEncoding.getOpCodeHexCode k of
      Just encodedOpCode -> Map.insert encodedOpCode v acc
      Nothing -> acc

-- Using available encoding functions directly for combinedEncoding, without direct access to maps
combinedEncoding :: Map Text Text
combinedEncoding = Map.unions [
    convertCharacterMap $ Map.fromList [(c, T.pack [c]) | c <- ['A'..'Z']], -- Generate a map from characters to themselves
    ProgLangEncoding.progLangEncoding,
    SmartContractEncoding.encodeSmartContractColors,
    convertOpCodeMap OpCodeEncoding.encodeOpcodeColors
    ]

reverseCombinedEncoding :: Map Text Text
reverseCombinedEncoding = Map.foldrWithKey (flip Map.insert) Map.empty combinedEncoding

encodeToRainbowCode :: Text -> Text
encodeToRainbowCode = T.concatMap (\c -> fromMaybe "#FFFFFF" $ Map.lookup (T.singleton c) combinedEncoding)

decodeFromRainbowCode :: Text -> Text
decodeFromRainbowCode encodedText = T.concat $ mapMaybe (`Map.lookup` reverseCombinedEncoding) (T.splitOn " " encodedText)

exampleIntegrationUsage :: IO ()
exampleIntegrationUsage = do
    let originalText = "Hello, world!"
    let encodedText = encodeToRainbowCode originalText
    putStrLn $ "Original: " ++ T.unpack originalText ++ ", Encoded to Rainbow Code: " ++ T.unpack encodedText
    let decodedText = decodeFromRainbowCode encodedText
    putStrLn $ "Decoded from Rainbow Code: " ++ T.unpack decodedText
