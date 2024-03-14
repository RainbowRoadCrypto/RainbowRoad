{-# LANGUAGE OverloadedStrings #-}

module CrossChain.Interoperability 
    ( decodeHexToChar
    , rainbowCodeToPlaintext
    , exampleConversionProcess
    )
    where

import Encoding.CharacterEncoding (decodeCharacterHexCode)
import Encoding.ProgLangEncoding (decodeProgLangHexCode)
import Encoding.SmartContractEncoding (decodeSmartContractHexCode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>)) -- Importing the required operator

-- Function to decode a single hex code using available decoding functions
decodeHexToChar :: Text -> Maybe Char
decodeHexToChar hexCode =
  decodeCharacterHexCode hexCode <|>
  fmap (const '?') (decodeProgLangHexCode hexCode) <|> -- Assuming a placeholder for non-char results
  fmap (const '?') (decodeSmartContractHexCode hexCode) -- Assuming a placeholder for non-char results

-- Function to decode "Rainbow Code" hex codes back to plaintext
rainbowCodeToPlaintext :: [Text] -> Text
rainbowCodeToPlaintext hexCodes =
  T.pack $ mapMaybe decodeHexToChar hexCodes

-- Example usage demonstrating the conversion process from "Rainbow Code" back to Text
exampleConversionProcess :: [Text] -> IO ()
exampleConversionProcess hexCodes = do
  let decodedText = rainbowCodeToPlaintext hexCodes
  putStrLn $ "Converted Hex Codes: " ++ T.unpack (T.unwords hexCodes)
  putStrLn $ "Decoded back to Text: " ++ T.unpack decodedText
