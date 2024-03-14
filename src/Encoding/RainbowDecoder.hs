{-# LANGUAGE OverloadedStrings #-}

module Encoding.RainbowDecoder 
    ( decodeFromRainbowCode
    , decodeCharacterHexCode
    , decodeHexCode
    , parseIR
    , parseLine
    , interpretType
    , plonkOutputToIR
    )
    where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, catMaybes)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Encoding.CharacterEncoding as CharEnc
import Encoding.CharacterEncoding (HexCode, Character)
import qualified Encoding.OpCodeEncoding as OpCodeEnc
import qualified Encoding.ProgLangEncoding as ProgLangEnc
import qualified Encoding.SmartContractEncoding as SmartContractEnc
import Types.Types (IR(..), Type(..), Literal(..), VariableName(..))

-- Decodes a string of color codes (separated by spaces) back to text
decodeFromRainbowCode :: Text -> Maybe Text
decodeFromRainbowCode encoded = do
    let hexCodes = T.splitOn " " encoded
    decodedChars <- mapM decodeHexCode hexCodes
    return $ T.concat decodedChars

decodeCharacterHexCode :: HexCode -> Maybe Character
decodeCharacterHexCode = CharEnc.decodeCharacterHexCode

decodeHexCode :: Text -> Maybe Text
decodeHexCode hexCode = do
    let decodeCharacterToText = fmap T.singleton . CharEnc.decodeCharacterHexCode
    let decoders = [ decodeCharacterToText
                   , ProgLangEnc.decodeProgLangHexCode
                   , SmartContractEnc.decodeSmartContractHexCode
                   , OpCodeEnc.decodeOpCodeHexCode
                   ]
    let decodedOptions = mapMaybe ($ hexCode) decoders
    return $ fromMaybe "<unknown>" $ listToMaybe decodedOptions

parseIR :: Text -> Maybe IR
parseIR text = if T.null text then Nothing else Just $ IRBlock $ mapMaybe parseLine $ T.lines text

parseLine :: Text -> Maybe IR
parseLine line =
    case T.words line of
        ["var", varNameText, varType] -> 
            let varName = VariableName varNameText  -- Correctly wrap the Text in VariableName
            in Just $ IRVariableDecl varName (interpretType varType) Nothing
        -- Extend this pattern matching for other IR constructors as needed
        _ -> Nothing

interpretType :: Text -> Type  
interpretType t = case t of
    "Int" -> TypeInt
    "String" -> TypeString
    "Bool" -> TypeBool
    _ -> error $ "Unknown type: " <> T.unpack t

plonkOutputToIR :: C8.ByteString -> Maybe IR
plonkOutputToIR output = do
    let encodedText = T.pack $ C8.unpack output
    decodedText <- decodeFromRainbowCode encodedText
    parseIR decodedText
