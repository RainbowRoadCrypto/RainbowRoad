{-# LANGUAGE OverloadedStrings #-}

module Encoding.ProgLangEncoding
    ( getProgLangHexCode
    , progLangEncoding
    , hexToProgLangMap
    , decodeProgLangHexCode
    , ProgLangElement
    , HexCode
    ) where

import qualified Data.Map as Map
import Data.Text (Text, unpack)

type ProgLangElement = Text
type HexCode = Text

progLangEncoding :: Map.Map ProgLangElement HexCode
progLangEncoding = Map.fromList
    [ ("async", "#004225"), ("await", "#026C3D"), ("bool", "#437C17")
    , ("break", "#347C17"), ("byte", "#6AA121"), ("bytestring", "#8A9A5B")
    , ("case", "#3F9B0B"), ("switch", "#4AA02C"), ("chan", "#41A317")
    , ("class", "#12AD2B"), ("const", "#3EA055"), ("continue", "#73A16C")
    , ("contract", "#6CBB3C"), ("default", "#6CC417"), ("defer", "#4CC417")
    , ("do", "#32CD32"), ("while", "#52D017"), ("enum", "#4CC552")
    , ("export", "#54C571"), ("import", "#89C35C"), ("fn", "#85BB65")
    , ("function", "#99C68E"), ("for", "#A0D6B4"), ("func", "#8FBC8F")
    , ("get", "#829F82"), ("if", "#A2AD9C"), ("impl", "#B8BC86")
    , ("interface", "#9CB071"), ("let", "#8FB31D"), ("match", "#B0BF1A")
    , ("mod", "#B2C248"), ("new", "#9DC209"), ("null", "#A1C935")
    , ("undefined", "#9ACD32"), ("option", "#77DD77"), ("package", "#7FE817")
    , ("private", "#59E817"), ("public", "#57E964"), ("protected", "#16F529")
    , ("internal", "#5EFB6E"), ("external", "#00FF7F"), ("promise", "#00FF80")
    , ("put", "#36F57F"), ("return", "#00FA9A"), ("select", "#12E193")
    , ("static", "#5FFB17"), ("struct", "#00FF00"), ("trait", "#7CFC00")
    , ("try", "#66FF00"), ("catch", "#7FFF00"), ("finally", "#87F717")
    , ("type", "#98F516"), ("typeof", "#B1FB17"), ("var", "#ADF802")
    , ("void", "#ADFF2F"), ("where", "#BDF516"), ("::", "#BDF5BA")
    , ("->", "#B2F575"), ("=>", "#B2F5A0"), ("<-", "#B2F5B2")
    , (">>=", "#DAEE01"), ("pure", "#E2F516"), ("Monad", "#CCFB5D")
    , ("IO", "#BCE954"), ("<<", "#347C2C"), (">>", "#227442")
    , ("&&", "#64E986"), ("||", "#90EE90"), ("==", "#6AFB92")
    , ("!=", "#98FB98"), (">=", "#98FF98"), ("<=", "#B5EAAA")
    ]

-- Reverse mapping from HexCode to ProgLangElement for decoding
hexToProgLangMap :: Map.Map HexCode ProgLangElement
hexToProgLangMap = Map.fromList [(b, a) | (a, b) <- Map.toList progLangEncoding]

getProgLangHexCode :: ProgLangElement -> Maybe HexCode
getProgLangHexCode = flip Map.lookup progLangEncoding

decodeProgLangHexCode :: HexCode -> Maybe ProgLangElement
decodeProgLangHexCode = flip Map.lookup hexToProgLangMap

-- Example usage demonstrating how to use the encoding and decoding functions
exampleUsage :: IO ()
exampleUsage = do
    let elements = ["function", "var", "async"]
        hexCodes = ["#004225", "#99C68E", "#E2F516"]
    putStrLn "Encoding ProgLang Elements to Hex Codes:"
    mapM_ (\e -> putStrLn $ unpack e ++ ": " ++ maybe "Not Found" unpack (getProgLangHexCode e)) elements
    putStrLn "\nDecoding Hex Codes to ProgLang Elements:"
    mapM_ (\h -> putStrLn $ unpack h ++ ": " ++ maybe "Not Mapped" unpack (decodeProgLangHexCode h)) hexCodes
