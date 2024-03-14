{-# LANGUAGE OverloadedStrings #-}

module Encoding.CharacterEncoding
    ( getCharacterHexCode
    , decodeCharacterHexCode
    , encodeCharacter
    , decodeCharacter
    , HexCode
    , Character
    ) where

import Data.Text (Text)
import qualified Data.Map as Map

-- Type aliases for clarity and consistency across modules
type Character = Char
type HexCode = Text

-- Mapping from Character to HexCode for encoding
encodeCharacterMap :: Map.Map Character HexCode
encodeCharacterMap = Map.fromList
    [ ('A', "#151B54"), ('B', "#191970"), ('C', "#000080"), ('D', "#151B8D"), ('E', "#00008B")
    , ('F', "#15317E"), ('G', "#0000A0"), ('H', "#0000A5"), ('I', "#0020C2"), ('J', "#0000CD")
    , ('K', "#0041C2"), ('L', "#2916F5"), ('M', "#0000FF"), ('N', "#0002FF"), ('O', "#0909FF")
    , ('P', "#1F45FC"), ('Q', "#2554C7"), ('R', "#1569C7"), ('S', "#1974D2"), ('T', "#2B60DE")
    , ('U', "#4169E1"), ('V', "#2B65EC"), ('W', "#0059FF"), ('X', "#306EFF"), ('Y', "#157DEC")
    , ('Z', "#1589FF"), ('a', "#1E90FF"), ('b', "#368BC1"), ('c', "#4682B4"), ('d', "#488AC7")
    , ('e', "#357EC7"), ('f', "#3090C7"), ('g', "#14A3C7"), ('h', "#659EC7"), ('i', "#87AFC7")
    , ('j', "#95B9C7"), ('k', "#6495ED"), ('l', "#6698FF"), ('m', "#56A5EC"), ('n', "#38ACEC")
    , ('o', "#00BFFF"), ('p', "#3BB9FF"), ('q', "#5CB3FF"), ('r', "#79BAEC"), ('s', "#82CAFF")
    , ('t', "#87CEFA"), ('u', "#87CEEB"), ('v', "#A0CFEC"), ('w', "#B7CEEC"), ('x', "#B4CFEC")
    , ('y', "#ADDFFF"), ('z', "#C2DFFF"), ('0', "#C6DEFF"), ('1', "#BDEDFF"), ('2', "#B0E0E6")
    , ('3', "#AFDCEC"), ('4', "#ADD8E6"), ('5', "#B0CFDE"), ('6', "#C9DFEC"), ('7', "#DBE9FA")
    , ('8', "#E3E4FA"), ('9', "#DBE9FA"), (';', "#FAF884"), ('"', "#FFFF33"), (':', "#FFFF00")
    , ('.', "#FEF250"), (',', "#FFEF00"), ('(', "#F5E216"), (')', "#FFDB58"), ('[', "#FFDF00")
    , (']', "#F9DB24"), ('{', "#EED202"), ('}', "#FFD801"), ('<', "#FFD700"), ('>', "#FDD017")
    , ('\'', "#FFCE44"), ('=', "#810541"), ('+', "#7D0541"), ('*', "#7D0552"), ('/', "#872657")
    , ('-', "#EBDDE2"), ('_', "#E1D9D1"), ('\\', "#E9E4D4"), ('|', "#EFEBD8"), ('!', "#EDE6D6")
    , ('@', "#F0E9D6"), ('#', "#F8F0E3"), ('$', "#FAF0DD"), ('%', "#FFF8E7"), ('^', "#F8F6F0")
    , ('&', "#F3E8EA"), ('~', "#FFF0F5"), ('`', "#FDEEF4"), ('?', "#FFF9E3")
    ]

-- Reverse mapping from HexCode to Character for decoding
hexToCharacterMap :: Map.Map HexCode Character
hexToCharacterMap = Map.fromList . map (\(a, b) -> (b, a)) $ Map.toList encodeCharacterMap

-- Decoding and encoding functions as described...
getCharacterHexCode :: Character -> Maybe HexCode
getCharacterHexCode = flip Map.lookup encodeCharacterMap

decodeCharacterHexCode :: HexCode -> Maybe Character
decodeCharacterHexCode = flip Map.lookup hexToCharacterMap

-- (Optional) Directly encode a character to its hex representation
encodeCharacter :: Character -> Maybe Text
encodeCharacter = getCharacterHexCode

-- (Optional) Directly decode a hex code to its character representation
decodeCharacter :: Text -> Maybe Char
decodeCharacter = decodeCharacterHexCode