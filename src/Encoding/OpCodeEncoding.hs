{-# LANGUAGE OverloadedStrings #-}

module Encoding.OpCodeEncoding
    ( encodeOpcodeColors
    , getOpCodeHexCode
    , decodeOpCodeHexCode
    , hexToOpCodeMap
    ) where

import qualified Data.Map as Map
import Data.Text (Text)

type OpCodeElement = Text
type HexCode = Text

encodeOpcodeColors :: Map.Map OpCodeElement HexCode
encodeOpcodeColors = Map.fromList
    [ ("STOP", "#FFA62F"), ("ADD", "#FFA600"), ("MUL", "#FFA500"), ("SUB", "#EE9A4D")
    , ("DIV", "#F4A460"), ("SDIV", "#E2A76F"), ("MOD", "#C19A6B"), ("SMOD", "#E6BF83")
    , ("ADDMOD", "#DEB887"), ("MULMOD", "#D2B48C"), ("EXP", "#C8AD7F")
    , ("SIGNEXTEND", "#C2B280"), ("LT", "#C6BA8B"), ("GT", "#BCB88A"), ("SLT", "#C8B560")
    , ("SGT", "#C9BE62"), ("EQ", "#C9AE5D"), ("ISZERO", "#BDB76B"), ("AND", "#BAB86C")
    , ("OR", "#B5A642"), ("XOR", "#C7A317"), ("NOT", "#D4AF37"), ("BYTE", "#E1AD01")
    , ("SHL", "#E9AB17"), ("SHR", "#E8A317"), ("SAR", "#DAA520"), ("SHA3", "#D4A017")
    , ("ADDRESS", "#C68E17"), ("BALANCE", "#B8860B"), ("ORIGIN", "#C58917")
    , ("CALLER", "#CD853F"), ("CALLVALUE", "#CD7F32"), ("CALLDATALOAD", "#CA762B")
    , ("CALLDATASIZE", "#C88141"), ("CALLDATACOPY", "#B87333"), ("CODESIZE", "#AA6C39")
    , ("CODECOPY", "#A97142"), ("GASPRICE", "#AB784E"), ("EXTCODESIZE", "#966F33")
    , ("EXTCODECOPY", "#906E3E"), ("RETURNDATASIZE", "#806517"), ("RETURNDATACOPY", "#665D1E")
    , ("EXTCODEHASH", "#8E7618"), ("BLOCKHASH", "#8B8000"), ("COINBASE", "#827839")
    , ("TIMESTAMP", "#93917C"), ("NUMBER", "#9F8C76"), ("DIFFICULTY", "#AF9B60")
    , ("GASLIMIT", "#827B60"), ("POP", "#786D5F"), ("MLOAD", "#483C32"), ("MSTORE", "#4A412A")
    , ("MSTORE8", "#473810"), ("SLOAD", "#493D26"), ("SSTORE", "#513B1C"), ("JUMP", "#3D3635")
    , ("JUMPI", "#3B2F2F"), ("PC", "#49413F"), ("MSIZE", "#43302E"), ("GAS", "#622F22")
    , ("JUMPDEST", "#5C3317"), ("PUSH1", "#644117"), ("PUSH2", "#654321"), ("PUSH3", "#704214")
    , ("PUSH4", "#804A00"), ("PUSH5", "#6F4E37"), ("PUSH6", "#835C3B"), ("PUSH7", "#7F5217")
    , ("PUSH8", "#7F462C"), ("PUSH9", "#A0522D"), ("PUSH10", "#8B4513"), ("PUSH11", "#8A4117")
    , ("PUSH12", "#7E3817"), ("PUSH13", "#7E3517"), ("PUSH14", "#954535"), ("PUSH15", "#9E4638")
    , ("PUSH16", "#A55D35"), ("PUSH17", "#B83C08"), ("PUSH18", "#B5651D"), ("PUSH19", "#B76734")
    , ("PUSH20", "#A55D35"), ("PUSH21", "#C36241"), ("PUSH22", "#CB6D51"), ("PUSH23", "#C47451")
    , ("PUSH24", "#D2691E"), ("PUSH25", "#CC6600"), ("PUSH26", "#E56717"), ("PUSH27", "#E66C2C")
    , ("PUSH28", "#FF6700"), ("PUSH29", "#FF5F1F"), ("PUSH30", "#FE632A"), ("PUSH31", "#F87217")
    , ("PUSH32", "#FF7900"), ("DUP1", "#F88017"), ("DUP2", "#FF8C00"), ("DUP3", "#F87431")
    , ("DUP4", "#FF7722"), ("DUP5", "#E67451"), ("DUP6", "#FF8040"), ("DUP7", "#FF7F50")
    , ("DUP8", "#F88158"), ("DUP9", "#F9966B"), ("DUP10", "#FFA07A"), ("DUP11", "#F89880")
    , ("DUP12", "#E9967A"), ("DUP13", "#E78A61"), ("DUP14", "#DA8A67"), ("DUP15", "#FF8674")
    , ("DUP16", "#FA8072"), ("SWAP1", "#F98B88"), ("SWAP2", "#F08080"), ("SWAP3", "#F67280")
    , ("SWAP4", "#E77471"), ("SWAP5", "#F75D59"), ("SWAP6", "#E55451"), ("SWAP7", "#CD5C5C")
    , ("SWAP8", "#FF6347"), ("SWAP9", "#E55B3C"), ("SWAP10", "#FF4500"), ("SWAP11", "#FF0000")
    , ("SWAP12", "#FD1C03"), ("SWAP13", "#FF2400"), ("SWAP14", "#F62217"), ("SWAP15", "#F70D1A")
    , ("SWAP16", "#F62817"), ("LOG0", "#E42217"), ("LOG1", "#E41B17"), ("LOG2", "#DC381F")
    , ("LOG3", "#C83F49"), ("LOG4", "#C24641"), ("CREATE", "#C11B17"), ("CALL", "#B22222")
    , ("CALLCODE", "#B21807"), ("RETURN", "#A70D2A"), ("DELEGATECALL", "#A70D2A"), ("CREATE2", "#A70D2A")
    , ("STATICCALL", "#A70D2A"), ("REVERT", "#A70D2A"), ("INVALID", "#A70D2A"), ("SELFDESTRUCT", "#A70D2A")
    ]

-- Creating a reverse mapping for decoding
hexToOpCodeMap :: Map.Map HexCode OpCodeElement
hexToOpCodeMap = Map.fromList $ map (\(op, hex) -> (hex, op)) $ Map.toList encodeOpcodeColors

getOpCodeHexCode :: OpCodeElement -> Maybe HexCode
getOpCodeHexCode = flip Map.lookup encodeOpcodeColors

decodeOpCodeHexCode :: HexCode -> Maybe OpCodeElement
decodeOpCodeHexCode = flip Map.lookup hexToOpCodeMap