{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Encoding.EncodingService
    ( findHexCode
    , decodeHexCode
    , allEncodings
    , allDecodings
    , decodeAST
    , encodeAST
    , irToRainbowCode
    , encodeIR
    , HexCode
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text, pack, unpack)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified RainbowCode.Rainbow as RainbowCode
import qualified Encoding.CharacterEncoding as CharEncoding
import qualified Encoding.OpCodeEncoding as OpCodeEncoding
import qualified Encoding.ProgLangEncoding as ProgLangEncoding
import qualified Encoding.SmartContractEncoding as SmartContractEncoding
import Data.Binary (Binary, Get, get, put, decode, putWord8, getWord8, decodeOrFail)
import Data.Word (Word8)
import Types.Types
    ( ASTNode(..)
    , LoopType(..)
    , OperationType(..)
    , LiteralType(..)
    , FunctionName(..)
    , VariableName(..)
    , TypeName(..)
    , ASTParameter(..)
    , Type(..)
    , Operator(..)
    , Literal(..)
    , Parameter(..)
    , IR(..)
    , Gate(..)
    , Wire(..)
    , WireType(..)
    , Circuit(..)
    , Input(..)
    , Output(..)
    , Proof(..)
    , SecretKey(..)
    , PublicKey(..)
    , Prover(..)
    , Verifier(..)
    )

instance Binary IR where
    put (IRFunction fname params body) = do
        putWord8 0
        put fname
        put params
        put body
    put (IRVariableDecl vname varType initialValue) = do
        putWord8 1
        put vname
        put varType
        put initialValue
    -- Add cases for other constructors of the IR type

    get = do
        tag <- getWord8
        case tag of
            0 -> IRFunction <$> get <*> get <*> get
            1 -> IRVariableDecl <$> get <*> get <*> get
            -- Add cases for other constructors of the IR type
            _ -> fail "Invalid IR tag"

instance Binary Parameter where
    put (Parameter (VariableName name) paramType) = do
        put (encodeUtf8 name)
        put paramType
    get = do
        name <- VariableName . decodeUtf8 <$> get
        Parameter name <$> get

instance Binary LiteralType where
    put (LString text) = do
        putWord8 0
        put (encodeUtf8 text)
    put (LNumber num) = do
        putWord8 1
        put num
    put (LBoolean bool) = do
        putWord8 2
        put bool
    -- Extend this pattern for other LiteralType constructors

    get = do
        tag <- getWord8
        case tag of
            0 -> LString . decodeUtf8 <$> get
            1 -> LNumber <$> get
            2 -> LBoolean <$> get
            -- Extend this pattern for other LiteralType constructors
            _ -> fail "Invalid LiteralType tag"

instance Binary ASTNode where
    put (FunctionNode fname params body) = do
        putWord8 0
        put fname
        put params
        put body
    put (VariableDecl vname typ maybeNode) = do
        putWord8 1
        put vname
        put typ
        put maybeNode
    put (LiteralNode litType) = do
        putWord8 2
        put litType
    -- Extend this pattern for other ASTNode constructors

    get = do
        tag <- getWord8
        case tag of
            0 -> FunctionNode <$> get <*> get <*> get
            1 -> VariableDecl <$> get <*> get <*> get
            2 -> LiteralNode <$> get
            -- Extend this pattern for other ASTNode constructors
            _ -> fail "Invalid ASTNode tag"

instance Binary FunctionName where
    put (FunctionName name) = put (encodeUtf8 name)
    get = FunctionName . decodeUtf8 <$> get

instance Binary VariableName where
    put (VariableName name) = put (encodeUtf8 name)
    get = VariableName . decodeUtf8 <$> get

instance Binary Type where
    put TypeInt = putWord8 0
    put TypeString = putWord8 1
    get = do
        tag <- getWord8
        case tag of
            0 -> return TypeInt
            1 -> return TypeString
            _ -> fail "Invalid Type tag"

instance Binary TypeName where
    put (TypeName name) = put (encodeUtf8 name)
    get = TypeName . decodeUtf8 <$> get

type HexCode = T.Text

instance Binary ASTParameter where
    put (ASTParameter (VariableName name) paramType) = do
        put (encodeUtf8 name)
        put paramType
    get = do
        name <- VariableName . decodeUtf8 <$> get
        ASTParameter name <$> get

irToRainbowCode :: IR -> Text
irToRainbowCode ir = case ir of
    IRFunction (FunctionName name) params body -> 
        "Function Name: " <> name <> "; Params: " <> T.intercalate ", " (map (\(Parameter (VariableName pName) pType) -> pName <> " : " <> showType pType) params) <> "; Body: " <> T.concat (map irToRainbowCode body)
    IRVariableDecl (VariableName name) varType initialValue -> 
        "Variable Declaration: Name = " <> name <> ", Type = " <> showType varType <> ", Initial Value = " <> maybe "None" irToRainbowCode initialValue
    -- Handle other cases based on the constructors of your IR type
    _ -> "Unhandled IR type"

encodeIR :: IR -> B.ByteString
encodeIR ast = encodeUtf8 $ irToRainbowCode ast

encodeAST :: IR -> B.ByteString
encodeAST ast = encodeUtf8 $ irToRainbowCode ast

decodeAST :: B.ByteString -> Either String IR
decodeAST bs = case decodeOrFail $ BL.fromStrict bs of
    Left (_, _, err) -> Left err
    Right (_, _, ast) -> Right ast

showType :: Type -> Text
showType TypeInt = "Int"
showType TypeString = "String"

allEncodings :: [(T.Text, HexCode)]
allEncodings = [(T.singleton c, code) | c <- [minBound..maxBound], Just code <- [CharEncoding.encodeCharacter c]]
    ++ Map.toList OpCodeEncoding.encodeOpcodeColors
    ++ Map.toList ProgLangEncoding.progLangEncoding 
    ++ Map.toList SmartContractEncoding.encodeSmartContractColors

findHexCode :: T.Text -> Maybe HexCode
findHexCode input = listToMaybe [hex | (element, hex) <- allEncodings, element == input]

allDecodings :: [(HexCode, T.Text)]
allDecodings = concat
    [ maybeToList ((\c -> (hexCode, T.singleton c)) <$> CharEncoding.decodeCharacter hexCode) | hexCode <- Map.elems (OpCodeEncoding.encodeOpcodeColors `Map.union` ProgLangEncoding.progLangEncoding `Map.union` SmartContractEncoding.encodeSmartContractColors) ]
    ++ Map.toList OpCodeEncoding.hexToOpCodeMap
    ++ Map.toList ProgLangEncoding.hexToProgLangMap 
    ++ Map.toList SmartContractEncoding.hexToSmartContractMap

decodeHexCode :: HexCode -> Maybe T.Text
decodeHexCode hex = listToMaybe [element | (hexCode, element) <- allDecodings, hexCode == hex]
