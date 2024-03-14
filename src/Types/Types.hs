{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Types.Types
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
    , ErrorMessage(..)
    , RainbowProof(..)
    , RainbowRoadState(..)
    ) where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Aeson (ToJSON(..), FromJSON(..), withObject, (.:?), (.=), (.:), object)
import GHC.Generics (Generic)
import Data.Aeson.Types (withText, Parser)

-- New Types for Better Type Safety
newtype FunctionName = FunctionName Text deriving (Show, Eq)
newtype VariableName = VariableName Text deriving (Show, Eq)
newtype TypeName = TypeName Text deriving (Show, Eq)

-- Enhanced Literal and Operation Types for Consistency with IRDefinitions
data LiteralType
    = LString Text
    | LNumber Double
    | LBoolean Bool
    | LNull
    | LTuple [LiteralType]
    | LArray [LiteralType]
    | LObject [(Text, LiteralType)]
    deriving (Show, Eq)

data CommonAST
    = CommonFunction Text [CommonAST]
    | CommonVariable Text CommonAST
    | CommonReturn CommonAST
    | CommonIf CommonAST [CommonAST] (Maybe [CommonAST])
    | CommonLoop LoopType CommonAST [CommonAST]
    | CommonCall Text [CommonAST]
    | CommonOperation OperationType [CommonAST]
    | CommonLiteral LiteralType
    | CommonAssignment Text CommonAST
    | CommonBlock [CommonAST]
    | CommonComment Text
    | CommonClass Text [CommonAST]
    | CommonMethod Text [CommonAST]
    | CommonConstructor [CommonAST]
    | CommonProperty Text CommonAST
    | CommonImport Text (Maybe Text)
    | CommonExport Text
    deriving (Show, Eq)
    
-- Simplified AST Structure with Enhanced Type Safety
data ASTNode
    = FunctionNode FunctionName [ASTParameter] [ASTNode]
    | VariableDecl VariableName TypeName (Maybe ASTNode)
    | VariableAssign VariableName ASTNode
    | OperationNode OperationType [ASTNode]
    | LiteralNode LiteralType
    | IfNode ASTNode [ASTNode] (Maybe [ASTNode])
    | WhileNode LoopType ASTNode [ASTNode]
    | BlockNode [ASTNode]
    | FunctionCallNode FunctionName [ASTNode]
    | ClassNode TypeName [ASTNode]
    | MethodNode FunctionName [ASTParameter] [ASTNode]
    | ReturnNode (Maybe ASTNode)
    | CommentNode Text
    deriving (Show, Eq, Generic)

instance ToJSON ASTNode where
    -- Implement toJSON for ASTNode

instance FromJSON ASTNode where
    -- Implement parseJSON for ASTNode

instance ToJSON FunctionName where
    toJSON (FunctionName name) = toJSON name

instance FromJSON FunctionName where
    parseJSON = fmap FunctionName . parseJSON

instance ToJSON OperationType where
    toJSON (OpArithmetic text) = toJSON text
    toJSON (OpLogical text) = toJSON text
    toJSON (OpComparison text) = toJSON text

instance FromJSON OperationType where
    parseJSON = withObject "OperationType" $ \obj -> do
        arithmetic <- obj .:? pack "arithmetic"
        logical <- obj .:? pack "logical"
        comparison <- obj .:? pack "comparison"
        case (arithmetic, logical, comparison) of
            (Just text, Nothing, Nothing) -> return (OpArithmetic text)
            (Nothing, Just text, Nothing) -> return (OpLogical text)
            (Nothing, Nothing, Just text) -> return (OpComparison text)
            _ -> fail "Invalid OperationType"

instance ToJSON VariableName where
    toJSON (VariableName name) = toJSON name

instance FromJSON VariableName where
    parseJSON = fmap VariableName . parseJSON

instance ToJSON TypeName where
    toJSON (TypeName name) = toJSON name

instance FromJSON TypeName where
    parseJSON = fmap TypeName . parseJSON

instance ToJSON LiteralType where
    toJSON (LString text) = object ["type" .= pack "string", "value" .= text]
    toJSON (LNumber num) = object ["type" .= pack "number", "value" .= num]
    toJSON (LBoolean bool) = object ["type" .= pack "boolean", "value" .= bool]
    toJSON LNull = object ["type" .= pack "null"]
    toJSON (LTuple elems) = object ["type" .= pack "tuple", "value" .= elems]
    toJSON (LArray elems) = object ["type" .= pack "array", "value" .= elems]
    toJSON (LObject pairs) = object ["type" .= pack "object", "value" .= pairs]

instance FromJSON LiteralType where
    parseJSON = withObject "LiteralType" $ \obj -> do
        type' <- obj .: "type" :: Parser Text
        case type' of
            "string" -> LString <$> obj .: "value"
            "number" -> LNumber <$> obj .: "value"
            "boolean" -> LBoolean <$> obj .: "value"
            "null" -> pure LNull
            "tuple" -> LTuple <$> obj .: "value"
            "array" -> LArray <$> obj .: "value"
            "object" -> LObject <$> obj .: "value"
            _ -> fail "Invalid LiteralType"

instance ToJSON LoopType where
    toJSON ForLoop = "ForLoop"
    toJSON WhileLoop = "WhileLoop"
    toJSON DoWhileLoop = "DoWhileLoop"

instance FromJSON LoopType where
    parseJSON = withText "LoopType" $ \case
        "ForLoop" -> pure ForLoop
        "WhileLoop" -> pure WhileLoop
        "DoWhileLoop" -> pure DoWhileLoop
        _ -> fail "Invalid LoopType"

instance ToJSON ASTParameter where
    toJSON (ASTParameter name typ) = object ["paramName" .= name, "paramType" .= typ]

instance FromJSON ASTParameter where
    parseJSON = withObject "ASTParameter" $ \obj ->
        ASTParameter <$> obj .: "paramName" <*> obj .: "paramType"

data ASTParameter = ASTParameter
    { paramName :: VariableName
    , paramType :: TypeName
    }
    deriving (Show, Eq)

data LoopType = ForLoop | WhileLoop | DoWhileLoop deriving (Show, Eq)

-- IR Definitions
data IR
    = IRFunction { functionName :: FunctionName, parameters :: [Parameter], body :: [IR] }
    | IRVariableDecl { varName :: VariableName, varType :: Type, initialValue :: Maybe IR }
    | IRVariableAssign { varName :: VariableName, newValue :: IR }
    | IROperation { operator :: Operator, operands :: [IR] }
    | IRLiteral { literalValue :: Literal }
    | IRIf { condition :: IR, thenBranch :: [IR], elseBranch :: Maybe [IR] }
    | IRWhile { condition :: IR, loopBody :: [IR] }
    | IRReturn { returnValue :: Maybe IR }
    | IRBlock { statements :: [IR] }
    | IRFunctionCall { functionName :: FunctionName, arguments :: [IR] }
    | IRComment { commentText :: Text }
    | IRSequence { sequenceOfIR :: [IR] }
    | IRUnknown { unknownText :: Text }
    deriving (Show, Eq)

data Parameter = Parameter
    { paramName' :: VariableName
    , paramType' :: Type
    }
    deriving (Show, Eq)

data Type
    = TypeInt
    | TypeString
    | TypeBool
    | TypeArray Type
    | TypeObject [(VariableName, Type)]
    | TypeFunction [Type] Type
    | TypeUnknown
    deriving (Show, Eq)

data OperationType
    = OpArithmetic Text
    | OpLogical Text
    | OpComparison Text
    | OpBitwise Text
    deriving (Show, Eq)

data Operator
    = OpAdd
    | OpSubtract
    | OpMultiply
    | OpDivide
    | OpEqual
    | OpNotEqual
    | OpGreaterThan
    | OpGreaterThanOrEqual
    | OpLessThan
    | OpLessThanOrEqual
    | OpAnd
    | OpOr
    | OpBitwiseAnd
    | OpBitwiseOr
    | OpBitwiseXor
    | OpBitwiseNot
    | OpShiftLeft
    | OpShiftRight
    | OpUnknown
    deriving (Show, Eq)

data Literal
    = IntLit Integer
    | StringLit String
    | BoolLit Bool
    | ArrayLit [Literal]
    | ObjectLit [(VariableName, Literal)]
    deriving (Show, Eq)

-- Enhanced Gate type to include a unique identifier and a list of connected wires.
data Gate = Gate {
    gateId :: Int,
    operation :: ByteString -> ByteString,
    metadata :: Map String String,
    connectedWires :: [Wire]
}

instance Show Gate where
    show (Gate gid _ meta wires) =
        "Gate {gateId = " ++ show gid ++
        ", operation = <function>, metadata = " ++ show meta ++
        ", connectedWires = " ++ show wires ++ "}"


data Wire = Wire {
    wireId :: Int,
    wireType :: WireType,
    connectedGate :: Int
} deriving (Show, Eq, Ord)

data WireType = InputWire | OutputWire deriving (Show, Eq, Ord)

data Circuit = Circuit {
    nextGateId :: Int,
    nextWireId :: Int,
    gates :: Map Int Gate,
    wireConnections :: Map Int Int,
    gateOutputs :: Map Int ByteString
} deriving (Show)

newtype Input = Input ByteString deriving (Show)
newtype Output = Output ByteString deriving (Show)

-- Proof type representing the PLONK proof.
data Proof = Proof {
    proofData :: ByteString,
    proofMetadata :: Map String String
} deriving (Show)

-- SecretKey type representing the secret key used by the prover.
newtype SecretKey = SecretKey ByteString deriving (Show)

-- PublicKey type representing the public key used by the verifier.
newtype PublicKey = PublicKey ByteString deriving (Show)

-- Prover type representing the entity generating the PLONK proof.
data Prover = Prover {
    proverId :: String,
    proverSecretKey :: SecretKey
} deriving (Show)

-- Verifier type representing the entity verifying the PLONK proof.
data Verifier = Verifier {
    verifierId :: String,
    verifierPublicKey :: PublicKey
} deriving (Show)

-- ErrorMesage
newtype ErrorMessage = ErrorMessage { message :: Text } deriving (Show, Eq, Generic)

data RainbowRoadState = RainbowRoadState
    { stateData :: ByteString
    , stateHash :: ByteString
    } deriving (Show, Eq)

data RainbowProof = RainbowProof
    { rainbowProofData :: ByteString
    , rainbowProofHash :: ByteString
    } deriving (Show, Eq)