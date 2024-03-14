{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.RainbowAPI
    ( API
    , CodeInput(..)
    , PlaintextOutput(..)
    , ErrorMessage(..)
    , encodeHandler
    , decodeHandler
    , app
    ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import Data.Text (Text, pack)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import qualified Encoding.EncodingService as Encoding
import qualified Encoding.RainbowDecoder as RainbowDecoder
import qualified Ast.ASTToRainbowToPLONK as ASTParser
import Text.Parsec (parse)
import Data.Either (lefts, rights)
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
import Data.Bifunctor (bimap)

-- Definitions for the API's data types
newtype CodeInput = CodeInput { code :: Text } deriving (Eq, Show, Generic)
newtype PlaintextOutput = PlaintextOutput { plaintext :: Text } deriving (Eq, Show, Generic)
newtype ErrorMessage = ErrorMessage { message :: Text } deriving (Eq, Show, Generic)

instance FromJSON CodeInput
instance ToJSON PlaintextOutput
instance ToJSON ErrorMessage
instance FromJSON PlaintextOutput
instance ToJSON CodeInput

type API = "encode" :> ReqBody '[JSON] CodeInput :> Post '[JSON] (Either ErrorMessage PlaintextOutput)
      :<|> "decode" :> ReqBody '[JSON] PlaintextOutput :> Post '[JSON] (Either ErrorMessage CodeInput)

-- Handler to encode input code to RainbowCode
encodeHandler :: CodeInput -> Handler (Either ErrorMessage PlaintextOutput)
encodeHandler (CodeInput codeText) = do
    let code = T.unpack codeText -- Convert Text to String if needed for parsing
    case parse ASTParser.astParser "" code of
        Left parseError -> return $ Left $ ErrorMessage (T.pack $ show parseError)
        Right astNode -> do
            -- Convert ASTNode to IR
            let ir = astNodeToIR astNode -- Use a local function to convert ASTNode to IR
            -- Convert IR to RainbowCode
            let encodedResult = Encoding.irToRainbowCode ir -- Use the available function from Encoding module
            return $ Right $ PlaintextOutput encodedResult

astNodeToIR :: ASTNode -> IR
astNodeToIR (FunctionNode name params body) = IRFunction name (map parameterToIRParameter params) (map astNodeToIR body)
astNodeToIR (VariableDecl name typ maybeInit) = IRVariableDecl name (typeNameToType typ) (astNodeToIR <$> maybeInit)
astNodeToIR (VariableAssign name value) = IRVariableAssign name (astNodeToIR value)
astNodeToIR (OperationNode opType operands) = IROperation (operationTypeToOperator opType) (map astNodeToIR operands)
astNodeToIR (LiteralNode lit) = IRLiteral (literalToIR lit)
astNodeToIR (IfNode condition thenBranch maybeElseBranch) = IRIf (astNodeToIR condition) (map astNodeToIR thenBranch) (map astNodeToIR <$> maybeElseBranch)
astNodeToIR (WhileNode loopType condition body) = IRWhile (astNodeToIR condition) (map astNodeToIR body)
astNodeToIR (BlockNode stmts) = IRBlock (map astNodeToIR stmts)
astNodeToIR (FunctionCallNode name args) = IRFunctionCall name (map astNodeToIR args)
astNodeToIR (ClassNode name body) = IRUnknown (pack "Class definition not supported")
astNodeToIR (MethodNode name params body) = IRFunction name (map parameterToIRParameter params) (map astNodeToIR body)
astNodeToIR (ReturnNode maybeValue) = IRReturn (astNodeToIR <$> maybeValue)
astNodeToIR (CommentNode text) = IRComment text

literalToIR :: LiteralType -> Literal
literalToIR (LString s) = StringLit (T.unpack s)
literalToIR (LNumber n) = IntLit (round n)
literalToIR (LBoolean b) = BoolLit b
literalToIR LNull = ObjectLit []
literalToIR (LTuple elems) = ArrayLit (map literalToIR elems)
literalToIR (LArray elems) = ArrayLit (map literalToIR elems)
literalToIR (LObject pairs) = ObjectLit (map (bimap VariableName literalToIR) pairs)

operationTypeToOperator :: OperationType -> Operator
operationTypeToOperator (OpArithmetic "+") = OpAdd
operationTypeToOperator (OpArithmetic "-") = OpSubtract
operationTypeToOperator (OpArithmetic "*") = OpMultiply
operationTypeToOperator (OpArithmetic "/") = OpDivide
operationTypeToOperator (OpComparison "==") = OpEqual
operationTypeToOperator (OpComparison "!=") = OpNotEqual
operationTypeToOperator (OpComparison ">") = OpGreaterThan
operationTypeToOperator (OpComparison ">=") = OpGreaterThanOrEqual
operationTypeToOperator (OpComparison "<") = OpLessThan
operationTypeToOperator (OpComparison "<=") = OpLessThanOrEqual
operationTypeToOperator (OpLogical "&&") = OpAnd
operationTypeToOperator (OpLogical "||") = OpOr
operationTypeToOperator (OpBitwise "&") = OpBitwiseAnd
operationTypeToOperator (OpBitwise "|") = OpBitwiseOr
operationTypeToOperator (OpBitwise "^") = OpBitwiseXor
operationTypeToOperator (OpBitwise "~") = OpBitwiseNot
operationTypeToOperator (OpBitwise "<<") = OpShiftLeft
operationTypeToOperator (OpBitwise ">>") = OpShiftRight

parameterToIRParameter :: ASTParameter -> Parameter
parameterToIRParameter (ASTParameter name typ) = Parameter name (typeNameToType typ)

typeNameToType :: TypeName -> Type
typeNameToType (TypeName "Int") = TypeInt
typeNameToType (TypeName "String") = TypeString
typeNameToType (TypeName "Bool") = TypeBool
typeNameToType (TypeName "Array") = TypeArray TypeUnknown
typeNameToType (TypeName "Object") = TypeObject []
typeNameToType (TypeName "Function") = TypeFunction [] TypeUnknown

-- Handler to decode RainbowCode back to source code
decodeHandler :: PlaintextOutput -> Handler (Either ErrorMessage CodeInput)
decodeHandler (PlaintextOutput encodedText) = do
    case RainbowDecoder.decodeFromRainbowCode encodedText of
        Just text -> return $ Right $ CodeInput text
        Nothing -> return $ Left $ ErrorMessage "Failed to decode the input text."

-- Servant server and app definition
server :: Server API
server = encodeHandler :<|> decodeHandler

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = do
    putStrLn "Starting RainbowAPI server on port 8080..."
    run 8080 app
