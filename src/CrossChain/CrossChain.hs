module CrossChain.CrossChain where
{-
{-# LANGUAGE OverloadedStrings #-}

module CrossChain.CrossChain
    ( CrossChainConfig(..)
    , CrossChainTx(..)
    , initiateCrossChainTx
    , verifyCrossChainTx
    , crossChainComputation
    , verifyProof
    , executeComputation
    , integrateWithBitcoin
    , processCrossChainTx
    , CircuitExecution(..)
    , ComputationResult(..)
    ) where

import Types.Types (Proof, PublicKey, Circuit(..))
import qualified Plonk.Functions as Plonk
import qualified Encoding.RainbowDecoder as RainbowDecoder
import Control.Concurrent.Async (async, wait)
import Control.Monad (join)
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import RainbowRoad.Types (RainbowRoadState, Proof)
import qualified RainbowRoad.Crypto as Crypto
import qualified RainbowRoad.ZkProof as ZkProof
import qualified CrossChain.Protocol as Protocol
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified RainbowBitcoin.Merkle as Merkle

data CrossChainConfig = CrossChainConfig
    { protocolConfig :: Protocol.Config
    , zkProofConfig :: ZkProof.Config
    }

data CrossChainTx = CrossChainTx
    { txHash :: ByteString
    , payload :: ByteString
    , proof :: Proof
    }

data CircuitExecution = CircuitExecution
    { circuitInputs :: [ByteString]
    , circuitOutput :: ByteString
    } deriving (Show, Eq, Generic, NFData)

data ComputationResult = ComputationResult
    { computationOutput :: Text
    , computationProof :: Proof
    } deriving (Show, Eq, Generic, NFData)

crossChainComputation :: Circuit -> PublicKey -> Proof -> IO (Either String ComputationResult)
crossChainComputation circuit publicKey proof = do
    proofValidationResult <- async $ verifyProof circuit publicKey proof
    computationResult <- async $ executeComputation circuit
    join $ do
        isValid <- wait proofValidationResult
        if isValid
            then do
                CircuitExecution {circuitOutput = output} <- wait computationResult
                decodedOutput <- RainbowDecoder.decodeFromRainbowCode $ TE.decodeUtf8 output
                case decodedOutput of
                    Left err -> return $ Left $ "Error decoding circuit output: " ++ err
                    Right plaintext -> return $ Right $ ComputationResult plaintext proof
            else return $ Left "Invalid proof"

verifyProof :: Circuit -> PublicKey -> Proof -> IO Bool
verifyProof Circuit {gates = gateList} (PublicKey publicKey) (Proof proofData) = do
    let circuitInputs = getCircuitInputs gateList
    let circuitOutput = getCircuitOutput gateList
    let circuitExecutionResult = CircuitExecution circuitInputs circuitOutput
    Plonk.verifyProof circuitExecutionResult (C8.pack publicKey) proofData

executeComputation :: Circuit -> IO CircuitExecution
executeComputation circuit@Circuit {gates = gateList} = do
    let circuitInputs = getCircuitInputs gateList
    let circuitOutput = getCircuitOutput gateList
    case Plonk.executeCircuit circuit circuitInputs of
        Left err -> error $ "Error executing circuit: " ++ err
        Right output -> return $ CircuitExecution circuitInputs output

getCircuitInputs :: [Plonk.Gate] -> [ByteString]
getCircuitInputs = map (\(Plonk.InputGate input) -> input)

getCircuitOutput :: [Plonk.Gate] -> ByteString
getCircuitOutput = \case
    (Plonk.OutputGate output:_) -> output
    _ -> error "No output gate found in the circuit"

initiateCrossChainTx :: CrossChainConfig -> RainbowRoadState -> ExceptT String IO CrossChainTx
initiateCrossChainTx config state = do
    let zkProofConfig = zkProofConfig config
    proof <- liftIO $ ZkProof.generateProof zkProofConfig state
    let payload = Protocol.encodePayload state proof
    txHash <- liftIO $ Protocol.submitTx (protocolConfig config) payload
    return $ CrossChainTx txHash payload proof

verifyCrossChainTx :: CrossChainConfig -> CrossChainTx -> ExceptT String IO Bool
verifyCrossChainTx config (CrossChainTx txHash payload proof) = do
    let zkProofConfig = zkProofConfig config
    proofValid <- liftIO $ ZkProof.verifyProof zkProofConfig proof
    txValid <- liftIO $ Protocol.verifyTx (protocolConfig config) txHash
    payloadValid <- liftIO $ Protocol.verifyPayload (protocolConfig config) payload
    return $ proofValid && txValid && payloadValid

integrateWithBitcoin :: BtcAnchor.BtcAnchorConfig -> [RainbowRoadState] -> ExceptT String IO ()
integrateWithBitcoin config states = do
    proofs <- liftIO $ mapM generateProof states
    let stateHashes = map Crypto.hash states
    let proofHashes = map Crypto.hash proofs
    let merkleTree = Merkle.buildMerkleTree (stateHashes ++ proofHashes)
    let merkleRoot = Merkle.computeMerkleRoot (stateHashes ++ proofHashes)
    btcAnchorTx <- BtcAnchor.anchorToBitcoin config merkleRoot
    verified <- BtcAnchor.verifyBtcAnchorTx config btcAnchorTx
    if verified
        then return ()
        else throwError "Failed to integrate with Bitcoin"

processCrossChainTx :: CrossChain.CrossChainConfig -> ByteString -> CrossChain.CrossChainTx -> ExceptT String IO RainbowRoadState
processCrossChainTx config merkleRoot crossChainTx = do
    verified <- CrossChain.verifyCrossChainTx config merkleRoot crossChainTx
-}