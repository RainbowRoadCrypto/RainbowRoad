module Zkp.ZkProofGeneration where
{-
{-# LANGUAGE OverloadedStrings #-}

module Zkp.ZkProofGeneration
    ( generateProof
    , createProver
    , ProofGenerationError(..)
    ) where

import Types.Types (SecretKey(..), Circuit(..), Proof(..), Prover(..))
import qualified Plonk.Functions as Plonk
import qualified Plonk.Circuit as Plonk
import qualified Plonk.Gates as Plonk
import qualified Encoding.RainbowDecoder as RainbowDecoder
import Control.Concurrent.Async (async, wait)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data ProofGenerationError = InvalidSecretKey | InvalidCircuit | ProofGenerationFailure
    deriving (Show, Eq, Generic, NFData)

generateProof :: SecretKey -> Circuit -> IO (Either ProofGenerationError Proof)
generateProof secretKey circuit = do
    let prover = createProver secretKey
    runExceptT $ do
        validatedCircuit <- validateCircuit circuit
        proof <- generatePlonkProof prover validatedCircuit
        optimizeProof proof

createProver :: SecretKey -> Prover
createProver (SecretKey secretKey) = Prover (C8.pack secretKey)

validateCircuit :: Circuit -> ExceptT ProofGenerationError IO Circuit
validateCircuit circuit@Circuit {gates = gateList} = do
    let inputGates = filter isInputGate gateList
    let outputGates = filter isOutputGate gateList
    case (inputGates, outputGates) of
        ([], []) -> throwError InvalidCircuit
        _ -> return circuit
  where
    isInputGate (Plonk.InputGate _) = True
    isInputGate _ = False
    isOutputGate (Plonk.OutputGate _) = True
    isOutputGate _ = False

generatePlonkProof :: Prover -> Circuit -> ExceptT ProofGenerationError IO Proof
generatePlonkProof (Prover secretKey) circuit = do
    let circuitInputs = getCircuitInputs (gates circuit)
    case Plonk.generateProof secretKey circuit circuitInputs of
        Left err -> throwError ProofGenerationFailure
        Right proofData -> return $ Proof proofData

optimizeProof :: Proof -> ExceptT ProofGenerationError IO Proof
optimizeProof (Proof proofData) = do
    optimizedProofData <- liftIO $ async $ Plonk.optimizeProof proofData
    liftIO $ wait optimizedProofData

getCircuitInputs :: [Plonk.Gate] -> [ByteString]
getCircuitInputs = map (\(Plonk.InputGate input) -> input)
-}