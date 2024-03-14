{-# LANGUAGE OverloadedStrings #-}

module Proofs.PLONKProof 
    ( hashOutput
    , verifySignature
    , generateMockSignature
    , generateMockECCKeyPair
    , verifyCircuit
    , exampleVerification)
    where

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
    , Prover(..)
    , Verifier(..)
    )
import Plonk.Functions (executeCircuit)
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.PubKey.ECC.Types (getCurveByName, CurveName(..))
import qualified Data.ByteString.Char8 as C8
import Crypto.Hash (Digest, hashWith)
import Crypto.Hash.Algorithms (SHA256(..))
import Crypto.PubKey.ECC.ECDSA (PublicKey, PrivateKey, Signature, sign, verify)
import Data.ByteString (ByteString)
import Crypto.Random (MonadRandom)
import Control.Monad (guard)
import Crypto.Error (CryptoFailable(..))
import qualified Crypto.PubKey.ECC.ECDSA as PKCS15
import qualified Data.ByteArray as BA

-- hashOutput function to hash a `ByteString` to a `Digest SHA256`
hashOutput :: ByteString -> Digest SHA256
hashOutput = hashWith SHA256

-- verifySignature function to verify a `Signature` against a `PublicKey` and a `Digest SHA256`
verifySignature :: PublicKey -> Digest SHA256 -> Signature -> Bool
verifySignature publicKey hashedMessage signature = PKCS15.verify SHA256 publicKey signature hashedMessage

-- generateMockSignature function to generate a `Signature` from a `PrivateKey` and a `ByteString`
generateMockSignature :: MonadRandom m => PrivateKey -> ByteString -> m (CryptoFailable Signature)
generateMockSignature privateKey message = fmap CryptoPassed (sign privateKey SHA256 message)

-- generateMockECCKeyPair function to generate a `PublicKey` and a `PrivateKey`
generateMockECCKeyPair :: IO (PublicKey, PrivateKey)
generateMockECCKeyPair = do
    let curve = getCurveByName SEC_p256r1
    generate curve

-- generateProof function to generate a proof given a secret key and a circuit
generateProof :: SecretKey -> Circuit -> Proof
generateProof secretKey circuit = Proof { proofData = "mock-proof-data" }

-- Placeholder for a function to verify a PLONK proof against a public key and a circuit
verifyProof :: PublicKey -> Circuit -> Proof -> Bool
verifyProof pk circuit proof = True  -- Placeholder for actual implementation

-- Ensure this is the only `verifyCircuit` definition in use
verifyCircuit :: Circuit -> PublicKey -> Signature -> [ByteString] -> IO Bool
verifyCircuit mockCircuit mockPublicKey signature mockInputs = do
    let convertedInputs = map Input mockInputs
    Output output <- executeCircuit mockCircuit convertedInputs
    -- Assuming `output` is already a ByteString
    let hashedOutput = hashOutput output
    return $ verifySignature mockPublicKey hashedOutput signature

-- exampleVerification function to demonstrate the verification process
exampleVerification :: IO ()
exampleVerification = do
    (mockPublicKey, mockPrivateKey) <- generateMockECCKeyPair
    signatureResult <- generateMockSignature mockPrivateKey "Mock message for signature"
    case signatureResult of
        CryptoPassed signature -> do
            let mockCircuit = undefined :: Circuit  -- Make sure this is properly defined or loaded
            let mockInputs = [C8.pack "Example input 1", C8.pack "Example input 2"]
            verified <- verifyCircuit mockCircuit mockPublicKey signature mockInputs
            putStrLn $ if verified then "Circuit output verified successfully." else "Circuit output verification failed."
        CryptoFailed error -> putStrLn $ "Signature generation failed with error: " ++ show error
