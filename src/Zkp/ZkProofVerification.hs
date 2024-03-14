module Zkp.ZkProofVerification where
{-
{-# LANGUAGE OverloadedStrings #-}

module Zkp.ZkProofVerification
    ( verifyProof
    , ProofVerificationError(..)
    ) where

import Types.Types (PublicKey(..), Proof(..))
import qualified Plonk.Functions as Plonk
import qualified Encoding.RainbowDecoder as RainbowDecoder
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data ProofVerificationError = InvalidProof | InvalidPublicKey
    deriving (Show, Eq, Generic, NFData)

verifyProof :: PublicKey -> Proof -> ExceptT ProofVerificationError IO Bool
verifyProof (PublicKey publicKey) (Proof proofData) = do
    let publicKeyBytes = C8.pack publicKey
    let proofBytes = proofData
    case Plonk.verifyProof publicKeyBytes proofBytes of
        Left err -> throwError InvalidProof
        Right result -> return result
-}