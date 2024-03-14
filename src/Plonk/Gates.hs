{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module Plonk.Gates where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Codec.Compression.GZip (compress, decompress)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (cipherInit, ecbEncrypt, ecbDecrypt)
import Data.ByteArray (convert)
import Control.Exception (SomeException, try)
import System.Log.Logger (infoM)
import Crypto.Hash (hashWith, SHA256(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Crypto.Error (CryptoFailable(..))
import Crypto.Random.Types (MonadRandom)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Crypto.PubKey.RSA (PrivateKey, PublicKey)
import Crypto.PubKey.RSA.PKCS15 (signSafer, verify)

data Gate a = Gate
    { gateName :: Text
    , gateLogic :: a -> IO a
    }
    deriving (Generic, NFData)

signMessage :: MonadRandom m => PrivateKey -> ByteString -> m (Either String ByteString)
signMessage privateKey msg = do
    let msgHash = hashWith SHA256 msg
    result <- signSafer (Just SHA256) privateKey (convert msgHash)
    return $ case result of
        Left err -> Left (show err)
        Right signedMsg -> Right signedMsg

verifyMessage :: PublicKey -> ByteString -> ByteString -> Bool
verifyMessage publicKey signature msg = 
    case hashWith SHA256 msg of
        digest -> verify (Just SHA256) publicKey (convert digest) signature

createGate :: (NFData a) => Text -> (a -> IO a) -> Gate a
createGate name logic = Gate
    { gateName = name
    , gateLogic = \input -> do
        result <- try (logic input)
        case result of
            Left e -> do
                infoM "Plonk.Gates" $ "Error in gate " ++ show name ++ ": " ++ show (e :: SomeException)
                return input
            Right output -> return output
    }

echoGate :: (NFData a) => Gate a
echoGate = createGate "EchoGate" return

reverseGate :: Gate ByteString
reverseGate = createGate "ReverseGate" $ \input -> return $ ByteString.reverse input

compressGate :: Gate ByteString
compressGate = createGate "CompressGate" $ \input -> return $ BL.toStrict $ compress $ BL.fromStrict input

decompressGate :: Gate ByteString
decompressGate = createGate "DecompressGate" $ \input -> return $ BL.toStrict $ decompress $ BL.fromStrict input

encryptGate :: AES256 -> Gate ByteString
encryptGate key = createGate "EncryptGate" $ \input -> return $ ecbEncrypt key input

decryptGate :: AES256 -> Gate ByteString
decryptGate key = createGate "DecryptGate" $ \input -> return $ ecbDecrypt key input

-- Further implementation for parallelGate, dynamicDecisionGate, and signGate
-- would follow similar patterns of handling concurrency and cryptographic operations as above.