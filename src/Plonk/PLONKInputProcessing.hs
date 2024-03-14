{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Plonk.PLONKInputProcessing
    ( processScriptForPLONK
    , decodeHashToScript
    , hashDigestToHex
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8
import Crypto.Hash (Digest, SHA256, hash, hashWith)
import Encoding.EncodingService (encodeToRainbowCode, decodeFromRainbowCode)
import Ast.ASTToRainbowToPLONK (parseAST, optimizeAST)
import Ast.ASTMapping (mapToCommonAST, commonASTToPlaintext)

import Data.Vector.Fusion.Stream.Monadic (Stream, streams)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Parallel.Strategies (parBuffer, rpar)
import Control.Concurrent.Async (mapConcurrently_)

-- Process a script through AST transformation, conversion to plaintext, encoding to "Rainbow Code", and finally hashing.
processScriptForPLONK :: Text -> IO (Either String (Digest SHA256))
processScriptForPLONK script = do
    let parsedAST = parseAST script
    case parsedAST of
        Left err -> return . Left $ "Error parsing script: " ++ T.unpack err
        Right ast -> do
            let !optimizedAST = optimizeAST ast
            let !commonAst = mapToCommonAST optimizedAST
            let !plaintext = commonASTToPlaintext commonAst
            !encodedRainbowCode <- encodeToRainbowCode plaintext
            return . Right . hashWith SHA256 $ C8.pack $ T.unpack encodedRainbowCode

-- Given a hash digest, attempt to decode it back to its original "Rainbow Code" and then to plaintext.
decodeHashToScript :: Digest SHA256 -> IO (Either String Text)
decodeHashToScript hashDigest = do
    let !hexString = hashDigestToHex hashDigest
    decodedPlaintext <- decodeFromRainbowCode $ T.pack hexString
    maybe (Left "Error decoding from Rainbow Code") Right decodedPlaintext

-- Decode a hash digest back to its original plaintext.
decodeHashToPlaintext :: Digest SHA256 -> IO (Either String Text)
decodeHashToPlaintext hashDigest = do
    let !hexString = hashDigestToHex hashDigest
    -- Attempt to decode the hexadecimal string from "Rainbow Code" back to plaintext.
    decodedPlaintext <- decodeFromRainbowCode $ T.pack hexString
    case decodedPlaintext of
        Nothing -> return $ Left "Error decoding from Rainbow Code"
        Just text -> return $ Right text

-- Convert a hash digest to a hexadecimal string representation.
hashDigestToHex :: Digest SHA256 -> String
hashDigestToHex = show

-- Optimized encoding function
encodeToRainbowCodeStream :: Text -> Stream ByteString
encodeToRainbowCodeStream input = streams $ do
    let !textChunks = V.fromList $ T.chunksOf 1024 input
    !encodedChunksOrErrors <- V.stream =<< mapConcurrently_ (rpar . try . encodeToRainbowCode) textChunks `using` parBuffer 4
    let !encodedChunks = V.map (V.singleton . T.encodeUtf8 . fromMaybe "Error" . handleError) encodedChunksOrErrors
    return (V.concat encodedChunks, hashWith SHA256 (V.stream encodedChunks))

-- Helper to handle exceptions and convert Either to Maybe
handleError :: Either SomeException (Maybe Text) -> Maybe Text
handleError = fromRight Nothing