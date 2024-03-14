{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Plonk.Circuit
    ( FoleyCircuit(..)
    , Gate(..)
    , ConditionalGate(..)
    , DynamicGate(..)
    , newFoleyCircuit
    , addGate
    , addConditionalGate
    , addDynamicGate
    , runFoleyCircuit
    ) where


import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Crypto.Hash (hashWith, Digest, SHA256(..))
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Control.Monad (join)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Control.DeepSeq (NFData, force)
import Encoding.EncodingService (findHexCode, decodeHexCode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- EDSL for Foley Circuit Abstraction
data Gate a = AddGate a a a
            | MulGate a a a
            | HashGate a
            | EncodeGate a
            | DecodeGate a
            deriving (Generic, NFData)

data ConditionalGate a = ConditionalGate (a -> Bool) (Gate a) (Gate a)
    deriving (Generic, NFData)

newtype DynamicGate a = DynamicGate (a -> FoleyCircuit a)
    deriving (Generic, NFData)

data FoleyCircuit a = FoleyCircuit
    { gates :: [Gate a]
    , conditionalGates :: [ConditionalGate a]
    , dynamicGates :: [DynamicGate a]
    }
    deriving (Generic, NFData)


newFoleyCircuit :: FoleyCircuit a
newFoleyCircuit = FoleyCircuit { gates = [], conditionalGates = [], dynamicGates = [] }

addGate :: Gate a -> FoleyCircuit a -> FoleyCircuit a
addGate gate circuit = circuit { gates = gate : gates circuit }

addConditionalGate :: (a -> Bool) -> Gate a -> Gate a -> FoleyCircuit a -> FoleyCircuit a
addConditionalGate cond trueG falseG circuit =
    circuit { conditionalGates = ConditionalGate cond trueG falseG : conditionalGates circuit }

addDynamicGate :: (a -> FoleyCircuit a) -> FoleyCircuit a -> FoleyCircuit a
addDynamicGate dynamicFunc circuit =
    circuit { dynamicGates = DynamicGate dynamicFunc : dynamicGates circuit }

processGate :: (NFData a, Show a, Read a, Num a) => a -> V.Vector (a, Digest SHA256) -> Gate a -> V.Vector (a, Digest SHA256)
processGate input acc gate = case gate of
    AddGate a b c -> do
        let result = force $ a + b + c
        V.cons (result, hashWith SHA256 (C8.pack $ show result)) acc
    MulGate a b c -> do
        let result = force $ a * b * c
        V.cons (result, hashWith SHA256 (C8.pack $ show result)) acc
    HashGate a -> do
        let result = hashWith SHA256 (C8.pack $ show a)
        V.cons (a, result) acc
    EncodeGate a -> do
        let result = encodeElement a
        V.cons (a, hashWith SHA256 (C8.pack $ show result)) acc
    DecodeGate a -> do
        let result = decodeElement (C8.pack $ show a)
        V.cons (result, hashWith SHA256 (C8.pack $ show result)) acc
    

runFoleyCircuit :: (NFData a, Show a, Read a, Num a) => FoleyCircuit a -> a -> V.Vector (a, Digest SHA256)
runFoleyCircuit circuit inputData = do
    let outputAfterGates = foldl' (processGate inputData) V.empty (gates circuit)
    let outputAfterConditionalGates = applyConditionalGates inputData outputAfterGates (conditionalGates circuit)
    applyDynamicGates inputData outputAfterConditionalGates (dynamicGates circuit)

applyConditionalGates :: (NFData a, Show a, Read a, Num a) => a -> V.Vector (a, Digest SHA256) -> [ConditionalGate a] -> V.Vector (a, Digest SHA256)
applyConditionalGates input acc [] = acc
applyConditionalGates input acc (ConditionalGate cond trueG falseG : rest) =
    if cond input
        then applyConditionalGates input (processGate input acc trueG) rest
        else applyConditionalGates input (processGate input acc falseG) rest

applyDynamicGates :: (NFData a, Show a, Read a, Num a) => a -> V.Vector (a, Digest SHA256) -> [DynamicGate a] -> V.Vector (a, Digest SHA256)
applyDynamicGates input acc [] = acc
applyDynamicGates input acc (DynamicGate dynamicFunc : rest) =
    let FoleyCircuit{gates=subGates, conditionalGates=subCondGates, dynamicGates=subDynGates} = dynamicFunc input
        subResult = foldl' (processGate input) acc subGates
        subResultAfterCond = applyConditionalGates input subResult subCondGates
        subResultAfterDyn = applyDynamicGates input subResultAfterCond subDynGates
    in applyDynamicGates input subResultAfterDyn rest


-- Encoding and decoding functions
encodeElement :: (NFData a, Show a) => a -> ByteString
encodeElement element = do
    let inputText = show element
    -- Convert from Char to Text before findHexCode and handle Text in fromMaybe
    let encodedParts = T.unpack $ T.concatMap (fromMaybe (T.pack "Error") . findHexCode . T.singleton) (T.pack inputText)
    C8.pack encodedParts

decodeElement :: (NFData a, Read a) => ByteString -> a
decodeElement encodedData = do
    let hexCodes = T.words $ TE.decodeUtf8 encodedData
    -- Convert from Text to String after decodeHexCode
    let decodedChars = maybe "?" T.unpack . decodeHexCode =<< hexCodes
    read decodedChars
