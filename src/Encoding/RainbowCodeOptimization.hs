{-
{-# LANGUAGE OverloadedStrings #-}

module Encoding.RainbowCodeOptimization
    ( optimizeRainbowCode
    , RainbowCodeOptimizationError(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, runState, get, put)
import Control.Parallel.Strategies (parMap, rseq)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data RainbowCodeOptimizationError = InvalidRainbowCode | OptimizationFailure
    deriving (Show, Eq, Generic, NFData)

optimizeRainbowCode :: Text -> Either RainbowCodeOptimizationError Text
optimizeRainbowCode rainbowCode = do
    let (result, _) = runState (runExceptT $ optimize rainbowCode) ()
    case result of
        Left err -> Left err
        Right optimizedCode -> Right optimizedCode

optimize :: Text -> ExceptT RainbowCodeOptimizationError (State ()) Text
optimize rainbowCode = do
    validatedCode <- validateRainbowCode rainbowCode
    let chunks = T.chunksOf 1024 validatedCode
    let optimizedChunks = parMap rseq optimizeChunk chunks
    return $ T.concat optimizedChunks

validateRainbowCode :: Text -> ExceptT RainbowCodeOptimizationError (State ()) Text
validateRainbowCode rainbowCode = do
    let isValid = T.all (\c -> c == '#' || c `elem` ['0'..'9'] ++ ['A'..'F']) rainbowCode
    if isValid
        then return rainbowCode
        else throwError InvalidRainbowCode

optimizeChunk :: Text -> Text
optimizeChunk chunk = do
    let hexValues = T.chunksOf 6 chunk
    let optimizedHexValues = mapMaybe shortenHexValue hexValues
    T.concat optimizedHexValues

shortenHexValue :: Text -> Maybe Text
shortenHexValue hexValue = do
    let (r, g, b) = (T.take 2 hexValue, T.take 2 $ T.drop 2 hexValue, T.take 2 $ T.drop 4 hexValue)
    if r == g && g == b
        then Just $ "#" <> r
        else Just hexValue
-}