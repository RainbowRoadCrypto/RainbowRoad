module Base.BaseLayer where
{-
module Base.BaseLayer
    ( BaseLayerConfig(..)
    , BaseLayerTx(..)
    , anchorToBaseLayer
    , verifyBaseLayerTx
    ) where

import RainbowRoad.Types (RainbowRoadState, Proof)
import qualified RainbowRoad.Crypto as Crypto
import qualified BaseLayer.Ethereum as Ethereum
import qualified BaseLayer.Bitcoin as Bitcoin
import Control.Concurrent.Async (async)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)

data BaseLayerConfig = EthereumConfig Ethereum.Config | BitcoinConfig Bitcoin.Config

data BaseLayerTx = BaseLayerTx
    { txHash :: ByteString
    , payload :: ByteString
    }

anchorToBaseLayer :: BaseLayerConfig -> RainbowRoadState -> Proof -> ExceptT String IO BaseLayerTx
anchorToBaseLayer config state proof = do
    case config of
        EthereumConfig ethConfig -> do
            let stateHash = Crypto.hash state
            let proofHash = Crypto.hash proof
            let payload = Ethereum.encodePayload stateHash proofHash
            txHash <- liftIO $ Ethereum.submitTx ethConfig payload
            return $ BaseLayerTx txHash payload
        BitcoinConfig btcConfig -> do
            let stateHash = Crypto.hash state
            let proofHash = Crypto.hash proof
            let payload = Bitcoin.encodePayload stateHash proofHash
            txHash <- liftIO $ Bitcoin.submitTx btcConfig payload
            return $ BaseLayerTx txHash payload

verifyBaseLayerTx :: BaseLayerConfig -> BaseLayerTx -> ExceptT String IO Bool
verifyBaseLayerTx config (BaseLayerTx txHash payload) = do
    case config of
        EthereumConfig ethConfig -> do
            txValid <- liftIO $ Ethereum.verifyTx ethConfig txHash
            payloadValid <- liftIO $ Ethereum.verifyPayload ethConfig payload
            return $ txValid && payloadValid
        BitcoinConfig btcConfig -> do
            txValid <- liftIO $ Bitcoin.verifyTx btcConfig txHash
            payloadValid <- liftIO $ Bitcoin.verifyPayload btcConfig payload
            return $ txValid && payloadValid
-}