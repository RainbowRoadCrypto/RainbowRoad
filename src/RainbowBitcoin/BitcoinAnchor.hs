module RainbowBitcoin.BitcoinAnchor where
{-
module RainbowBitcoin.BitcoinAnchor
    ( BtcAnchorConfig(..)
    , BtcAnchorTx(..)
    , anchorToBitcoin
    , verifyBtcAnchorTx
    ) where

import RainbowRoad.Types (RainbowRoadState, Proof)
import qualified RainbowRoad.Crypto as Crypto
import qualified RainbowRoad.Merkle as Merkle
import qualified Bitcoin.Network as BtcNetwork
import qualified Bitcoin.Script as BtcScript
import Control.Concurrent.Async (async)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)

data BtcAnchorConfig = BtcAnchorConfig
    { btcNetwork :: BtcNetwork.Network
    , btcFee :: Int
    , btcChangeAddress :: String
    }

data BtcAnchorTx = BtcAnchorTx
    { txHash :: ByteString
    , blockHeight :: Int
    , outputIndex :: Int
    }

anchorToBitcoin :: BtcAnchorConfig -> RainbowRoadState -> Proof -> ExceptT String IO BtcAnchorTx
anchorToBitcoin config state proof = do
    let stateHash = Crypto.hash state
    let proofHash = Crypto.hash proof
    let merkleRoot = Merkle.computeMerkleRoot [stateHash, proofHash]
    let payload = Crypto.encodeBytes merkleRoot
    let script = BtcScript.scriptOpReturn payload
    let btcTx = BtcNetwork.buildTx (btcNetwork config) [(script, 0)] (btcChangeAddress config) (btcFee config)
    txHash <- liftIO $ BtcNetwork.broadcastTx (btcNetwork config) btcTx
    let blockHeight = -1  -- Retrieve the block height asynchronously
    let outputIndex = 0  -- Assuming the OP_RETURN output is at index 0
    return $ BtcAnchorTx txHash blockHeight outputIndex

verifyBtcAnchorTx :: BtcAnchorConfig -> BtcAnchorTx -> ExceptT String IO Bool
verifyBtcAnchorTx config (BtcAnchorTx txHash blockHeight outputIndex) = do
    tx <- liftIO $ BtcNetwork.getTx (btcNetwork config) txHash
    case tx of
        Just btcTx -> do
            let output = BtcScript.txOutputs btcTx !! outputIndex
            case BtcScript.outputScript output of
                BtcScript.Script script -> do
                    case BtcScript.parseOpReturn script of
                        Just payload -> do
                            let expectedPayload = Crypto.encodeBytes $ Merkle.computeMerkleRoot [Crypto.hash state, Crypto.hash proof]
                            return $ payload == expectedPayload
                        Nothing -> return False
                _ -> return False
        Nothing -> return False
-}