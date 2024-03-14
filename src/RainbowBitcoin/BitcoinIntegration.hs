module RainbowBitcoin.BitcoinIntegration where
{-
module RainbowBitcoin.BitcoinIntegration
    ( integrateWithBitcoin
    , verifyBitcoinAnchor
    ) where

import RainbowRoad.Types (RainbowRoadState)
import qualified RainbowRoad.BitcoinAnchor as BtcAnchor
import Control.Concurrent.Async (async)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)

integrateWithBitcoin :: BtcAnchor.BtcAnchorConfig -> RainbowRoadState -> ExceptT String IO ()
integrateWithBitcoin config state = do
    proof <- liftIO $ generateProof state
    btcAnchorTx <- BtcAnchor.anchorToBitcoin config state proof
    verified <- BtcAnchor.verifyBtcAnchorTx config btcAnchorTx
    if verified
        then return ()
        else throwError "Failed to integrate with Bitcoin"

verifyBitcoinAnchor :: BtcAnchor.BtcAnchorConfig -> BtcAnchor.BtcAnchorTx -> ExceptT String IO Bool
verifyBitcoinAnchor = BtcAnchor.verifyBtcAnchorTx
-}