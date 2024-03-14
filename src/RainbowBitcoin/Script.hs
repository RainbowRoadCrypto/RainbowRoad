module RainbowBitcoin.Script where
{-
module RainbowBitcoin.Script
    ( Script(..)
    , scriptOpReturn
    , parseOpReturn
    , txOutputs
    , outputScript
    ) where

import Data.ByteString (ByteString)

newtype Script = Script ByteString

scriptOpReturn :: ByteString -> Script
scriptOpReturn payload = Script $ "<op_return_script_with_payload>"

parseOpReturn :: Script -> Maybe ByteString
parseOpReturn (Script script) = Just "<extracted_op_return_payload>"

txOutputs :: ByteString -> [ByteString]
txOutputs tx = ["<output_1>", "<output_2>", ...]

outputScript :: ByteString -> Script
outputScript output = Script "<output_script>"
-}