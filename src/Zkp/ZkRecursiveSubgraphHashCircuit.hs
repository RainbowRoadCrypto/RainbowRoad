{-# LANGUAGE OverloadedStrings #-}

module Zkp.ZkRecursiveSubgraphHashCircuit where

import Crypto.Hash (hash, Digest, SHA256)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC -- Added for Char8 operations
import Data.Word (Word8)
import Data.Bits (xor)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map
import Pipeline.FullPipeline (parseRawCodeToAST, astToIR, irToRainbowCode, rainbowCodeToIR)
import Types.Types (Circuit(..), Output(..), Input(..)) -- Assuming addGate is now properly imported
import Plonk.Functions (createCircuit, executeCircuit, addGate)
-- Function to apply XOR to each byte of a ByteString with a given byte
byteStringXor :: ByteString -> Word8 -> ByteString
byteStringXor input byte = B.map (`xor` byte) input

-- Process source code into a hashed format suitable for ZKP circuits using "Rainbow Code"
processSourceCodeForZkp :: String -> IO (Either String (Digest SHA256))
processSourceCodeForZkp sourceCode = do
  case parseRawCodeToAST (T.pack sourceCode) of
    Left err -> return $ Left err
    Right ast -> do
      let ir = astToIR ast
      encodedIR <- irToRainbowCode ir
      return $ Right $ hash (TE.encodeUtf8 encodedIR)

createRecursiveSubgraphHashCircuit :: Circuit
createRecursiveSubgraphHashCircuit = createCircuit -- Placeholder for circuit creation logic

exampleSourceCodeHashing :: String -> IO ()
exampleSourceCodeHashing sourceCode = do
  result <- processSourceCodeForZkp sourceCode
  case result of
    Right hashed -> putStrLn $ "Hashed 'Rainbow Code' representation: " ++ show hashed
    Left error -> putStrLn $ "Error processing source code: " ++ error

main :: IO ()
main = do
    let circuit = createCircuit

    let xorOp input = byteStringXor input 0xFF
    let meta = Map.fromList [("description", "XOR gate")]

    let (circuitWithGate, gateId) = addGate xorOp meta circuit

    -- Use BC.pack for converting String to ByteString
    let inputs = [Input $ BC.pack "test input"]

    Output out <- executeCircuit circuitWithGate inputs

    -- Use BC.unpack for converting ByteString to String
    putStrLn $ "Circuit output: " ++ BC.unpack out
