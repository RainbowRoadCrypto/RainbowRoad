{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Plonk.Custom where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Crypto.Hash (hashWith, SHA256(..), Digest)

encodingOperation :: ByteString -> (ByteString, Digest SHA256)
encodingOperation input = (input, hashWith SHA256 input)

-- Definition for sha256Operation is required
sha256Operation :: (ByteString, Digest SHA256) -> Digest SHA256
sha256Operation (_, digest) = digest

data CustomGate a b where
  CustomGate :: (a -> b) -> CustomGate a b

encodingGate :: CustomGate ByteString (ByteString, Digest SHA256)
encodingGate = CustomGate encodingOperation

sha256Gate :: CustomGate (ByteString, Digest SHA256) (Digest SHA256)
sha256Gate = CustomGate sha256Operation

data CustomFoleyCircuit a b where
  EmptyCircuit :: CustomFoleyCircuit a a
  CustomGateCircuit :: CustomGate a b -> CustomFoleyCircuit b c -> CustomFoleyCircuit a c

addCustomGate :: CustomGate a b -> CustomFoleyCircuit b c -> CustomFoleyCircuit a c
addCustomGate = CustomGateCircuit

runCustomFoleyCircuit :: CustomFoleyCircuit a b -> a -> b
runCustomFoleyCircuit EmptyCircuit x = x
runCustomFoleyCircuit (CustomGateCircuit (CustomGate f) next) x = runCustomFoleyCircuit next (f x)

exampleUsage :: IO ()
exampleUsage = do
  let circuit = EmptyCircuit
  -- Correct the order and type of gates
  let encodingCircuit = addCustomGate encodingGate circuit
  -- Correctly use the circuit with the appropriate input type
  let input = C8.pack "Hello, World!"
  let (encoded, digest) = runCustomFoleyCircuit encodingCircuit input
  let hashedOutput = sha256Operation (encoded, digest)
  putStrLn $ "Hashed output: " ++ show hashedOutput
