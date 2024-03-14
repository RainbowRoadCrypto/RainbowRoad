module RainbowBitcoin.Merkle where
{-
module RainbowBitcoin.Merkle
    ( MerkleTree(..)
    , MerkleProof(..)
    , computeMerkleRoot
    , buildMerkleTree
    , getMerkleProof
    , verifyMerkleProof
    ) where

import qualified RainbowRoad.Crypto as Crypto
import RainbowRoad.Types (RainbowRoadState, Proof)
import Data.ByteString (ByteString)

data MerkleTree = MerkleTree
    { root :: ByteString
    , leaves :: [ByteString]
    }

data MerkleProof = MerkleProof
    { proofHashes :: [ByteString]
    , proofIndex :: Int
    }

computeMerkleRoot :: [ByteString] -> ByteString
computeMerkleRoot = root . buildMerkleTree

buildMerkleTree :: [ByteString] -> MerkleTree
buildMerkleTree leaves = MerkleTree (buildTree leaves) leaves
  where
    buildTree [] = error "Empty list of leaves"
    buildTree [leaf] = leaf
    buildTree nodes = buildTree (pairwiseHash nodes)

    pairwiseHash [] = []
    pairwiseHash [x] = [x]
    pairwiseHash (x:y:xs) = Crypto.hash (x <> y) : pairwiseHash xs

getMerkleProof :: RainbowRoadState -> Proof -> MerkleProof
getMerkleProof state proof =
    let stateHash = Crypto.hash (stateData state)
        proofHash = Crypto.hash (proofData proof)
        leaves = [stateHash, proofHash]
        tree = buildMerkleTree leaves
        proofHashes = getProofHashes stateHash tree
        proofIndex = getProofIndex stateHash tree
    in MerkleProof proofHashes proofIndex
  where
    getProofHashes :: ByteString -> MerkleTree -> [ByteString]
    getProofHashes leaf (MerkleTree _ []) = []
    getProofHashes leaf (MerkleTree _ [l]) = []
    getProofHashes leaf (MerkleTree _ (l:r:xs))
        | leaf == l = r : getProofHashes leaf (buildMerkleTree xs)
        | leaf == r = l : getProofHashes leaf (buildMerkleTree xs)
        | otherwise = error "Leaf not found in Merkle tree"

    getProofIndex :: ByteString -> MerkleTree -> Int
    getProofIndex leaf (MerkleTree _ []) = error "Leaf not found in Merkle tree"
    getProofIndex leaf (MerkleTree _ [l]) = if leaf == l then 0 else error "Leaf not found in Merkle tree"
    getProofIndex leaf (MerkleTree _ (l:r:xs))
        | leaf == l = 0
        | leaf == r = 1
        | otherwise = getProofIndex leaf (buildMerkleTree xs) * 2

verifyMerkleProof :: ByteString -> MerkleProof -> ByteString -> Bool
verifyMerkleProof root (MerkleProof hashes index) leaf =
    let calculatedRoot = foldl hashPair leaf (zip hashes (map odd [index..]))
    in calculatedRoot == root
  where
    hashPair x (h, isRight) = if isRight then Crypto.hash (x <> h) else Crypto.hash (h <> x)
    odd n = n `mod` 2 == 1
-}