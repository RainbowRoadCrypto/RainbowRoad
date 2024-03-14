{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (isLeft, isRight)
import Servant.Server.Internal.Handler (runHandler)
import Servant (errReasonPhrase)
import qualified Data.Text as T
import Control.Exception (catch, SomeException)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe, fromJust)
import Data.Ini (readIniFile, Ini)
import Text.Parsec (runParser)
import qualified Data.Text.Encoding as TE
-- Importing custom modules
import qualified Types.Types as Types
    ( ASTNode(..)
    , LoopType(..)
    , OperationType(..)
    , LiteralType(..)
    , FunctionName(..)
    , VariableName(..)
    , TypeName(..)
    , ASTParameter(..)
    , Type(..)
    , Operator(..)
    , Literal(..)
    , Parameter(..)
    , IR(..)
    , Gate(..)
    , Wire(..)
    , WireType(..)
    , Circuit(..)
    , Input(..)
    , Output(..)
    , Proof(..)
    , SecretKey(..)
    , PublicKey(..)
    , Prover(..)
    , Verifier(..)
    , ErrorMessage(..)
    )
import qualified Encoding.EncodingService as Encoding
import qualified Visualize.VisualAid as Visualize
import qualified Zkp.ZkRecursiveSubgraphHashCircuit as Zkp
import qualified Pipeline.FullPipeline as Pipeline
import qualified Encoding.ASTandIR as ASTandIR (astNodeToIR, irToText)
import qualified Ast.ASTToRainbowToPLONK as ASTToPlonk
import qualified Plonk.Functions as PLONKF

-- Enhanced error handling to catch all exceptions
handleError :: SomeException -> IO ()
handleError e = putStrLn $ "An error occurred: " ++ show e

-- Load configuration settings
loadConfig :: IO (Maybe Ini)
loadConfig = readIniFile "config.ini" >>= either (const $ return Nothing) (return . Just)

-- Interactive command-line input with validation
promptForInput :: String -> IO String
promptForInput message = do
    putStr message
    hFlush stdout
    getLine

-- Main function to orchestrate the command-line interface
main :: IO ()
main = do
    putStrLn "Welcome to RainbowRoad!"
    config <- loadConfig
    args <- getArgs
    processArgs config args `catch` handleError

processArgs :: Maybe Ini -> [String] -> IO ()
processArgs config ["interactive"] = interactiveMode config
processArgs config (cmd:args) = handleCommand config cmd args
processArgs _ [] = putStrLn "No command provided. Usage: Main [command] <arguments>... or Main interactive"

-- Interactive mode for command-line interface
interactiveMode :: Maybe Ini -> IO ()
interactiveMode config = do
    command <- promptForInput "Enter command (or 'quit' to exit): "
    unless (command == "quit") $ do
        args <- promptForInput "Enter arguments separated by space: "
        handleCommand config command (words args)
        interactiveMode config

-- Modular command handling
handleCommand :: Maybe Ini -> String -> [String] -> IO ()
handleCommand _ "parse" [c] = parseASTCommand c
handleCommand _ "map" [c] = mapASTCommand c
handleCommand _ "plaintext" [c] = plaintextCommand c
handleCommand _ "runPLONK" [c] = runPLONKCommand c
handleCommand _ "findHex" [input] = findHexCommand input
handleCommand _ "decodeHex" [hex] = decodeHexCommand hex
handleCommand _ "visualize" [c] = visualizeCommand c
handleCommand _ "zkp" [c] = zkpCommand c
handleCommand _ "pipeline" [c] = pipelineCommand c
handleCommand _ "convert" [c] = convertCommand c
handleCommand _ _ _ = putStrLn "Unknown command or incorrect usage."

-- Parse AST command
parseASTCommand :: String -> IO ()
parseASTCommand code = do
    putStrLn "Parsing AST..."
    case runParser ASTToPlonk.astParser () "" code of
        Left err -> putStrLn $ "Parsing error: " ++ show err
        Right ast -> do
            putStrLn "AST:"
            print ast

-- Map AST command
mapASTCommand :: String -> IO ()
mapASTCommand code = do
    putStrLn "Mapping AST..."
    case runParser ASTToPlonk.astParser () "" code of
        Left err -> putStrLn $ "Parsing error: " ++ show err
        Right ast -> do
            putStrLn "Mapped AST:"
            print $ Visualize.visualizeAST ast

-- Plaintext command
plaintextCommand :: String -> IO ()
plaintextCommand code = do
    putStrLn "Decoding Rainbow Code to plaintext..."
    result <- Pipeline.decodeFromRainbowCode (T.pack code)
    case result of
        Left errorMsg -> putStrLn $ "Error decoding Rainbow Code: " ++ errorMsg
        Right text -> do
            putStrLn "Plaintext:"
            TIO.putStrLn text

-- Run PLONK command
runPLONKCommand :: String -> IO ()
runPLONKCommand code = do
    putStrLn "Running PLONK circuit..."
    let inputs = [Types.Input $ TE.encodeUtf8 (T.pack code)]
    let circuit = PLONKF.createCircuit
    output <- PLONKF.executeCircuit circuit inputs
    putStrLn "PLONK output:"
    print output

-- Find hex code command
findHexCommand :: String -> IO ()
findHexCommand input = do
    putStrLn "Finding hex code..."
    let hex = Encoding.findHexCode (T.pack input)
    case hex of
        Just hexCode -> do
            putStrLn "Hex code found:"
            TIO.putStrLn hexCode
        Nothing -> putStrLn "Hex code not found."

-- Decode hex code command
decodeHexCommand :: String -> IO ()
decodeHexCommand hex = do
    putStrLn "Decoding hex code..."
    let decoded = Encoding.decodeHexCode (T.pack hex)
    case decoded of
        Just decodedText -> do
            putStrLn "Decoded text:"
            TIO.putStrLn decodedText
        Nothing -> putStrLn "Decoding failed."

-- Visualize AST command
visualizeCommand :: String -> IO ()
visualizeCommand code = do
    putStrLn "Visualizing AST..."
    case runParser ASTToPlonk.astParser () "" code of
        Left err -> putStrLn $ "Parsing error: " ++ show err
        Right ast -> do
            putStrLn "Visualized AST:"
            TIO.putStrLn $ Visualize.visualizeAST ast

-- ZKP command
zkpCommand :: String -> IO ()
zkpCommand code = do
    putStrLn "Processing source code for ZKP..."
    result <- Zkp.processSourceCodeForZkp code
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right hashed -> do
            putStrLn "Hashed 'Rainbow Code' representation:"
            TIO.putStrLn $ T.pack (show hashed)
            putStrLn "ZKP circuit generated successfully."

-- Pipeline command
pipelineCommand :: String -> IO ()
pipelineCommand code = do
    putStrLn "Running the full pipeline..."
    case Pipeline.parseRawCodeToAST (T.pack code) of
        Left err -> putStrLn $ "Parsing error: " ++ err
        Right ast -> do
            putStrLn "AST parsed successfully."
            let ir = Pipeline.astToIR ast
            encoded <- Pipeline.irToRainbowCode ir
            putStrLn $ "IR encoded to Rainbow Code: " ++ T.unpack encoded
            decodedIRResult <- Pipeline.rainbowCodeToIR encoded
            case decodedIRResult of
                Left err -> putStrLn $ "Decoding error: " ++ err
                Right decodedIR -> do
                    putStrLn $ "IR decoded successfully: " ++ T.unpack (Pipeline.irToText decodedIR)
                    putStrLn "Pipeline completed successfully."

-- Convert command
convertCommand :: String -> IO ()
convertCommand code = do
    putStrLn "Converting AST to IR..."
    case runParser ASTToPlonk.astParser () "" code of
        Left err -> putStrLn $ "Parsing error: " ++ show err
        Right ast -> do
            case ASTandIR.astNodeToIR ast of
                Just irValue -> do
                    putStrLn "Converted IR:"
                    print irValue
                    putStrLn "Converted IR (text format):"
                    TIO.putStrLn $ ASTandIR.irToText irValue
                Nothing -> putStrLn "Failed to convert AST to IR."