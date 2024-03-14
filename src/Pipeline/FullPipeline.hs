{-# LANGUAGE OverloadedStrings #-}

module Pipeline.FullPipeline 
    ( parseAST
    , astToIR
    , irToText
    , encodeToRainbowCode
    , decodeFromRainbowCode
    , parseRawCodeToAST
    , irToRainbowCode
    , rainbowCodeToIR
    , main
    ) where

import Data.Text (Text)
import qualified Data.Text as T

-- Mock implementations for AST and IR conversion functions, and for encoding/decoding.
parseAST :: Text -> Either String Text
parseAST code = Right $ "AST for: " <> code

astToIR :: Text -> Text
astToIR ast = "IR representation of " <> ast

irToText :: Text -> Text
irToText ir = ir

textToIR :: Text -> Either String Text
textToIR = Right

encodeToRainbowCode :: Text -> IO Text
encodeToRainbowCode text = return $ "Encoded Rainbow Code of: " <> text

decodeFromRainbowCode :: Text -> IO (Either String Text)
decodeFromRainbowCode encoded = return . Right $ T.drop (T.length "Encoded Rainbow Code of: ") encoded

-- New functions based on the types provided earlier.
parseRawCodeToAST :: Text -> Either String Text
parseRawCodeToAST = parseAST

irToRainbowCode :: Text -> IO Text
irToRainbowCode ir = encodeToRainbowCode (irToText ir)

rainbowCodeToIR :: Text -> IO (Either String Text)
rainbowCodeToIR = decodeFromRainbowCode

-- Demonstrates the full pipeline from parsing through IR conversion, encoding, and decoding.
main :: IO ()
main = do
    let exampleCode = "function example() { return 42; }"
    putStrLn "Starting pipeline..."
    case parseRawCodeToAST exampleCode of
        Left err -> putStrLn $ "Parsing error: " ++ err
        Right ast -> do
            putStrLn "AST parsed successfully."
            let ir = astToIR ast
            encoded <- irToRainbowCode ir
            putStrLn $ "IR encoded to Rainbow Code: " ++ T.unpack encoded
            decodedIRResult <- rainbowCodeToIR encoded
            case decodedIRResult of
                Left err -> putStrLn $ "Decoding error: " ++ err
                Right decodedIR -> putStrLn $ "IR decoded successfully: " ++ T.unpack (irToText decodedIR)
