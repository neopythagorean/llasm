{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Assembler
import System.Environment

import System.Console.CmdArgs

data LLASM = LLASM
    { inputFile :: String
    , outputFile :: String
    } deriving (Data, Typeable, Show, Eq)


argsDef = LLASM { inputFile = def &= args &= typ "FILE"
                , outputFile = "a.bin" &= help "Output file location"
                }

main :: IO ()
main = do
    args <- cmdArgs argsDef
    contents <- readFile $ inputFile args
    writeMLOut (outputFile args) $ assemble contents





