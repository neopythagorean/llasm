module Main where

import Assembler
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if null args then printUsage
    else do
        contents <- readFile $ head args
        writeMLOut "a.bin" $ assemble contents


