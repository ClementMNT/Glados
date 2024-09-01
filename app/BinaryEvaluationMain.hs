{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- GLaDOS
-}

module Main (main) where

import BinaryEvaluation

import System.Environment
import System.Exit


import qualified Data.ByteString as BS
import Data.Word (Word8)


readFileAndCheckMagicNumber :: String -> IO [Word8]
readFileAndCheckMagicNumber filename = BS.readFile filename >>= \contents ->
    let bytecode = byteStringToWord8List contents
    in if checkMagicNumber (take 4 bytecode) == False
        then putStrLn "Magic number is incorrect" >> exitWith (ExitFailure 84)
        else return bytecode

evaluateBytecode :: [Word8] -> IO StackTable
evaluateBytecode b = evalEachValue b (drop headerSize b) [] headerSize [[]]

exitBasedOnStack :: StackTable -> IO ()
exitBasedOnStack stack | length stack < 1 = exitWith (ExitFailure 84)
        | (getLastIntFromStack stack) == 0 = exitWith (ExitSuccess)
        | otherwise = exitWith (ExitFailure (getLastIntFromStack stack))

evalAndExit :: [Word8] -> IO ()
evalAndExit bytecode = evaluateBytecode bytecode >>= exitBasedOnStack

main :: IO ()
main = getArgs >>= \args ->
    case args of
        [filename] -> readFileAndCheckMagicNumber filename >>= evalAndExit
        _ -> putStrLn "No file given as an argument"