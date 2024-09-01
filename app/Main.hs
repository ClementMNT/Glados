{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- GLaDOS
-}

module Main (main) where

import System.Environment
import Types
import AstToBytecode
import BytecodeToBinary
import Parser
import ParserToken (getAbsolutePath)

-- INFO: Main function
argManager :: [String] -> Int -> IO ()
argManager ("-h" : remainingArgs) n =
    putStrLn ("Usage: ./glados [-h] [<file> -o <output> | <file>] ...") >>
    putStrLn ("\nArguments:") >>
    putStrLn ("\t<file>\n\t\tFichier à compiler") >> putStrLn ("\nOptions:") >>
    putStrLn ("\t-h") >>
    putStrLn ("\t\tAffiche ce message d'aide") >> putStrLn ("\t-o <output>") >>
    putStrLn ("\t\tSpécifie le nom du fichier de sortie du binaire") >>
    putStr ("\t\tPar défaut, le fichier de sortie aura le même nom ") >>
    putStrLn ("que le fichier d'entrée avec l'extension .bin") >>
    argManager remainingArgs (n + 1)
argManager (filename : "-o" : name : remainingArgs) n = do
    contents <- readFile filename
    let file = File (lines contents)
    ast <- parser file filename
    putStrLn ("AST:") >> putStrLn (show ast)
    let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
    putStrLn ("Bytecode:") >> putStrLn (show bytecode)
    bytecodeToBinary bytecode name
    argManager remainingArgs (n + 1)
argManager ("-o" : _ : remainingArgs) n = argManager remainingArgs (n)
argManager (filename : remainingArgs) n = do
    contents <- readFile filename
    let file = File (lines contents)
    ast <- parser file filename
    putStrLn ("AST:") >> putStrLn (show ast)
    let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
    putStrLn ("Bytecode:") >> putStrLn (show bytecode)
    absoluteFilename <- getAbsolutePath filename
    bytecodeToBinary bytecode absoluteFilename
    argManager remainingArgs (n + 1)
argManager [] 0 = putStrLn "No file given as an argument"
argManager [] _ = putStrLn "Compilation finished!"

main :: IO ()
main = do
    args <- getArgs
    argManager args 0
