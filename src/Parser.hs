{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- GLaDOS
-}

module Parser (
    parser,
) where

import ParserToken
import ParserAST
import System.Exit
import System.IO
import Types
import Control.Exception
-- import Debug.Trace

-- INFO: Main function
checkSyntax :: [Token] -> IO ()
checkSyntax xs
    | IncludeToken `elem` xs =
        throw $ (ParserError $ "Error: #include token must be " ++
        "followed by a string for the file to include")
checkSyntax xs
    | length (filter (== OpenParenthesis) xs) <
        length (filter (== CloseParenthesis) xs) =
            throw $ (ParserError "Error: Missing ( token")
checkSyntax xs
    | length (filter (== OpenParenthesis) xs) >
        length (filter (== CloseParenthesis) xs) =
            throw $ (ParserError "Error: Missing ) token")
checkSyntax xs
    | length (filter (== OpenBracket) xs) <
        length (filter (== CloseBracket) xs) =
            throw $ (ParserError "Error: Missing [ token")
checkSyntax xs
    | length (filter (== OpenBracket) xs) >
        length (filter (== CloseBracket) xs) =
            throw $ (ParserError "Error: Missing ] token")
checkSyntax xs
    | length (filter (== OpenBraces) xs) <
        length (filter (== CloseBraces) xs) =
            throw $ (ParserError "Error: Missing { token")
checkSyntax xs
    | length (filter (== OpenBraces) xs) >
        length (filter (== CloseBraces) xs) =
            throw $ (ParserError "Error: Missing } token")
checkSyntax _ = return ()

printFile :: File -> String -> IO (String)
printFile file filename = do
    absoluteFilename <- getAbsolutePath filename
    putStrLn "Original file: "
    putStrLn absoluteFilename
    putStrLn "------------------------------------"
    putStrLn $ show file
    putStrLn "------------------------------------"
    putStrLn $ show $ cleanFile file False
    putStrLn "------------------------------------"
    return (absoluteFilename)

printTokenListAndSexpr :: [Token] -> IO ()
printTokenListAndSexpr tokenList = putStrLn (show $ tokenList) >>
    putStrLn "------------------------------------" >>
    putStrLn (show $ sexpr) >>
    putStrLn "------------------------------------" >>
    putStrLn (printAST $ sexprToAst sexpr) >>
    putStrLn "------------------------------------"
    where
        sexpr = tokenListToSexpr tokenList

parser :: File -> String -> IO (AST)
parser file filename = catch (do
        absoluteFilename <- printFile file filename
        let cleanedFile = cleanFile file False
        tokenList <- parseFile cleanedFile 1 [absoluteFilename] file
        checkSyntax tokenList
        printTokenListAndSexpr tokenList
        return (sexprToAst $ tokenListToSexpr tokenList)) handler
    where
        handler :: ParserError -> IO (AST)
        handler e = hPutStrLn stderr (show e) >> exitWith (ExitFailure 84)
