{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- GLaDOS
-}

module ParserToken (
    getAbsolutePath,
    cleanFile,

    parseKeyword,
    parseIntToken,
    parseSymbolToken,
    parseStringToken,
    parseCharToken,

    parseToken,

    parseLine,
    mergeSymbols,
    parseFile,

    getSubList,
    tokenListToSexpr,
) where

import ParserModule
import Types
import Control.Applicative
import Data.Char (chr)
import Control.Exception
-- import Debug.Trace

import System.FilePath (normalise)
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)

getAbsolutePath :: FilePath -> IO FilePath
getAbsolutePath relativePath = do
    currentDir <- getCurrentDirectory
    return $ normalise (currentDir </> relativePath)


cleanLine :: String -> Bool -> (Bool, String)
cleanLine [] isInComment = (isInComment, [])
cleanLine ('/':ys) False | (length ys) > 0 && head ys == '/' =
    (False, [])
cleanLine ('/':ys) False | (length ys) > 0 && head ys == '*' =
    (True, (snd (cleanLine (tail ys) True)))
cleanLine ('*':ys) True | (length ys) > 0 && head ys == '/' =
    (False, " " ++ (snd (cleanLine (tail ys) False)))
cleanLine (_:ys) True =
    (True, (snd (cleanLine ys True)))
cleanLine (y:ys) isInComment =
    (isInComment, [y] ++ (snd (cleanLine ys isInComment)))

getFileArray :: File -> [String]
getFileArray (File y) = y

-- Define the function to clean the file from all comments
cleanFile :: File -> Bool -> File
cleanFile (File []) _ = File []
cleanFile (File (x:xs)) inComment =
    File ([line] ++ (getFileArray (cleanFile (File xs) stillInComment)))
    where
        (stillInComment, line) = cleanLine x inComment

-- INFO: Token Parser
parseKeyword :: String -> Token -> Parser Token
parseKeyword str token = Parser f
    where
        f x = case runParser (parseString str) x of
            Just (_, xs) -> Just (token, xs)
            Nothing -> Nothing

parseFloatToken :: Parser Token
parseFloatToken = Parser f
    where
        f x = do
            num <- runParser parseInt x
            dot <- runParser (parseChar '.') (snd num)
            num2 <- runParser parseUInt (snd dot)
            Just (FloatToken (fromIntegral (fst num) + (fromIntegral
                (fst num2) / (10 ^ (length (show (fst num2)))))), snd num2)

parseIntToken :: Parser Token
parseIntToken = Parser f
    where
        f x = case runParser parseInt x of
            Just (y, ys) -> Just (IntToken y, ys)
            Nothing -> Nothing

getUTF16Except :: [Char] -> [Char]
getUTF16Except xs = [chr i | i <- [0x0000 .. 0x10FFFF], not (chr i `elem` xs)]

parseSymbolToken :: Parser Token
parseSymbolToken = Parser f
    where
        -- parse absolutely any char in UTF-16
        f x = case runParser (parseAnyChar
            [chr i | i <- [0x0000 .. 0x10FFFF]]) x of
            Just (y, ys) -> Just (SymbolToken [y], ys)
            Nothing -> Nothing

parseStringToken :: Parser Token
parseStringToken = Parser f
    where
        f x = case runParser (parseChar '\"') x of
            Just (_, ys) -> case runParser (parseMany (parseEscapedChar <|>
                parseAnyChar (getUTF16Except "\""))) ys of
                Just (z, zs) -> case runParser (parseChar '\"') zs of
                    Just (_, ws) -> Just (StringToken z, ws)
                    Nothing -> throw (ParserError "Missing \" token")
                Nothing -> Nothing
            Nothing -> Nothing

parseCharToken :: Parser Token
parseCharToken = Parser f
    where
        f x = case runParser (parseChar '\'') x of
            Just (_, ys) -> case runParser (parseEscapedChar <|>
                parseAnyChar (getUTF16Except "\'")) ys of
                Just (z, zs) -> case runParser (parseChar '\'') zs of
                    Just (_, ws) -> Just (CharToken z, ws)
                    Nothing -> throw (ParserError "Missing ' token")
                Nothing -> Nothing
            Nothing -> Nothing

parseTokenKeywords :: Parser Token
parseTokenKeywords = (parseKeyword "#define" DefineToken)
    <|> (parseKeyword "#include" IncludeToken)
    <|> (parseKeyword "if" IfToken)
    <|> (parseKeyword "else" ElseToken)
    <|> (parseKeyword "for" ForToken)
    <|> (parseKeyword "while" WhileToken)
    <|> (parseKeyword "fun" FunToken)
    <|> (parseKeyword "return" ReturnToken)
    <|> (parseKeyword ":" FunTypeToken)
    <|> empty

parseTokenTypes :: Parser Token
parseTokenTypes = (parseKeyword "int" IntTypeToken)
    <|> (parseKeyword "float" FloatTypeToken)
    <|> (parseKeyword "char" CharTypeToken)
    <|> (parseKeyword "string" StringTypeToken)
    <|> (parseKeyword "void" VoidTypeToken)
    <|> empty

-- parseTokenComments :: Parser Token
-- parseTokenComments = (parseKeyword "/*" CommentStart)
--     <|> (parseKeyword "*/" CommentEnd)
--     <|> (parseKeyword "//" InlineCommentStart)
--     <|> empty

parseTokenSeparator :: Parser Token
parseTokenSeparator = (parseKeyword "," CommaToken)
    <|> (parseKeyword ";" LineSeparator)
    <|> empty

parseTokenSpacer :: Parser Token
parseTokenSpacer = (parseKeyword " " SpaceToken)
    <|> (parseKeyword "\t" SpaceToken)
    <|> empty

parseTokenOperators1 :: Parser Token
parseTokenOperators1 = (parseKeyword "++" IncrementToken)
    <|> (parseKeyword "--" DecrementToken)
    <|> (parseKeyword "+=" PlusEqualToken)
    <|> (parseKeyword "-=" MinusEqualToken)
    <|> (parseKeyword "*=" TimesEqualToken)
    <|> (parseKeyword "/=" DivideEqualToken)
    <|> (parseKeyword "%=" ModuloEqualToken)
    <|> (parseKeyword "&&" AndToken)
    <|> (parseKeyword "||" OrToken)
    <|> empty

parseTokenOperators2 :: Parser Token
parseTokenOperators2 = (parseKeyword "&" BitAndToken)
    <|> (parseKeyword "|" BitOrToken)
    <|> (parseKeyword "^" BitXorToken)
    <|> (parseKeyword "==" EqualToken)
    <|> (parseKeyword "!=" NotEqualToken)
    <|> (parseKeyword "<=" LessThanEqualToken)
    <|> (parseKeyword ">=" GreaterThanEqualToken)
    <|> (parseKeyword "+" PlusToken)
    <|> (parseKeyword "-" MinusToken)
    <|> empty


parseTokenOperators3 :: Parser Token
parseTokenOperators3 = (parseKeyword "*" TimesToken)
    <|> (parseKeyword "/" DivideToken)
    <|> (parseKeyword "%" ModuloToken)
    <|> (parseKeyword "<" LessThanToken)
    <|> (parseKeyword ">" GreaterThanToken)
    <|> (parseKeyword "!" NotToken)
    <|> (parseKeyword "=" AssignToken)
    <|> empty

parseTokenOperators :: Parser Token
parseTokenOperators = parseTokenOperators1
    <|> parseTokenOperators2
    <|> parseTokenOperators3
    <|> empty

parseTokenList :: Parser Token
parseTokenList = (parseKeyword "(" OpenParenthesis)
    <|> (parseKeyword ")" CloseParenthesis)
    <|> (parseKeyword "[" OpenBracket)
    <|> (parseKeyword "]" CloseBracket)
    <|> (parseKeyword "{" OpenBraces)
    <|> (parseKeyword "}" CloseBraces)
    <|> empty

parseTokenQuotes :: Parser Token
parseTokenQuotes = (parseStringToken)
    <|> (parseCharToken)
    <|> empty

parseTokenNumbers :: Parser Token
parseTokenNumbers = (parseFloatToken)
    <|> (parseIntToken)
    <|> empty

parseTokenSymbols :: Parser Token
parseTokenSymbols = (parseSymbolToken)
    <|> empty

parseToken :: Parser Token
parseToken =
    parseTokenKeywords <|> parseTokenTypes
    -- <|> parseTokenComments
    <|> parseTokenSeparator
    <|> parseTokenSpacer <|> parseTokenOperators
    <|> parseTokenList <|> parseTokenQuotes
    <|> parseTokenNumbers <|> parseTokenSymbols
    <|> empty
    -- -- KeyWords
    -- (parseKeyword "#define" DefineToken)
    -- <|> (parseKeyword "#include" IncludeToken)
    -- <|> (parseKeyword "if" IfToken)
    -- <|> (parseKeyword "else" ElseToken)
    -- <|> (parseKeyword "for" ForToken)
    -- <|> (parseKeyword "while" WhileToken)
    -- <|> (parseKeyword "fun" FunToken)
    -- <|> (parseKeyword "return" ReturnToken)
    -- <|> (parseKeyword ":" FunTypeToken)
    -- -- Types
    -- <|> (parseKeyword "int" IntTypeToken)
    -- <|> (parseKeyword "float" FloatTypeToken)
    -- <|> (parseKeyword "char" CharTypeToken)
    -- <|> (parseKeyword "string" StringTypeToken)
    -- <|> (parseKeyword "void" VoidTypeToken)
    -- -- -- Comments
    -- -- <|> (parseKeyword "/*" CommentStart)
    -- -- <|> (parseKeyword "*/" CommentEnd)
    -- -- <|> (parseKeyword "//" InlineCommentStart)
    -- -- Separator
    -- <|> (parseKeyword "," CommaToken)
    -- -- Line separator
    -- <|> (parseKeyword ";" LineSeparator)
    -- -- Spacer
    -- <|> (parseKeyword " " SpaceToken)
    -- <|> (parseKeyword "\t" SpaceToken)
    -- -- Operators
    -- <|> (parseKeyword "++" IncrementToken)
    -- <|> (parseKeyword "--" DecrementToken)
    -- <|> (parseKeyword "+=" PlusEqualToken)
    -- <|> (parseKeyword "-=" MinusEqualToken)
    -- <|> (parseKeyword "*=" TimesEqualToken)
    -- <|> (parseKeyword "/=" DivideEqualToken)
    -- <|> (parseKeyword "%=" ModuloEqualToken)
    -- <|> (parseKeyword "&&" AndToken)
    -- <|> (parseKeyword "||" OrToken)
    -- <|> (parseKeyword "&" BitAndToken)
    -- <|> (parseKeyword "|" BitOrToken)
    -- <|> (parseKeyword "^" BitXorToken)
    -- <|> (parseKeyword "==" EqualToken)
    -- <|> (parseKeyword "!=" NotEqualToken)
    -- <|> (parseKeyword "<=" LessThanEqualToken)
    -- <|> (parseKeyword ">=" GreaterThanEqualToken)
    -- <|> (parseKeyword "+" PlusToken)
    -- <|> (parseKeyword "-" MinusToken)
    -- <|> (parseKeyword "*" TimesToken)
    -- <|> (parseKeyword "/" DivideToken)
    -- <|> (parseKeyword "%" ModuloToken)
    -- <|> (parseKeyword "<" LessThanToken)
    -- <|> (parseKeyword ">" GreaterThanToken)
    -- <|> (parseKeyword "!" NotToken)
    -- <|> (parseKeyword "=" AssignToken)
    -- -- List
    -- <|> (parseKeyword "(" OpenParenthesis)
    -- <|> (parseKeyword ")" CloseParenthesis)
    -- <|> (parseKeyword "[" OpenBracket)
    -- <|> (parseKeyword "]" CloseBracket)
    -- <|> (parseKeyword "{" OpenBraces)
    -- <|> (parseKeyword "}" CloseBraces)
    -- -- Quotes
    -- <|> (parseStringToken)
    -- <|> (parseCharToken)
    -- -- Numbers
    -- <|> (parseFloatToken)
    -- <|> (parseIntToken)
    -- -- Symbols (others)
    -- <|> (parseSymbolToken)
    -- -- Empty str
    -- <|> empty

-- INFO: Create token list
parseLine :: String -> Int -> String -> String -> IO ([Token])
parseLine str lineNumber filename originalStr =
    catch (case runParser (parseMany parseToken) str of
            Just (x, _) -> return (x)
            Nothing -> throw (ParserError "No token")
        ) handler
    where
        handler :: ParserError -> IO ([Token])
        handler e = throw (ParserError ("Invalid syntax on file: " ++
            show filename ++ " at line " ++ show lineNumber ++ ":\n" ++
            originalStr ++ "\n" ++ show e))

includeFile :: String -> [String] -> IO ([Token])
includeFile str filenames = do
    contents <- readFile str
    let file = File (lines contents)
    putStrLn "File included: "
    putStrLn (str) >> putStrLn "------------------------------------"
    putStrLn (show file) >> putStrLn "------------------------------------"
    putStrLn (show $ cleanFile file False)
    putStrLn "------------------------------------"
    parseFile (cleanFile file False) 1 filenames file

getFilenameAndRest :: [Token] -> String -> [String] -> IO (String, [Token])
getFilenameAndRest xs str filenames = do
    filename <- getAbsolutePath str
    rest <- mergeSymbols (tail (dropWhile (/= StringToken str) xs))
        (filename : filenames)
    return (filename, rest)

getStringToken :: [Token] -> String
getStringToken xs = case head (filter (/= SpaceToken) xs) of
    StringToken x -> x
    _ -> throw (ParserError "Error: Invalid type for include")

mergeSymbols :: [Token] -> [String] -> IO [Token]
mergeSymbols [] _ = return []
-- -- Once we found a InlineCommentStart we ignore all the rest of the line
-- mergeSymbols (InlineCommentStart : _) _ = return []
-- include a file
mergeSymbols (IncludeToken : xs) filenames
    | (length (list)) > 0 && head (list) == StringToken str = do
    (filename, rest) <- getFilenameAndRest xs str filenames
    if filename `elem` filenames then return (rest)
    else do
        includedFile <- includeFile filename (filename : filenames)
        return (includedFile ++ rest)
    where
        list = (filter (/= SpaceToken) xs)
        str = getStringToken xs
-- merge all consecutive symbols (ex: b o n j o u r  -> bonjour)
mergeSymbols (SymbolToken x : SymbolToken y : xs) filenames =
    mergeSymbols (SymbolToken (x ++ y) : xs) filenames
mergeSymbols (SymbolToken x : IntTypeToken : xs) filenames =
    mergeSymbols (SymbolToken (x ++ "int") : xs) filenames
mergeSymbols (SymbolToken x : FloatTypeToken : xs) filenames =
    mergeSymbols (SymbolToken (x ++ "float") : xs) filenames
mergeSymbols (SymbolToken x : CharTypeToken : xs) filenames =
    mergeSymbols (SymbolToken (x ++ "char") : xs) filenames
mergeSymbols (SymbolToken x : StringTypeToken : xs) filenames =
    mergeSymbols (SymbolToken (x ++ "string") : xs) filenames
mergeSymbols (SymbolToken x : VoidTypeToken : xs) filenames =
    mergeSymbols (SymbolToken (x ++ "void") : xs) filenames
mergeSymbols (SymbolToken x : IfToken : xs) filenames =
    mergeSymbols (SymbolToken (x ++ "if") : xs) filenames
mergeSymbols (SymbolToken x : ElseToken : xs) filenames =
    mergeSymbols (SymbolToken (x ++ "else") : xs) filenames
mergeSymbols (SymbolToken x : FunToken : xs) filenames =
    mergeSymbols (SymbolToken (x ++ "fun") : xs) filenames
mergeSymbols (SymbolToken x : ForToken : xs) filenames =
    mergeSymbols (SymbolToken (x ++ "for") : xs) filenames
mergeSymbols (SymbolToken x : WhileToken : xs) filenames =
    mergeSymbols (SymbolToken (x ++ "while") : xs) filenames
mergeSymbols (SymbolToken x : ReturnToken : xs) filenames =
    mergeSymbols (SymbolToken (x ++ "return") : xs) filenames
mergeSymbols (SymbolToken x : IntToken y : xs) filenames =
    mergeSymbols (SymbolToken (x ++ show y) : xs) filenames

mergeSymbols (IntTypeToken : SymbolToken x : xs) filenames =
    mergeSymbols (SymbolToken ("int" ++ x) : xs) filenames
mergeSymbols (FloatTypeToken : SymbolToken x : xs) filenames =
    mergeSymbols (SymbolToken ("float" ++ x) : xs) filenames
mergeSymbols (CharTypeToken : SymbolToken x : xs) filenames =
    mergeSymbols (SymbolToken ("char" ++ x) : xs) filenames
mergeSymbols (StringTypeToken : SymbolToken x : xs) filenames =
    mergeSymbols (SymbolToken ("string" ++ x) : xs) filenames
mergeSymbols (VoidTypeToken : SymbolToken x : xs) filenames =
    mergeSymbols (SymbolToken ("void" ++ x) : xs) filenames
mergeSymbols (IfToken : SymbolToken x : xs) filenames =
    mergeSymbols (SymbolToken ("if" ++ x) : xs) filenames
mergeSymbols (ElseToken : SymbolToken x : xs) filenames =
    mergeSymbols (SymbolToken ("else" ++ x) : xs) filenames
mergeSymbols (FunToken : SymbolToken x : xs) filenames =
    mergeSymbols (SymbolToken ("fun" ++ x) : xs) filenames
mergeSymbols (ForToken : SymbolToken x : xs) filenames =
    mergeSymbols (SymbolToken ("for" ++ x) : xs) filenames
mergeSymbols (WhileToken : SymbolToken x : xs) filenames =
    mergeSymbols (SymbolToken ("while" ++ x) : xs) filenames
mergeSymbols (ReturnToken : SymbolToken x : xs) filenames =
    mergeSymbols (SymbolToken ("return" ++ x) : xs) filenames
mergeSymbols (IntToken x : SymbolToken y : xs) filenames =
    mergeSymbols (SymbolToken (show x ++ y) : xs) filenames

-- merge else if to elif
mergeSymbols (ElseToken : xs) filenames
    | (length (filter (/= SpaceToken) xs)) > 0 &&
        (head (filter (/= SpaceToken) xs)) == IfToken =
            mergeSymbols (ElseIfToken : (tail (dropWhile
                (/= IfToken) xs))) filenames
-- merge negative numbers (ex: - 123 -> -123)
mergeSymbols (MinusToken : IntToken x : xs) filenames =
    mergeSymbols (IntToken (-x) : xs) filenames
mergeSymbols (MinusToken : FloatToken x : xs) filenames =
    mergeSymbols (FloatToken (-x) : xs) filenames
-- Delete all spaces
mergeSymbols (SpaceToken : xs) filenames =
    mergeSymbols xs filenames
-- Concat all LineSeparator
mergeSymbols (LineSeparator : LineSeparator : xs) filenames =
    mergeSymbols (LineSeparator : xs) filenames
-- No merge needed
mergeSymbols (x:xs) filenames = do
    rest <- mergeSymbols xs filenames
    return (x : rest)

parseFile :: File -> Int -> [String] -> File -> IO ([Token])
parseFile (File []) _ _ _ = return ([])
parseFile _ _ _ (File []) = return ([])
parseFile (File (x:xs)) lineNumber filenames (File (ox:oxs)) = do
    parsedLine <- parseLine x lineNumber (head filenames) ox
    currentLine <- mergeSymbols parsedLine filenames
    rest <- parseFile (File xs) (lineNumber + 1) filenames (File oxs)
    return (currentLine ++ rest)

-- INFO: Convert token list to SExpr
getSubList :: Token -> Token -> [Token] -> ([Token], [Token])
getSubList _ _ [] = ([], [])
-- -- Comment case
-- getSubList CommentStart CommentEnd (x:xs) | x == CommentEnd = ([], xs)
-- getSubList CommentStart CommentEnd (_:xs) = getSubList CommentStart CommentEnd xs
-- All case
getSubList _ close (x:xs) | x == close = ([], xs)
getSubList open close (x:xs) | x == open =
    (open : subList ++ close : subList2, rest2)
    where
        (subList, rest) = getSubList open close xs
        (subList2, rest2) = getSubList open close rest
getSubList open close (x:xs) =
    (x : subList, rest)
    where
        (subList, rest) = getSubList open close xs

tokenListToSexpr :: [Token] -> [Token]
tokenListToSexpr [] = []
-- -- all between comment is ignored
-- tokenListToSexpr (CommentStart : xs) = do
--     let (_, rest) = getSubList CommentStart CommentEnd xs
--     tokenListToSexpr rest
-- Fix minus token being assigned to the number even if it should be a binary operator
tokenListToSexpr (SymbolToken x : IntToken y : xs) | y < 0 =
    (SymbolToken x : MinusToken : IntToken (-y) : tokenListToSexpr xs)
tokenListToSexpr (SymbolToken x : FloatToken y : xs) | y < 0 =
    (SymbolToken x : MinusToken : FloatToken (-y) : tokenListToSexpr xs)
-- Create a sub list for function type
tokenListToSexpr (FunTypeToken : xs) =
    ListToken [FunTypeToken, head (tokenListToSexpr xs)] : (tail
        (tokenListToSexpr xs))
-- all between parenthesis is a sub list of tokens
tokenListToSexpr (OpenParenthesis : xs) =
    ListToken (tokenListToSexpr subList) : tokenListToSexpr rest
    where
        (subList, rest) = getSubList OpenParenthesis CloseParenthesis xs
-- all between bracket is a sub list of tokens
tokenListToSexpr (OpenBracket : xs) =
    ListToken (tokenListToSexpr subList) : tokenListToSexpr rest
    where
        (subList, rest) = getSubList OpenBracket CloseBracket xs
-- all between braces is a sub list of tokens
tokenListToSexpr (OpenBraces : xs) =
    ListToken (tokenListToSexpr subList) : tokenListToSexpr rest
    where
        (subList, rest) = getSubList OpenBraces CloseBraces xs
-- all other tokens are converted to SExpr
tokenListToSexpr (x:xs) = x : tokenListToSexpr xs
