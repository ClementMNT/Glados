{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- GLaDOS
-}

module ParserAST (
    splitAtValue,
    splitAtLastValue,

    getIfChain,

    binaryOperatorsAST,
    operatorsAfterAST,
    operatorsBeforeAST,
    listOperatorsASTCheck,

    pemdasTree,

    sexprToAst,
) where

import Types
import Control.Exception
-- import Debug.Trace

-- INFO: Convert SExpr to AST
splitAtValue :: Eq a => a -> [a] -> Maybe ([a], a, [a])
splitAtValue _ [] = Nothing
splitAtValue val (x:xs)
    | val == x = Just ([], x, xs)
    | otherwise = case splitAtValue val xs of
                    Nothing -> Nothing
                    Just (before, v, after) -> Just (x:before, v, after)

pureSplitAtValue :: Eq a => a -> [a] -> ([a], a, [a])
pureSplitAtValue x [] = ([], x, [])
pureSplitAtValue x ys = case splitAtValue x ys of
    Nothing -> ([], x, [])
    Just (before, v, after) -> (before, v, after)

pureSplitAtLastValue :: Eq a => a -> [a] -> ([a], a, [a])
pureSplitAtLastValue x [] = ([], x, [])
pureSplitAtLastValue x ys = case splitAtLastValue x ys of
    Nothing -> ([], x, [])
    Just (before, v, after) -> (before, v, after)

splitAtLastValue :: Eq a => a -> [a] -> Maybe ([a], a, [a])
splitAtLastValue _ [] = Nothing
-- splitAtValue until we ge nothing
splitAtLastValue val xs = case splitAtValue val xs of
    Nothing -> Nothing
    Just (before, v, after) -> case splitAtLastValue val after of
        Nothing -> Just (before, v, after)
        Just (before2, v2, after2) -> Just (before ++ v:before2, v2, after2)

getIfChain :: [Token] -> ([Token], [Token])
getIfChain [] = ([], [])
getIfChain (ElseIfToken : cond : expr : xs) =
    (ElseIfToken : cond : expr : (fst $ getIfChain xs), snd $ getIfChain xs)
getIfChain (ElseToken : expr : xs) =
    (ElseToken : expr : (fst $ getIfChain xs), snd $ getIfChain xs)
getIfChain xs = ([], xs)

binaryOperatorsAST :: Token -> (AST -> AST -> AST) -> [Token] -> AST
binaryOperatorsAST token ast xs = ast (sexprToAst' before) (sexprToAst' after)
    where
        (before, _, after) = pureSplitAtValue token xs

operatorsAfterAST :: Token -> (AST -> AST) -> [Token] -> AST
operatorsAfterAST token ast xs = ast (sexprToAst' after)
    where
        (_, _, after) = pureSplitAtValue token xs

operatorsBeforeAST :: Token -> (AST -> AST) -> [Token] -> AST
operatorsBeforeAST token ast xs = ast (sexprToAst' before)
    where
        (before, _, _) = pureSplitAtValue token xs

listOperatorsASTCheck :: [Token] -> [Token] -> Bool
listOperatorsASTCheck [] _ = True
listOperatorsASTCheck (token : tokens) xs = case splitAtValue token xs of
            Nothing -> False
            Just (_, _, _) -> listOperatorsASTCheck tokens xs

pemdasTreeAction :: Token -> (AST -> AST -> AST) -> [Token] -> AST
pemdasTreeAction token ast xs = ast (pemdasTree before) (pemdasTree after)
    where
        (before, _, after) = pureSplitAtLastValue token xs

pemdasTreeAction2 :: Token -> Token -> (AST -> AST -> AST) ->
    (AST -> AST -> AST) -> [Token] -> AST
pemdasTreeAction2 token1 token2 ast1 ast2 xs | length before > length before2 =
        ast1 (pemdasTree before) (pemdasTree after)
    | otherwise = ast2 (pemdasTree before2) (pemdasTree after2)
    where
        (before, _, after) = pureSplitAtLastValue token1 xs
        (before2, _, after2) = pureSplitAtLastValue token2 xs

sizeCompare :: [Token] -> [Token] -> [Token] -> Bool
sizeCompare before1 before2 before3 = (length before1 > length before2) &&
    (length before1 > length before3)

pemdasTree :: [Token] -> AST
pemdasTree [] = DeadLeafAST
-- ! Plus and Minus token
pemdasTree x | listOperatorsASTCheck [PlusToken, MinusToken] x =
    pemdasTreeAction2 PlusToken MinusToken PlusAST MinusAST x
-- ! Plus token
pemdasTree x | listOperatorsASTCheck [PlusToken] x =
    pemdasTreeAction PlusToken PlusAST x
-- ! Minus token
pemdasTree x | listOperatorsASTCheck [MinusToken] x =
    pemdasTreeAction MinusToken MinusAST x

-- ! Modulo, Divide and Times token
pemdasTree x | listOperatorsASTCheck [ModuloToken, DivideToken, TimesToken] x =
    if sizeCompare beforeDivide beforeModulo beforeTimes then
        pemdasTreeAction DivideToken DivideAST x
    else pemdasTreeAction2 TimesToken ModuloToken TimesAST ModuloAST x
    where
        (beforeModulo, _, _) = pureSplitAtLastValue ModuloToken x
        (beforeDivide, _, _) = pureSplitAtLastValue DivideToken x
        (beforeTimes, _, _) = pureSplitAtLastValue TimesToken x

-- ! Modulo and Divide token
pemdasTree x | listOperatorsASTCheck [ModuloToken, DivideToken] x =
    pemdasTreeAction2 ModuloToken DivideToken ModuloAST DivideAST x
-- ! Modulo and Times token
pemdasTree x | listOperatorsASTCheck [ModuloToken, TimesToken] x =
    pemdasTreeAction2 ModuloToken TimesToken ModuloAST TimesAST x
-- ! Divide and Times token
pemdasTree x | listOperatorsASTCheck [DivideToken, TimesToken] x =
    pemdasTreeAction2 DivideToken TimesToken DivideAST TimesAST x
-- ! Modulo token
pemdasTree x | listOperatorsASTCheck [ModuloToken] x =
    pemdasTreeAction ModuloToken ModuloAST x
-- ! Divide token
pemdasTree x | listOperatorsASTCheck [DivideToken] x =
    pemdasTreeAction DivideToken DivideAST x
-- ! Times token
pemdasTree x | listOperatorsASTCheck [TimesToken] x =
    pemdasTreeAction TimesToken TimesAST x

pemdasTree x = sexprToAst' x

getAsList :: Token -> [Token]
getAsList (ListToken x) = x
getAsList x = [x]

sexprToAst' :: [Token] -> AST
sexprToAst' [] = DeadLeafAST
-- ! Comma token
sexprToAst' x | case splitAtValue CommaToken x of
    Nothing -> False
    Just (_, _, _) -> True = case length $ filter (/= CommaToken) after of
        0 -> AST [sexprToAst' before]
        _ -> AST [sexprToAst' before] <> sexprToAst' (after ++ [CommaToken])
    where
        (before, _, after) = pureSplitAtValue CommaToken x
-- ! While token
sexprToAst' (WhileToken : cond : expr : xs) =
    AST [WhileAST (sexprToAst' cond2) (sexprToAst' expr2)] <> sexprToAst' xs
    where
        cond2 = getAsList cond
        expr2 = getAsList expr
-- ! For token
sexprToAst' (ForToken : ici : expr : xs) =
    AST [ForAST (sexprToAst' initer) (sexprToAst' cond)
        (sexprToAst' incr) (sexprToAst' expr2)] <> sexprToAst' xs
    where
        ici2 = getAsList ici
        expr2 = getAsList expr
        (initer, _, rest) = pureSplitAtValue LineSeparator ici2
        (cond, _, rest2) = pureSplitAtValue LineSeparator rest
        (incr, _, _) = pureSplitAtValue LineSeparator
            (rest2 ++ [LineSeparator])
-- ! Fun type token
sexprToAst' (FunTypeToken : xs) =
    FunTypeAST (sexprToAst' xs)
-- ! Fun token
sexprToAst' (FunToken : name : args : returnType : body : xs) =
    AST [FunAST (show name) (sexprToAst' args2)
        (sexprToAst' returnType2) (sexprToAst' body2)] <> sexprToAst' xs
    where
        args2 = getAsList args
        returnType2 = getAsList returnType
        body2 = getAsList body
-- ! If token
sexprToAst' (IfToken : cond : expr : ElseIfToken : xs) =
    AST [IfAST (sexprToAst' cond2) (sexprToAst' expr2)
        (sexprToAst' ifChain)] <> sexprToAst' rest
    where
        cond2 = getAsList cond
        expr2 = getAsList expr
        (ifChain, rest) = getIfChain (ElseIfToken : xs)
sexprToAst' (IfToken : cond : expr : ElseToken : xs) =
    AST [IfAST (sexprToAst' cond2) (sexprToAst' expr2)
        (sexprToAst' ifChain)] <> sexprToAst' rest
    where
        cond2 = getAsList cond
        expr2 = getAsList expr
        (ifChain, rest) = getIfChain (ElseToken : xs)
sexprToAst' (IfToken : cond : expr : xs) =
    AST [IfAST (sexprToAst' cond2) (sexprToAst' expr2)
        DeadLeafAST] <> sexprToAst' xs
    where
        cond2 = getAsList cond
        expr2 = getAsList expr
-- ! Else if token
sexprToAst' (ElseIfToken : cond : expr : ElseIfToken : xs) =
    AST [ElseIfAST (sexprToAst' cond2) (sexprToAst' expr2)
        (sexprToAst' ifChain)] <> sexprToAst' rest
    where
        cond2 = getAsList cond
        expr2 = getAsList expr
        (ifChain, rest) = getIfChain (ElseIfToken : xs)
sexprToAst' (ElseIfToken : cond : expr : ElseToken : xs) =
    AST [ElseIfAST (sexprToAst' cond2) (sexprToAst' expr2)
        (sexprToAst' ifChain)] <> sexprToAst' rest
    where
        cond2 = getAsList cond
        expr2 = getAsList expr
        (ifChain, rest) = getIfChain (ElseToken : xs)
sexprToAst' (ElseIfToken : cond : expr : xs) =
    AST [ElseIfAST (sexprToAst' cond2) (sexprToAst' expr2)
        DeadLeafAST] <> sexprToAst' xs
    where
        cond2 = getAsList cond
        expr2 = getAsList expr
-- ! Else token
sexprToAst' (ElseToken : expr : xs) =
    AST [ElseAST (sexprToAst' expr2)] <> sexprToAst' xs
    where
        expr2 = getAsList expr
-- ! Define token
-- sexprToAst' (DefineToken : name : expr : xs) = do
--     -- if type of name isn't SymbolToken then throw error
--     if case name of
--         SymbolToken _ -> False
--         _ -> True then do
--         throw $ (ParserError "Error: Invalid type for name in define")
--     else do
--         let expr2 = case expr of
--                 ListToken x -> x
--                 _ -> [expr]
--         AST [DefineAST (show name) (sexprToAst' expr2)] <> sexprToAst' xs

sexprToAst' (DefineToken : _ : _ : xs) =
    sexprToAst' xs
-- ! Line separator token
sexprToAst' x | case splitAtValue LineSeparator x of
            Nothing -> False
            Just (_, _, _) -> True =
                AST [sexprToAst' before] <> sexprToAst' after
    where
        (before, _, after) = pureSplitAtValue LineSeparator x
-- ! Return token
sexprToAst' (ReturnToken : xs) = ReturnAST (sexprToAst' xs)
-- ! Symbol Tree
sexprToAst' x | listOperatorsASTCheck [AssignToken] x =
    binaryOperatorsAST AssignToken AssignAST x
sexprToAst' x | listOperatorsASTCheck [PlusEqualToken] x =
    binaryOperatorsAST PlusEqualToken PlusEqualAST x
sexprToAst' x | listOperatorsASTCheck [MinusEqualToken] x =
    binaryOperatorsAST MinusEqualToken MinusEqualAST x
sexprToAst' x | listOperatorsASTCheck [TimesEqualToken] x =
    binaryOperatorsAST TimesEqualToken TimesEqualAST x
sexprToAst' x | listOperatorsASTCheck [DivideEqualToken] x =
    binaryOperatorsAST DivideEqualToken DivideEqualAST x
sexprToAst' x | listOperatorsASTCheck [ModuloEqualToken] x =
    binaryOperatorsAST ModuloEqualToken ModuloEqualAST x

sexprToAst' x | listOperatorsASTCheck [AndToken] x =
    binaryOperatorsAST AndToken AndAST x
sexprToAst' x | listOperatorsASTCheck [OrToken] x =
    binaryOperatorsAST OrToken OrAST x

sexprToAst' x | listOperatorsASTCheck [EqualToken] x =
    binaryOperatorsAST EqualToken EqualAST x
sexprToAst' x | listOperatorsASTCheck [NotEqualToken] x =
    binaryOperatorsAST NotEqualToken NotEqualAST x
sexprToAst' x | listOperatorsASTCheck [LessThanToken] x =
    binaryOperatorsAST LessThanToken LessThanAST x
sexprToAst' x | listOperatorsASTCheck [LessThanEqualToken] x =
    binaryOperatorsAST LessThanEqualToken LessThanEqualAST x
sexprToAst' x | listOperatorsASTCheck [GreaterThanToken] x =
    binaryOperatorsAST GreaterThanToken GreaterThanAST x
sexprToAst' x | listOperatorsASTCheck [GreaterThanEqualToken] x =
    binaryOperatorsAST GreaterThanEqualToken GreaterThanEqualAST x

sexprToAst' x | listOperatorsASTCheck [MinusToken] x =
    pemdasTree x
sexprToAst' x | listOperatorsASTCheck [PlusToken] x =
    pemdasTree x
sexprToAst' x | listOperatorsASTCheck [ModuloToken] x =
    pemdasTree x
sexprToAst' x | listOperatorsASTCheck [DivideToken] x =
    pemdasTree x
sexprToAst' x | listOperatorsASTCheck [TimesToken] x =
    pemdasTree x

sexprToAst' x | listOperatorsASTCheck [IncrementToken] x =
    operatorsBeforeAST IncrementToken IncrementAST x
sexprToAst' x | listOperatorsASTCheck [DecrementToken] x =
    operatorsBeforeAST DecrementToken DecrementAST x

sexprToAst' x | listOperatorsASTCheck [BitAndToken] x =
    binaryOperatorsAST BitAndToken BitAndAST x
sexprToAst' x | listOperatorsASTCheck [BitOrToken] x =
    binaryOperatorsAST BitOrToken BitOrAST x
sexprToAst' x | listOperatorsASTCheck [BitXorToken] x =
    binaryOperatorsAST BitXorToken BitXorAST x

sexprToAst' x | listOperatorsASTCheck [NotToken] x =
    operatorsAfterAST NotToken NotAST x
-- ! Types token
sexprToAst' (IntTypeToken : xs) =
    AST [IntTypeAST] <> sexprToAst' xs
sexprToAst' (FloatTypeToken : xs) =
    AST [FloatTypeAST] <> sexprToAst' xs
sexprToAst' (CharTypeToken : xs) =
    AST [CharTypeAST] <> sexprToAst' xs
sexprToAst' (StringTypeToken : xs) =
    AST [StringTypeAST] <> sexprToAst' xs
sexprToAst' (VoidTypeToken : xs) =
    AST [VoidTypeAST] <> sexprToAst' xs
-- ! Int token
sexprToAst' (IntToken x : xs) =
    AST [IntAST x] <> sexprToAst' xs
-- ! Float token
sexprToAst' (FloatToken x : xs) =
    AST [FloatAST x] <> sexprToAst' xs
-- ! Symbol token
sexprToAst' (SymbolToken x : xs) =
    AST [SymbolAST x] <> sexprToAst' xs
-- ! String token
sexprToAst' (StringToken x : xs) =
    AST [StringAST x] <> sexprToAst' xs
-- ! Char token
sexprToAst' (CharToken x : xs) =
    AST [CharAST x] <> sexprToAst' xs
-- ! List token
sexprToAst' (ListToken (x : xs) : ys) =
    AST [sexprToAst' (x : xs)] <> sexprToAst' ys
-- ! Other token
sexprToAst' (_ : xs) =
    sexprToAst' xs

-- INFO: Apply define
-- a function that will go through the AST and on each symbolAST node will check if it's a define
-- if it is a define then it will replace the symbolAST node with the AST contained in the define
applyDefine :: AST -> [(String, AST)] -> AST
applyDefine (AST []) _ = DeadLeafAST
applyDefine (AST ((SymbolAST name) : xs)) defineList =
    AST [getASTFromDefineList name defineList] <> ast2
    where
        ast2 = applyDefine (AST xs) defineList

applyDefine (AST ((AST ys) : xs)) defineList =
    AST [applyDefine (AST ys) defineList] <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((FunTypeAST subAst) : xs)) defineList =
    FunTypeAST (applyDefine subAst defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((FunAST name args returnType body) : xs)) defineList =
    FunAST name (applyDefine args defineList)
        (applyDefine returnType defineList)
        (applyDefine body defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((IfAST cond expr ifChain) : xs)) defineList =
    IfAST (applyDefine cond defineList)
        (applyDefine expr defineList)
        (applyDefine ifChain defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((ElseIfAST cond expr ifChain) : xs)) defineList =
    ElseIfAST (applyDefine cond defineList)
        (applyDefine expr defineList)
        (applyDefine ifChain defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((ElseAST expr) : xs)) defineList =
    ElseAST (applyDefine expr defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((WhileAST cond expr) : xs)) defineList =
    WhileAST (applyDefine cond defineList)
        (applyDefine expr defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((ForAST initer cond incr expr) : xs)) defineList =
    ForAST (applyDefine initer defineList)
        (applyDefine cond defineList)
        (applyDefine incr defineList)
        (applyDefine expr defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((ReturnAST expr) : xs)) defineList =
    ReturnAST (applyDefine expr defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList

applyDefine (AST ((AssignAST to from) : xs)) defineList =
    AssignAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((EqualAST to from) : xs)) defineList =
    EqualAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((NotEqualAST to from) : xs)) defineList =
    NotEqualAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((LessThanAST to from) : xs)) defineList =
    LessThanAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((LessThanEqualAST to from) : xs)) defineList =
    LessThanEqualAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((GreaterThanAST to from) : xs)) defineList =
    GreaterThanAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((GreaterThanEqualAST to from) : xs)) defineList =
    GreaterThanEqualAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((AndAST to from) : xs)) defineList =
    AndAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((OrAST to from) : xs)) defineList =
    OrAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((BitAndAST to from) : xs)) defineList =
    BitAndAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((BitOrAST to from) : xs)) defineList =
    BitOrAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((BitXorAST to from) : xs)) defineList =
    BitXorAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((PlusEqualAST to from) : xs)) defineList =
    PlusEqualAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((MinusEqualAST to from) : xs)) defineList =
    MinusEqualAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((TimesEqualAST to from) : xs)) defineList =
    TimesEqualAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((DivideEqualAST to from) : xs)) defineList =
    DivideEqualAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((ModuloEqualAST to from) : xs)) defineList =
    ModuloEqualAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((PlusAST to from) : xs)) defineList =
    PlusAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((MinusAST to from) : xs)) defineList =
    MinusAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((TimesAST to from) : xs)) defineList =
    TimesAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((DivideAST to from) : xs)) defineList =
    DivideAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((ModuloAST to from) : xs)) defineList =
    ModuloAST (applyDefine to defineList)
        (applyDefine from defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((IncrementAST to) : xs)) defineList =
    IncrementAST (applyDefine to defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((DecrementAST to) : xs)) defineList =
    DecrementAST (applyDefine to defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((NotAST to) : xs)) defineList =
    NotAST (applyDefine to defineList)
        <> ast2
    where
        ast2 = applyDefine (AST xs) defineList

applyDefine (AST ((IntAST y) : xs)) defineList =
    AST [IntAST y] <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((FloatAST y) : xs)) defineList =
    AST [FloatAST y] <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((CharAST y) : xs)) defineList =
    AST [CharAST y] <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((StringAST y) : xs)) defineList =
    AST [StringAST y] <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((IntTypeAST) : xs)) defineList =
    AST [IntTypeAST] <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((FloatTypeAST) : xs)) defineList =
    AST [FloatTypeAST] <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((CharTypeAST) : xs)) defineList =
    AST [CharTypeAST] <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((StringTypeAST) : xs)) defineList =
    AST [StringTypeAST] <> ast2
    where
        ast2 = applyDefine (AST xs) defineList
applyDefine (AST ((VoidTypeAST) : xs)) defineList =
    AST [VoidTypeAST] <> ast2
    where
        ast2 = applyDefine (AST xs) defineList

applyDefine (AST (x : xs)) defineList =
    x <> ast2
    where
        ast2 = applyDefine (AST xs) defineList

applyDefine x defineList = applyDefine (AST [x]) defineList

getASTFromDefineList :: String -> [(String, AST)] -> AST
getASTFromDefineList name [] = SymbolAST name
getASTFromDefineList name ((name2, ast) : xs)
    | name == name2 = ast
    | otherwise = getASTFromDefineList name xs

getDefineList :: [Token] -> [(String, AST)]
getDefineList [] = []
getDefineList (DefineToken : name : expr : xs)
    | case name of
        SymbolToken _ -> False
        _ -> True =
            throw $ (ParserError "Error: Invalid type for name in define")
    | otherwise = (show name, sexprToAst' expr2) : getDefineList xs
    where
        expr2 = getAsList expr
getDefineList (_ : xs) = getDefineList xs

sexprToAst :: [Token] -> AST
sexprToAst x = applyDefine (sexprToAst' x) (getDefineList x)
-- sexprToAst x = do
    -- let defineList = getDefineList x
    -- let ast = sexprToAst' x
    -- -- trace ("TRACE: " ++ (printAST $ ast) ++ "\n------------------------------------") $ applyDefine ast defineList
    -- let ast2 = applyDefine ast defineList
    -- ast2
