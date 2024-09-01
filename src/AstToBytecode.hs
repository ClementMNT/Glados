{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- GLaDOS
-}
module AstToBytecode (
    astToBytecode',
    valueSimpleToBytecode,
    astStoreValue,
    getListOfFunctions,
) where

import Types(Bytecode(..), AST(..), DataType(..))
import Debug.Trace
import Unsafe.Coerce (unsafeCoerce)

floatToInt :: Float -> Int
floatToInt = unsafeCoerce

valueSimpleToBytecode :: AST -> [Bytecode]
valueSimpleToBytecode (AST []) = []
valueSimpleToBytecode (AST (x:_)) =
    case x of
        IntAST y -> [LoadConst y IntType]
        SymbolAST y -> [LoadVarBefore y StringType]
        _ -> []
valueSimpleToBytecode _ = []


-- * ----------------------------------- STORE ---------------------------------------------------- * --

-- add all the functions to know if it's a function call or a variable
astStoreValue :: AST -> [String] -> [Bytecode]
astStoreValue (AST [IntTypeAST, SymbolAST x])    
    funct | existsInList x funct = error "ERROR astStoreValue IntTypeAST"
          | otherwise = [StoreVarBefore x IntType]
astStoreValue (AST [StringTypeAST, SymbolAST x]) 
    funct | existsInList x funct = error "ERROR astStoreValue StringTypeAST"
          | otherwise = [StoreVarBefore x StringType]
astStoreValue (AST [CharTypeAST, SymbolAST x])   
    funct | existsInList x funct = error "ERROR astStoreValue CharTypeAST"
          | otherwise = [StoreVarBefore x CharType]
astStoreValue (AST [FloatTypeAST, SymbolAST x])  
    funct | existsInList x funct = error "ERROR astStoreValue FloatTypeAST"
          | otherwise = [StoreVarBefore x FloatType]
astStoreValue (AST [SymbolAST x])                
    funct | existsInList x funct = [LoadPC, CallUserFun x]
          | otherwise = [StoreVarBefore x UnknownType]
astStoreValue (AST [SymbolAST x, AST [y]])       
    funct | existsInList x funct = [LoadPC, CallUserFun x] -- ! SaveAt, only for assignation
        | otherwise = let (_, yBytecode, _) = astToBytecode' (AST [y]) 0 funct
    in yBytecode ++ [LoadVarBefore x UnknownType]
    ++ [SaveAt] ++ [StoreVarBefore x UnknownType]
astStoreValue x _ = trace ("astStoreValue NO AST STORE FOUND" ++ show x) $ []

astStoreArgs :: AST -> [String] -> [Bytecode]
astStoreArgs DeadLeafAST _ = []
astStoreArgs (AST []) _ = []
astStoreArgs (AST (AST(x):xs)) funct =
    astStoreArgs (AST xs) funct ++ astStoreArgs (AST (x)) funct
astStoreArgs x funct = astStoreValue x funct


-- * ----------------------------------- GET ------------------------------------------------------ * --

getTypes :: AST -> DataType
getTypes (AST []) = error "ERROR getTypes empty"
getTypes (IntTypeAST) = IntType
getTypes (CharTypeAST) = CharType
getTypes (StringTypeAST) = StringType
getTypes (FloatTypeAST) = FloatType
getTypes (VoidTypeAST) = VoidType
getTypes (AST (x:_)) = getTypes x
getTypes x = error ("ERROR getTypes " ++ show x)


getNextJmp :: [Int] -> Int
getNextJmp [] = 0
getNextJmp (x:[]) = x
getNextJmp (x:y:xs) | x > y = getNextJmp (x:xs)
                    | otherwise = getNextJmp (y:xs)



-- * ----------------------------------- AST TO BYTECODE ------------------------------------------ * --

-- getType :: AST -> DataType
-- getType (AST []) = error "ERROR getType empty"
-- getType (IntTypeAST) = IntType
-- getType (CharTypeAST) = CharType
-- getType (StringTypeAST) = StringType
-- getType (FloatTypeAST) = FloatType
-- getType (VoidTypeAST) = VoidType
-- getType (AST (x:_)) = getType x
-- getType x = error ("ERROR getType " ++ show x)

--             AST      id_jmp  list_of_functions -> (AST, [Bytecode], id_jmp)
astToBytecode' :: AST -> Int -> [String] -> (AST, [Bytecode], Int)
astToBytecode' (AST []) jmp _ = (AST [], [], jmp)

-- * System calls
astToBytecode' (AST [SymbolAST "print", x]) jmp functs =
    let (xAST, xBytecode, jmp_1) = astToBytecode' (AST [x]) jmp functs
    in (xAST, xBytecode ++ [Call 1], jmp_1)
astToBytecode' (AST [SymbolAST "getline"]) jmp _ =
    (AST [], [Call 2], jmp)
astToBytecode' (AST [SymbolAST "readFile", x]) jmp functs =
    let (xAST, xBytecode, jmp_1) = astToBytecode' (AST [x]) jmp functs
    in (xAST, xBytecode ++ [Call 3], jmp_1)
astToBytecode' (AST [SymbolAST "writeInFile", x]) jmp functs =
    let (xAST, xBytecode, jmp_1) = astToBytecode' (AST [x]) jmp functs
    in (xAST, xBytecode ++ [Call 4], jmp_1)
astToBytecode' (AST [SymbolAST "appendInFile", x]) jmp functs =
    let (xAST, xBytecode, jmp_1) = astToBytecode' (AST [x]) jmp functs
    in (xAST, xBytecode ++ [Call 5], jmp_1)
astToBytecode' (AST [SymbolAST "exit", x]) jmp functs =
    let (xAST, xBytecode, jmp_1) = astToBytecode' (AST [x]) jmp functs
    in (xAST, xBytecode ++ [Call 60], jmp_1)


astToBytecode' (AST [IntTypeAST, SymbolAST x]) jmp _ =
    (AST [], [LoadVarBefore x IntType], jmp)
astToBytecode' (AST [CharTypeAST, SymbolAST x]) jmp _ =
    (AST [], [LoadVarBefore x CharType], jmp)
astToBytecode' (AST [StringTypeAST, SymbolAST x]) jmp _ =
    (AST [], [LoadVarBefore x StringType], jmp)
astToBytecode' (AST [FloatTypeAST, SymbolAST x]) jmp _ =
    (AST [], [LoadVarBefore x FloatType], jmp)

-- can be a function call or a str[y]
astToBytecode' (AST (SymbolAST x : y : xs)) jmp functs =
    let (_, aBytecode, _) = astToBytecode' y jmp functs
        (yAST, yBytecode, jmp_2) = astToBytecode' (AST xs) jmp functs
    in if existsInList x functs then
        (yAST, (aBytecode ++ [LoadPC, CallUserFun x] ++ yBytecode), jmp_2)
    else
        (yAST, ([LoadVarBefore x UnknownType]
        ++ aBytecode ++ [Index] ++ yBytecode), jmp_2)

-- * (AST (x:xs))
astToBytecode' (AST (x:xs)) jmp functs =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp functs
        (xsAST, xsBytecode, jmp_2) = astToBytecode' (AST xs) jmp_1 functs
    in (xsAST, xBytecode ++ xsBytecode, jmp_2)

-- * Condition operations

astToBytecode' (EqualAST x y) jmp functs =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp_2) = astToBytecode' y jmp_1 functs
    in (AST [], xBytecode ++ yBytecode ++ [CompareOp "="], jmp_2)

astToBytecode' (LessThanAST x y) jmp functs =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp_2) = astToBytecode' y jmp_1 functs
    in (AST [], xBytecode ++ yBytecode ++ [CompareOp "<"], jmp_2)

astToBytecode' (GreaterThanAST x y) jmp functs =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp_2) = astToBytecode' y jmp_1 functs
    in (AST [], xBytecode ++ yBytecode ++ [CompareOp ">"], jmp_2)

astToBytecode' (LessThanEqualAST x y) jmp functs =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp_2) = astToBytecode' y jmp_1 functs
    in (AST [], xBytecode ++ yBytecode ++ [CompareOp "a"], jmp_2)

astToBytecode' (GreaterThanEqualAST x y) jmp functs =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp_2) = astToBytecode' y jmp_1 functs
    in (AST [], xBytecode ++ yBytecode ++ [CompareOp "b"], jmp_2)

astToBytecode' (NotEqualAST x y) jmp functs =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp_2) = astToBytecode' y jmp_1 functs
    in (AST [], xBytecode ++ yBytecode ++ [CompareOp "!"], jmp_2)

astToBytecode' (AndAST x y) jmp functs =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp_2) = astToBytecode' y jmp_1 functs
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "&"], jmp_2)

astToBytecode' (OrAST x y) jmp functs =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp_2) = astToBytecode' y jmp_1 functs
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "|"], jmp_2)

astToBytecode' (BitAndAST x y) jmp functs =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp_2) = astToBytecode' y jmp_1 functs
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "&"], jmp_2)

astToBytecode' (BitOrAST x y) jmp functs =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp_2) = astToBytecode' y jmp_1 functs
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "|"], jmp_2)

astToBytecode' (BitXorAST x y) jmp functs =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp_2) = astToBytecode' y jmp_1 functs
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "^"], jmp_2)

-- FunTypeAST return_type
astToBytecode' (FunAST name args (FunTypeAST return_type) scope) jmp functs =
    let (_, scopeBytecode, jmp_2) = astToBytecode' scope jmp functs
    in (AST [],
        [FunEntryPoint name (getTypes return_type)] ++
        (astStoreArgs args functs) ++ scopeBytecode ++ [Return],
    jmp_2)

-- * IF / ELSE IF / functs ELSE
astToBytecode' (IfAST cond expr1 elseIfExpr1) jmp functs =
    let (_, condBytecode, jmp3) = astToBytecode' cond jmp functs
        (_, expr1Bytecode, jmp1) = astToBytecode' expr1 jmp functs
        (_, elseb, jmp2) = astToBytecode' elseIfExpr1 jmp1 functs
        n_mp = (getNextJmp [jmp, jmp1, jmp2, jmp3]) + 1
        cc = condBytecode ++ [JumpIfFalseBefore n_mp] ++ expr1Bytecode
    in if elseIfExpr1 == DeadLeafAST then
        (AST [], cc ++ [JumpRef n_mp] ++ elseb, n_mp)
    else let elseCode = [JumpBefore (n_mp + 1)] ++ [JumpRef n_mp] ++ elseb
        in (AST [], cc ++ elseCode ++ [JumpRef (n_mp + 1)], n_mp + 1)

astToBytecode' (ElseIfAST cond expr1 elseIfExpr1) jmp functs =
    let (_, condBytecode, jmp3) = astToBytecode' cond jmp functs
        (_, expr1Bytecode, jmp1) = astToBytecode' expr1 jmp functs
        (_, elseb, jmp2) = astToBytecode' elseIfExpr1 jmp1 functs
        n_jmp = (getNextJmp [jmp, jmp1, jmp2, jmp3]) + 1
        commonCode = condBytecode ++ [JumpIfFalseBefore n_jmp] ++ expr1Bytecode
    in if elseIfExpr1 == DeadLeafAST then
        (AST [], commonCode ++ [JumpRef n_jmp] ++ elseb, n_jmp)
    else let elseCode = [JumpBefore (n_jmp + 1)] ++ [JumpRef n_jmp] ++ elseb
        in (AST [], commonCode ++ elseCode ++ [JumpRef (n_jmp + 1)], n_jmp + 1)

astToBytecode' (ElseAST expr1) jmp functs =
    let (_, expr1Bytecode, jmp_1) = astToBytecode' expr1 jmp functs
    in trace ("ElseAST: jmp = " ++ show jmp ++ " jmp_1 = " ++ show jmp_1) $
        (AST [], expr1Bytecode, jmp_1)

-- * WHILE
astToBytecode' (WhileAST cond expr1) jmp functs =
    let (_, condBytecode, jmp2) = astToBytecode' cond jmp functs
        (_, expr1Bytecode, jmp1) = astToBytecode' expr1 jmp functs
        new_jmp = (getNextJmp [jmp, jmp1, jmp2]) + 1
    in ( AST []
       , [JumpRef (new_jmp + 1)] ++ condBytecode
            ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode
            ++ [JumpBefore (new_jmp + 1)] ++ [JumpRef new_jmp]
       , new_jmp + 1)

-- * FOR
astToBytecode' (ForAST initi cond increment scope) jmp functs =
    let (_, initiBytecode, jmp1) = astToBytecode' initi jmp functs
        (_, condBytecode, jmp4) = astToBytecode' cond jmp functs
        (_, incrementBytecode, jmp2) = astToBytecode' increment jmp1 functs
        (_, scopeBytecode, jmp3) = astToBytecode' scope jmp2 functs
        new_jmp = (getNextJmp [jmp, jmp1, jmp2, jmp3, jmp4]) + 1
    in ( AST []
       , initiBytecode ++ [JumpRef (new_jmp + 1)] ++ condBytecode
        ++ [JumpIfFalseBefore new_jmp] ++ scopeBytecode ++ incrementBytecode
        ++ [JumpBefore (new_jmp + 1)] ++ [JumpRef new_jmp], new_jmp + 1)

-- * RETURN
astToBytecode' (ReturnAST expr1) jmp functs =
    let (_, expr1Bytecode, jmp_1) = astToBytecode' expr1 jmp functs
    in (AST [], expr1Bytecode ++ [Return], jmp_1)

-- * Assignation operatiofuncts n
astToBytecode' (AssignAST x y) jmp functs =
    let (yAST, yBytecode, jmp_1) = astToBytecode' y jmp functs
    in (yAST, yBytecode ++ (astStoreValue x functs), jmp_1)

-- * Simple operations
astToBytecode' (PlusAST x y) jmp functs =
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp functs
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1 functs
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "+"]], jmp2)

astToBytecode' (MinusAST x y) jmp functs =
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp functs
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1 functs
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "-"]], jmp2)

astToBytecode' (TimesAST x y) jmp functs =
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp functs
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1 functs
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "*"]], jmp2)

astToBytecode' (DivideAST x y) jmp functs =
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp functs
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1 functs
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "/"]], jmp2)

astToBytecode' (ModuloAST x y) jmp functs =
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp functs
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1 functs
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "%"]], jmp2)

astToBytecode' (NotAST x) jmp functs =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp functs
    in (AST [], xBytecode ++ [UnaryOp "!"], jmp1)

-- * Incrementation and decrementation
astToBytecode' (IncrementAST x) jmp functs =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp functs
        incrementCode = [LoadConst 1 IntType, BinaryOp "+"]
        storeCode = astStoreValue x functs
    in (AST [], xBytecode ++ incrementCode ++ storeCode, jmp1)

astToBytecode' (DecrementAST x) jmp functs =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp functs
        decrementCode = [LoadConst 1 IntType, BinaryOp "-"]
        storeCode = astStoreValue x functs
    in (AST [], xBytecode ++ decrementCode ++ storeCode, jmp1)

-- * Assignation avec opération (ex: +=, -=, /=, %=)
astToBytecode' (PlusEqualAST x y) jmp functs =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp2) = astToBytecode' y jmp1 functs
        storeCode = astStoreValue x functs
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "+"] ++ storeCode, jmp2)

astToBytecode' (MinusEqualAST x y) jmp functs =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp2) = astToBytecode' y jmp1 functs
        storeCode = astStoreValue x functs
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "-"] ++ storeCode, jmp2)

astToBytecode' (TimesEqualAST x y) jmp functs =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp2) = astToBytecode' y jmp1 functs
        storeCode = astStoreValue x functs
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "*"] ++ storeCode, jmp2)

astToBytecode' (DivideEqualAST x y) jmp functs =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp functs
        (_, yBytecode, jmp2) = astToBytecode' y jmp1 functs
        storeCode = astStoreValue x functs
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "/"] ++ storeCode, jmp2)

-- * Load operations
 -- we can't know the type of the variable
astToBytecode' (SymbolAST x) jmp funs = (AST [], varOrFunctCall x funs, jmp)
astToBytecode' (IntAST x) jmp _ = (AST [], [LoadConst x IntType], jmp)
astToBytecode' (FloatAST x) jmp _ =
    (AST [], [LoadConst (floatToInt x) FloatType], jmp)
astToBytecode' (CharAST x) jmp _ =
    (AST [], [LoadConst (fromEnum x) CharType], jmp)
astToBytecode' (StringAST x) jmp _ =
    (AST [], [LoadConst 0 StringType, StringToSave x], jmp)
astToBytecode' DeadLeafAST jmp _ = (AST [], [], jmp)
astToBytecode' a jmp _ = (a, [], jmp)


-- * ----------------------------------- LIST OF FUNCTIONS --------------------------------------- * --

existsInList :: String -> [String] -> Bool
existsInList _ [] = False
existsInList x (y:ys) | x == y = True
                      | otherwise = existsInList x ys

-- VoidType

varOrFunctCall :: String -> [String] -> [Bytecode]
varOrFunctCall x functs
    | existsInList x functs = [LoadPC, CallUserFun x]
    | otherwise = [LoadVarBefore x UnknownType]

-- get the list of functions withouth args
getListOfFunctions :: AST -> [String]
getListOfFunctions (AST []) = []
getListOfFunctions (FunAST name _ _ _) = [name]
getListOfFunctions (AST (x:xs)) = getListOfFunctions x ++
    getListOfFunctions (AST xs)
getListOfFunctions _ = []
