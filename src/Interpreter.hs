module Interpreter where

import AbsGrammar
import ErrM
import Utils

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M

--------------------------------------------------------------------------------
-- DATA TYPES
--------------------------------------------------------------------------------

data IPVal = IPInt Integer
    | IPBool Bool
    | IPString String
    | IPTuple [IPVal]
    | IPList [IPVal]

data IPArg = IPArg Ident
    | IPArgVal Ident Expr 
    | IPArgRef Ident
    | IPArgRefVal Ident Expr

data IPFun = IPFunc [IPArg] Body 
    | IPProc [IPArg] Body

instance Show IPVal where
    show (IPInt i) = show i
    show (IPBool b) = show b
    show (IPString s) = show s
    show (IPTuple vs)  = "<" ++ tail(concatMap (\el -> "," ++ show el) vs) ++ ">"
    show (IPList l) = show l

instance Eq IPVal where
    IPInt i1 == IPInt i2 = i1 == i2
    IPBool b1 == IPBool b2 = b1 == b2
    IPString s1 == IPString s2 = s1 == s2
    IPTuple l1 == IPTuple l2 = l1 == l2
    IPList l1 == IPList l2 = l1 == l2
    _ == _ = False

type Loc = Int

type Output = String
type ReturnVal = Maybe IPVal
type FlagContinue = Bool
type FlagBreak = Bool
type Info = (Output, ReturnVal, FlagContinue, FlagBreak)

type VStore = M.Map Loc IPVal
type FStore = M.Map Loc (IPFun, Env) -- function have env from moment of decl
type Store = (VStore, FStore, Info)

type Venv = M.Map Ident Loc
type Fenv = M.Map Ident Loc
type Env = (Venv, Fenv)

type IP = ExceptT String (StateT Store (Reader Env))

errMsg :: Int -> String
errMsg i = "unexpected situation: " ++ show i
-- for Debug look "errMsg x"

--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------

getVStore :: IP VStore
getVStore = do
    (vStore, _, _) <- get
    return vStore

setVStore :: VStore -> IP ()
setVStore vStore = do
    (_, fStore, info) <- get
    put (vStore, fStore, info)
    return ()

getFStore :: IP FStore
getFStore = do
    (_, fStore, _) <- get
    return fStore

setFStore :: FStore -> IP ()
setFStore fStore = do
    (vStore, _, info) <- get
    put (vStore, fStore, info)
    return ()

getOutput :: IP String
getOutput = do
    (_, _, (output, _, _, _)) <- get
    return output

getOrOnFlags :: IP Bool
getOrOnFlags = do
    flagCont <- getFlagCont
    flagBreak <- getFlagBreak
    flagRetVal <- getFlagRetVal
    return (flagCont || flagBreak || flagRetVal)

getFlagRetVal :: IP Bool
getFlagRetVal = do
    (_, _, (_, retVal, _, _)) <- get
    case retVal of
        Just _ -> return True
        Nothing -> return False

getRetVal :: IP ReturnVal
getRetVal = do
    (_, _, (_, retVal, _, _)) <- get
    return retVal

setRetVal :: ReturnVal -> IP ()
setRetVal retVal = do
    (vStore, fStore, (output, _, fCont, fBreak)) <- get
    put (vStore, fStore, (output, retVal, fCont, fBreak))
    return ()

getFlagCont :: IP Bool
getFlagCont = do
    (_, _, (_, _, flagCont, _)) <- get
    return flagCont

setFlagCont :: Bool -> IP ()
setFlagCont fCont = do
    (vStore, fStore, (output, retVal, _, fBreak)) <- get
    put (vStore, fStore, (output, retVal, fCont, fBreak))
    return ()

getFlagBreak :: IP Bool
getFlagBreak = do
    (_, _, (_, _, _, flagBreak)) <- get
    return flagBreak

setFlagBreak :: Bool -> IP ()
setFlagBreak fBreak = do
    (vStore, fStore, (output, retVal, fCont, _)) <- get
    put (vStore, fStore, (output, retVal, fCont, fBreak))
    return ()

setOutput :: String -> IP ()
setOutput output = do
    (vStore, fStore, (_, retVal, fCont, fBreak)) <- get
    put (vStore, fStore, (output, retVal, fCont, fBreak))
    return ()

addVar :: Ident -> IPVal -> IP Env
addVar ident ipVal = do
    (vEnv, fEnv) <- ask
    vStore <- getVStore
    let loc = (if M.null vStore then 0 else fst (M.findMax vStore) + 1)
    setVStore (M.insert loc ipVal vStore)
    return (M.insert ident loc vEnv, fEnv)

getVarLoc :: Ident -> IP Loc
getVarLoc ident = do
    (vEnv, _) <- ask
    case M.lookup ident vEnv of
        Just loc -> return loc
        Nothing -> throwError (errMsg 1)

setVarLoc :: Ident -> Loc -> IP Env
setVarLoc ident loc = do
    (vEnv, fEnv) <- ask
    return (M.insert ident loc vEnv, fEnv)

getVarVal :: Ident -> IP IPVal
getVarVal ident = do
    loc <- getVarLoc ident
    vStore <- getVStore
    case M.lookup loc vStore of
        Just ipVal -> return ipVal
        Nothing -> throwError (errMsg 2)

saveVarVal :: Ident -> IPVal -> IP Env
saveVarVal ident ipVal = do 
    vStore <- getVStore
    loc <- getVarLoc ident
    setVStore (M.insert loc ipVal vStore)
    ask

getFunLoc :: Ident -> IP Loc
getFunLoc ident = do
    (_, fEnv) <- ask
    case M.lookup ident fEnv of
        Just loc -> return loc
        Nothing -> throwError (errMsg 3)

getFunAndEnv :: Ident -> IP (IPFun, Env)
getFunAndEnv ident = do
    loc <- getFunLoc ident
    fStore <- getFStore
    case M.lookup loc fStore of
        Just (fun, env) -> return (fun, env)
        Nothing -> throwError (errMsg 4)

addFun :: Ident -> IPFun -> IP Env
addFun ident ipFun = do
    (vEnv, fEnv) <- ask
    fStore <- getFStore
    let loc = (if M.null fStore then 0 else fst (M.findMax fStore) + 1)
    let env = (vEnv, M.insert ident loc fEnv)
    setFStore (M.insert loc (ipFun, env) fStore)
    return env

convArgType :: InitArg -> IPArg
convArgType initArg = case initArg of
    (ArgWithValue _ _ ident expr) -> IPArgVal ident expr
    (ArgRefWithValue _ _ ident expr) -> IPArgRefVal ident expr
    (ArgWithoutValue _ _ ident) -> IPArg ident
    (ArgRefWithoutValue _ _ ident) -> IPArgRef ident

initValue :: Type -> IPVal
initValue t = case t of
    Int _ -> IPInt 0
    Bool _ -> IPBool False
    String _ -> IPString ""
    Tuple _ ts -> IPTuple (map initValue ts)
    List _ _ -> IPList []

setNthElement :: Int -> String -> Char -> String
setNthElement ind [] c = []
setNthElement 0 suf c = c : setNthElement (-1) (tail suf) c
setNthElement ind suf c = head suf : setNthElement (ind - 1) (tail suf) c

innerFor :: Expr -> Instr -> Body -> IP ()
innerFor expr instr body = do
    ipExpr <- transExpr expr
    case ipExpr of
        (IPBool True) -> do
            transBody body
            flagRetVal <- getFlagRetVal
            if flagRetVal
                then return ()
                else (do
                    setFlagCont False
                    flagBreak <- getFlagBreak
                    setFlagBreak False
                    if flagBreak
                        then return ()
                        else (do
                            transInstr instr
                            innerFor expr instr body
                        )
                )
        (IPBool False) -> return ()
        _ -> throwError (errMsg 5)

innerAssign :: Var -> IPVal -> IP Env
innerAssign var ipVal = case var of

    Var p ident -> do
        saveVarVal ident ipVal

    VarStringEl p ident expr -> case ipVal of
        IPString [c] -> do
            ipString <- getVarVal ident
            ipInd <- transExpr expr -- ind will be after all right side
            case (ipString, ipInd) of
                (IPString str, IPInt ind) -> do
                    let len = length str
                    if 0 <= ind && ind < fromIntegral len
                        then saveVarVal ident (IPString (setNthElement (fromIntegral ind) str c))
                        else throwError (genPosInfo p ++ "index out of range")
                _ -> throwError (errMsg 6)
        IPString str -> throwError (genPosInfo p ++ "expected to assign single symbol")
        _ -> throwError (errMsg 7)

    VarTie _ tieEls -> case ipVal of
        IPTuple l -> do 
            mapM_ (\(TieEl _ var, ipValEl) -> innerAssign var ipValEl) (zip tieEls l)
            ask
        _ -> throwError (errMsg 8)

--------------------------------------------------------------------------------
-- INTERPRETER
--------------------------------------------------------------------------------

run :: Program -> Err String
run program = case runReader (runStateT (runExceptT (transProgram program)) (M.empty, M.empty, ("", Nothing, False, False))) (M.empty, M.empty) of
    (Left err, _) -> Bad err
    (Right res, _) -> Ok res

transProgram :: Program -> IP String
transProgram (Program p decls) = case decls of
    [] -> getOutput
    (decl:ds) -> do
        env <- transDecl decl
        local (const env) (transProgram (Program p ds))

transDecl :: Decl -> IP Env
transDecl decl = case decl of

    DeclFunc p _ ident@(Ident "main") initArgs body -> do
        env <- addFun ident (IPFunc [] body)
        res <- local (const env) (transExpr (EexecFunc p ident []))
        case res of
            (IPInt 0) -> ask
            _ -> throwError (genPosInfo p ++ "main function returned " 
                            ++ show res ++ ", expected 0")
    
    DeclFunc _ _ ident initArgs body -> do
        let ipArgs = map convArgType initArgs
        addFun ident (IPFunc ipArgs body)
          
    DeclProc _ ident initArgs body -> do
        let ipArgs = map convArgType initArgs
        addFun ident (IPProc ipArgs body)
        
    DeclVars _ t initVars -> do
        env <- ask
        foldM 
            (\tmpEnv initVar -> case initVar of
                VarWithValue _ ident expr -> do
                    ipExpr <- local (const tmpEnv) (transExpr expr)
                    local (const tmpEnv) (addVar ident ipExpr)
                VarWithoutValue _ ident -> local (const tmpEnv) (addVar ident (initValue t))) 
            env 
            initVars

transBody :: Body -> IP ()
transBody (Body p instrs) = case instrs of
    [] -> return ()
    (instr:is) -> do
        env <- transInstr instr
        local (const env) (transBody (Body p is))

transInstr :: Instr -> IP Env
transInstr instr = do
    skip <- getOrOnFlags
    if skip
        then ask
        else case instr of

        LocalDeclFunc p t ident initArgs body -> transDecl (DeclFunc p t ident initArgs body)

        LocalDeclProc p ident initArgs body -> transDecl (DeclProc p ident initArgs body)

        LocalDeclVars p t initVars -> transDecl (DeclVars p t initVars)

        FuncReturn _ expr -> do
            ipVal <- transExpr expr
            setRetVal (Just ipVal)
            ask

        ProcReturn _ -> do
            setRetVal (Just (IPString "void"))
            ask

        Skip _ -> ask

        ExecProc _ ident exprs -> do
            (fun, env) <- getFunAndEnv ident
            case fun of
                (IPProc ipArgs body) -> do

                    let ipArgsAndExprs = zip ipArgs exprs
                    let sufIpArgs = drop (length ipArgsAndExprs) ipArgs

                    bodyEnv <- foldM (\tmpEnv (ipArg, expr) -> case ipArg of

                        IPArg ident -> do
                            ipVal <- transExpr expr
                            local (const tmpEnv) (addVar ident ipVal)

                        IPArgVal ident _ -> do
                            ipVal <- transExpr expr
                            local (const tmpEnv) (addVar ident ipVal)

                        IPArgRef ident -> case expr of
                            Evariable _ identVar -> do
                                loc <- getVarLoc identVar
                                local (const tmpEnv) (setVarLoc ident loc)
                            _ -> throwError (errMsg 9)

                        IPArgRefVal ident _ -> case expr of
                            Evariable _ identVar -> do
                                loc <- getVarLoc identVar
                                local (const tmpEnv) (setVarLoc ident loc)
                            _ -> throwError (errMsg 10)

                        ) env ipArgsAndExprs

                    bodyEnv <- foldM (\tmpEnv ipArg -> case ipArg of 
                        
                        IPArgVal ident expr -> do
                            ipVal <- local (const env) (transExpr expr)
                            local (const tmpEnv) (addVar ident ipVal)

                        IPArgRefVal ident expr -> case expr of
                            Evariable _ identVar -> do
                                loc <- local (const env) (getVarLoc identVar)
                                local (const tmpEnv) (setVarLoc ident loc)
                            _ -> throwError (errMsg 62)

                        _ -> throwError (errMsg 11)
                        
                        ) bodyEnv sufIpArgs
                    
                    local (const bodyEnv) (transBody body)

                    setFlagCont False
                    setFlagBreak False
                    setRetVal Nothing

                    ask
            
                _ -> throwError (errMsg 12)

        While p expr body -> do
            ipBool <- transExpr expr
            case ipBool of
                IPBool b -> if b
                    then (do
                        transBody body
                        flagRetVal <- getFlagRetVal
                        if flagRetVal
                            then ask
                            else (do
                                setFlagCont False
                                flagBreak <- getFlagBreak
                                setFlagBreak False
                                if flagBreak
                                    then ask
                                    else transInstr (While p expr body)
                            )
                    )
                    else ask
                _ -> throwError (errMsg 13)

        If _ expr body -> do
            b <- transExpr expr
            case b of
                (IPBool True) -> do
                    transBody body
                    ask
                _ -> ask

        IfElse _ expr body1 body2 -> do
            b <- transExpr expr
            case b of
                (IPBool True) -> do
                    transBody body1
                    ask
                _ -> do
                    transBody body2
                    ask

        For p t initVars expr var modif body -> do
            env <- transDecl (DeclVars p t initVars)
            local (const env) (innerFor expr (VarModif p var modif) body)
            ask

        Break _ -> do
            setFlagBreak True
            ask

        Countinue _ -> do
            setFlagCont True
            ask

        PushBack _ ident expr -> do
            ipVal <- transExpr expr
            ipList <- getVarVal ident
            case ipList of
                IPList l -> do
                    saveVarVal ident (IPList (l ++ [ipVal]))
                _ -> throwError (errMsg 14)

        PopBack p ident -> do
            ipList <- getVarVal ident
            case ipList of
                IPList [] -> throwError (genPosInfo p ++ "pop back on empty list")
                IPList l -> saveVarVal ident (IPList (init l))
                _ -> throwError (errMsg 15)

        PushFront _ ident expr -> do
            ipVal <- transExpr expr
            ipList <- getVarVal ident
            case ipList of
                IPList l -> do
                    saveVarVal ident (IPList (ipVal : l))
                _ -> throwError (errMsg 16)

        PopFront p ident -> do
            ipList <- getVarVal ident
            case ipList of
                IPList [] -> throwError (genPosInfo p ++ "pop front on empty list")
                IPList l -> saveVarVal ident (IPList (tail l))
                _ -> throwError (errMsg 17)

        Cout _ coutArgs -> do
            let exprs = map (\coutArg@(CoutArgs _ expr) -> expr) coutArgs
            out <- foldM (\outTmp expr -> do
                ipVal <- transExpr expr
                case ipVal of

                    IPString s -> return (outTmp ++  s) where

                    _ -> return (outTmp ++ show ipVal) ) "" exprs

            output <- getOutput
            setOutput (output ++ out)
            ask

        VarModif _ var modif -> case modif of

            Iinc _ -> case var of
                Var p ident -> do
                    ipVal <- getVarVal ident
                    case ipVal of
                        IPInt ipInt -> saveVarVal ident (IPInt (ipInt + 1))
                        _ -> throwError (errMsg 18)
                _ -> throwError (errMsg 19)

            IaddValue _ expr -> do
                ipExpr <- transExpr expr
                case (var, ipExpr) of
                    (Var p ident, IPInt ipInt) -> do
                        ipVal <- getVarVal ident
                        case ipVal of
                            IPInt ipInt2 -> saveVarVal ident (IPInt (ipInt + ipInt2))
                            _ -> throwError (errMsg 20)
                    (Var p ident, IPString ipString) -> do
                        ipVal <- getVarVal ident
                        case ipVal of
                            IPString ipString2 -> saveVarVal ident (IPString (ipString2 ++ ipString))
                            _ -> throwError (errMsg 21)
                    _ -> throwError (errMsg 22)

            Idec _ -> case var of
                Var p ident -> do
                    ipVal <- getVarVal ident
                    case ipVal of
                        IPInt ipInt -> saveVarVal ident (IPInt (ipInt - 1))
                        _ -> throwError (errMsg 23)
                _ -> throwError (errMsg 24)

            IdecValue _ expr -> do
                ipExpr <- transExpr expr
                case (var, ipExpr) of
                    (Var p ident, IPInt ipDec) -> do
                        ipVal <- getVarVal ident
                        case ipVal of
                            IPInt ipInt -> saveVarVal ident (IPInt (ipInt - ipDec))
                            _ -> throwError (errMsg 25)
                    _ -> throwError (errMsg 26)

            IandEq _ expr -> case var of
                (Var p ident) -> do
                    ipExpr <- getVarVal ident
                    case ipExpr of
                        IPBool False -> ask
                        IPBool True -> do
                            ipBool <- transExpr expr
                            case ipBool of
                                IPBool b -> saveVarVal ident (IPBool b)
                                _ -> throwError (errMsg 27)
                        _ -> throwError (errMsg 28)
                _ -> throwError (errMsg 29)

            IorEq _ expr -> case var of
                (Var p ident) -> do
                    ipExpr <- getVarVal ident
                    case ipExpr of
                        IPBool True -> ask
                        IPBool False -> do
                            ipBool <- transExpr expr
                            case ipBool of
                                IPBool b -> saveVarVal ident (IPBool b)
                                _ -> throwError (errMsg 30)
                        _ -> throwError (errMsg 31)
                _ -> throwError (errMsg 32)

            Imod _ expr -> do
                ipExpr <- transExpr expr
                case (var, ipExpr) of
                    (Var p ident, IPInt 0) -> throwError (genPosInfo p ++ "modulo by 0")
                    (Var p ident, IPInt ipMod) -> do
                        ipVal <- getVarVal ident
                        case ipVal of
                            IPInt ipInt -> saveVarVal ident (IPInt (ipInt `mod` ipMod))
                            _ -> throwError (errMsg 33)
                    _ -> throwError (errMsg 34)

            Itimes _ expr -> do
                ipExpr <- transExpr expr
                case (var, ipExpr) of
                    (Var p ident, IPInt ipTimes) -> do
                        ipVal <- getVarVal ident
                        case ipVal of
                            IPInt ipInt -> saveVarVal ident (IPInt (ipInt * ipTimes))
                            _ -> throwError (errMsg 35)
                    _ -> throwError (errMsg 36)

            Idiv _ expr -> do
                ipExpr <- transExpr expr
                case (var, ipExpr) of
                    (Var p ident, IPInt 0) -> throwError (genPosInfo p ++ "divide by 0")
                    (Var p ident, IPInt ipDiv) -> do
                        ipVal <- getVarVal ident
                        case ipVal of
                            IPInt ipInt -> saveVarVal ident (IPInt (ipInt `div` ipDiv))
                            _ -> throwError (errMsg 37)
                    _ -> throwError (errMsg 38)

            Iassign _ expr -> do
                ipExpr <- transExpr expr
                innerAssign var ipExpr
                ask

transExpr :: Expr -> IP IPVal
transExpr expr = case expr of

    Eor _ expr1 expr2 -> do
        ipVal1 <- transExpr expr1
        case ipVal1 of
            (IPBool True) -> return (IPBool True)
            (IPBool False) -> do
                ipVal2 <- transExpr expr2
                case ipVal2 of
                    (IPBool b2) -> return (IPBool b2)
                    _ -> throwError (errMsg 39)
            _ -> throwError (errMsg 40)

    Eand _ expr1 expr2 -> do
        ipVal1 <- transExpr expr1
        case ipVal1 of
            (IPBool False) -> return (IPBool False)
            (IPBool True) -> do
                ipVal2 <- transExpr expr2
                case ipVal2 of
                    (IPBool b2) -> return (IPBool b2)
                    _ -> throwError (errMsg 41)
            _ -> throwError (errMsg 42)

    Ecmp _ expr1 cmpOp expr2 -> do
        ipVal1 <- transExpr expr1
        ipVal2 <- transExpr expr2
        case cmpOp of

            CmpLe _ -> case (ipVal1, ipVal2) of
                (IPInt i1, IPInt i2) -> return (IPBool (i1 < i2))
                _ -> throwError (errMsg 43)

            CmpLeq _ -> case (ipVal1, ipVal2) of
                (IPInt i1, IPInt i2) -> return (IPBool (i1 <= i2))
                _ -> throwError (errMsg 44)

            CmpEq _ -> return (IPBool (ipVal1 == ipVal2))

            CmpGeq _ -> case (ipVal1, ipVal2) of
                (IPInt i1, IPInt i2) -> return (IPBool (i1 >= i2))
                _ -> throwError (errMsg 45)

            CmpGe _ -> case (ipVal1, ipVal2) of
                (IPInt i1, IPInt i2) -> return (IPBool (i1 > i2))
                _ -> throwError (errMsg 46)

            CmpNeq _ -> return (IPBool (ipVal1 /= ipVal2))

    Ecomplex _ complexVal -> case complexVal of

        ConstTuple _ exprs -> do
            ipVals <- mapM transExpr exprs
            return (IPTuple ipVals)

        ConstList _ t exprs -> do
            ipVals <- mapM transExpr exprs
            return (IPList ipVals)

    Eadd _ expr1 addOp expr2 -> do
        ipVal1 <- transExpr expr1
        ipVal2 <- transExpr expr2
        case addOp of

            Plus _ -> case (ipVal1, ipVal2) of
                (IPInt i1, IPInt i2) -> return (IPInt (i1 + i2))
                (IPString s1, IPString s2) -> return (IPString (s1 ++ s2))
                _ -> throwError (errMsg 47)

            Minus _ -> case (ipVal1, ipVal2) of
                (IPInt i1, IPInt i2) -> return (IPInt (i1 - i2))
                _ -> throwError (errMsg 48)

    Emul _ expr1 mulOp expr2 -> do
        ipVal1 <- transExpr expr1
        ipVal2 <- transExpr expr2
        case (ipVal1, ipVal2) of
            (IPInt i1, IPInt i2) -> case mulOp of

                Times _ -> return (IPInt (i1 * i2))

                Div p -> if i2 == 0
                    then throwError (genPosInfo (hasPosition expr2) ++ "divide by 0")
                    else return (IPInt (i1 `div` i2))

                Mod p -> if i2 == 0
                    then throwError (genPosInfo (hasPosition expr2) ++ "modulo by 0")
                    else return (IPInt (i1 `mod` i2))

            _ -> throwError (errMsg 49)

    Eneg _ expr -> do
        ipVal <- transExpr expr
        case ipVal of
            IPInt i -> return (IPInt (-i))
            _ -> throwError (errMsg 50)

    Enot _ expr -> do
        ipVal <- transExpr expr
        case ipVal of
            IPBool b -> return (IPBool (not b))
            _ -> throwError (errMsg 51)

    Econst _ simpleVal -> case simpleVal of

        ConstInt _ integer -> return (IPInt integer)

        ConstBoolTrue _ -> return (IPBool True)

        ConstBoolFalse _ -> return (IPBool False)

        ConstString _ string -> return (IPString string)

    EeasyMethod _ expr easyMethod -> do
        ipVal <- transExpr expr
        case easyMethod of

            Size _ -> case ipVal of
                IPString str -> return (IPInt (fromIntegral (length str)))
                _ -> throwError (errMsg 52)

            Empty _ -> case ipVal of
                IPString str -> return (IPBool (null str))
                IPList l -> return (IPBool (null l))
                _ -> throwError (errMsg 53)

    EhardMethod _ expr hardMethod -> do
        ipVal <- transExpr expr
        case hardMethod of

            Get p exprInd -> do
                ipInd <- transExpr exprInd
                case (ipVal, ipInd) of
                    (IPTuple t, IPInt ind) -> do
                        let len = length t
                        if 0 <= ind && ind < fromIntegral len
                            then return (t !! fromIntegral ind)
                            else throwError (genPosInfo p ++ "index out of range")
                    _ -> throwError (errMsg 54)

            Back p -> case ipVal of
                (IPList []) -> throwError (genPosInfo p ++ "back on empty list")
                (IPList l) -> return (last l)
                _ -> throwError (errMsg 55)

            Front p -> case ipVal of
                (IPList []) -> throwError (genPosInfo p ++ "front on empty list")
                (IPList l) -> return (head l)
                _ -> throwError (errMsg 56)

    EstringEl p expr1 expr2 -> do
        ipVal1 <- transExpr expr1
        ipVal2 <- transExpr expr2
        case (ipVal1, ipVal2) of
            (IPString str, IPInt ind) -> do
                let len = length str
                if 0 <= ind && ind < fromIntegral len
                    then return (IPString [str !! fromIntegral ind])
                    else throwError (genPosInfo p ++ "index out of range")
            _ -> throwError (errMsg 57)

    Evariable p ident -> getVarVal ident

    EexecFunc p ident@(Ident stringName) exprs -> do
        (fun, env) <- getFunAndEnv ident
        case fun of
            (IPFunc ipArgs body) -> do

                let ipArgsAndExprs = zip ipArgs exprs
                let sufIpArgs = drop (length ipArgsAndExprs) ipArgs

                bodyEnv <- foldM (\tmpEnv (ipArg, expr) -> case ipArg of

                    IPArg ident -> do
                        ipVal <- transExpr expr
                        local (const tmpEnv) (addVar ident ipVal)

                    IPArgVal ident _ -> do
                        ipVal <- transExpr expr
                        local (const tmpEnv) (addVar ident ipVal)

                    IPArgRef ident -> case expr of
                        Evariable _ identVar -> do
                            loc <- getVarLoc identVar
                            local (const tmpEnv) (setVarLoc ident loc)
                        _ -> throwError (errMsg 58)

                    IPArgRefVal ident _ -> case expr of
                        Evariable _ identVar -> do
                            loc <- getVarLoc identVar
                            local (const tmpEnv) (setVarLoc ident loc)
                        _ -> throwError (errMsg 59)

                    ) env ipArgsAndExprs

                bodyEnv <- foldM (\tmpEnv ipArg -> case ipArg of 
                        
                    IPArgVal ident expr -> do
                        ipVal <- local (const env) (transExpr expr)
                        local (const tmpEnv) (addVar ident ipVal)

                    IPArgRefVal ident expr -> case expr of
                        Evariable _ identVar -> do
                            loc <- local (const env) (getVarLoc identVar)
                            local (const tmpEnv) (setVarLoc ident loc)
                        _ -> throwError (errMsg 63)

                    _ -> throwError (errMsg 60)
                        
                    ) bodyEnv sufIpArgs
                    
                local (const bodyEnv) (transBody body)

                setFlagCont False
                setFlagBreak False
                retVal <- getRetVal
                setRetVal Nothing

                case retVal of
                    Just val -> return val
                    Nothing -> throwError (genPosInfo p ++ "no returned value")
            
            _ -> throwError (errMsg 61)