module TypeChecker where

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

data TCType = TCInt
    | TCBool
    | TCString
    | TCTuple [TCType]
    | TCList TCType

data TCArg = TCArg TCType
    | TCArgVal TCType
    | TCArgRef TCType
    | TCArgRefVal TCType

data TCFun = TCFunc TCType [TCArg]
    | TCProc [TCArg]

instance Show TCType where
    show TCInt = "int"
    show TCBool = "bool"
    show TCString = "string"
    show (TCTuple types) =
        "tuple<" ++ tail(concatMap (\t -> "," ++ show t) types) ++ ">"
    show (TCList typeList) = "list<" ++ show typeList ++ ">"

instance Eq TCType where
    TCInt == TCInt = True
    TCBool == TCBool = True
    TCString == TCString = True
    TCList typeA == TCList typeB = typeA == typeB
    TCTuple typesA == TCTuple typesB = typesA == typesB
    _ == _ = False

type Loc = Int
type Store = M.Map Loc TCType
type Venv = M.Map Ident Loc
type Fenv = M.Map Ident TCFun
type Env = (Venv, Fenv, Ident) -- last fun indent
type TC = ExceptT String (StateT Store (Reader Env))

--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------

forceVarType :: Var -> TCType -> TC TCType
forceVarType var tForced = do
    t <- transVar var
    if t == tForced
        then return t
        else throwError (genPosInfo (hasPosition var)
                        ++ "invalid type " ++ genTypeInfo t
                        ++ ", expected " ++ genTypeInfo tForced)

forceVarOneOfTypes :: Var -> [TCType] -> TC TCType
forceVarOneOfTypes var tsForced = do
    t <- transVar var
    if t `elem` tsForced
        then return t
        else throwError (genPosInfo (hasPosition var)
                        ++ "invalid type " ++ genTypeInfo t
                        ++ ", expected one of " ++ genTypesInfo tsForced)

forceExprType :: Expr -> TCType -> TC TCType
forceExprType expr forcedType = do
    t <- transExpr expr
    if forcedType == t
        then return t
        else throwError (genPosInfo (hasPosition expr)
                        ++ "invalid type " ++ genTypeInfo t
                        ++ ", expected " ++ genTypeInfo forcedType)

forceExprOneOfTypes :: Expr -> [TCType] -> TC TCType
forceExprOneOfTypes expr forcedTypes = do
    t <- transExpr expr
    if t `elem` forcedTypes
        then return t
        else throwError (genPosInfo (hasPosition expr)
                        ++ "invalid type " ++ genTypeInfo t
                        ++ ", expected one of " ++ genTypesInfo forcedTypes)

forceDefTCArgsSuf :: [TCArg] -> BNFC'Position -> TC ()
forceDefTCArgsSuf tcArgs p = do
    foldM_
        (\isDef arg -> do
            let withoutValue = tcArgWithoutValue arg
                tcArgWithoutValue :: TCArg -> Bool
                tcArgWithoutValue (TCArg _) = True
                tcArgWithoutValue (TCArgRef _) = True
                tcArgWithoutValue _ = False
            if withoutValue && isDef
                then throwError (genPosInfo p
                                ++ "expected sufix of default values")
                else return (isDef || not withoutValue))
        False
        tcArgs

forceAppliedArgsTypes :: [Expr] -> [TCArg] -> BNFC'Position -> TC ()
forceAppliedArgsTypes exprs tcArgs p = case (exprs, tcArgs) of

    ([], []) -> return ()

    ([], _) -> do
        if all (\tcArg -> case tcArg of
            TCArgVal _ -> True
            TCArgRefVal _ -> True
            _ -> False) tcArgs
            then return ()
            else throwError (genPosInfo p ++ "too small number of arguments")

    (_, []) -> throwError (genPosInfo p ++ "too big number of arguments")

    (expr:exprs, tcArg:tcArgs) -> do
        let forcedType = getTCArgType tcArg
        forceExprType expr forcedType
        case expr of
            (Evariable pe _) -> forceAppliedArgsTypes exprs tcArgs p
            _ -> if getRefArgInfo tcArg
                then throwError (genPosInfo (hasPosition expr) ++ "one of aguments should be variable")
                else forceAppliedArgsTypes exprs tcArgs p

lookUpReturnInBody :: Body -> Bool
lookUpReturnInBody (Body _ []) = False
lookUpReturnInBody (Body p (instr:instrs)) =
    case instr of
        FuncReturn _ _ -> True
        ProcReturn _ -> True
        While _ _ body -> lookUpReturnInBody body
        If _ _ body -> lookUpReturnInBody body
        IfElse _ _ body1 body2 -> lookUpReturnInBody body1 || lookUpReturnInBody body2
        For _ _ _ _ _ _ body -> lookUpReturnInBody body
        _ -> False
    || lookUpReturnInBody (Body p instrs)

convArgType :: InitArg -> TCArg
convArgType initArg = case initArg of
    (ArgWithValue _ t _ _) -> TCArgVal (convType t)
    (ArgRefWithValue _ t _ _) -> TCArgRefVal (convType t)
    (ArgWithoutValue _ t _) -> TCArg (convType t)
    (ArgRefWithoutValue _ t _) -> TCArgRef (convType t)

getArgIdent :: InitArg -> Ident
getArgIdent initArg = case initArg of
    (ArgWithValue _ _ ident _) -> ident
    (ArgRefWithValue _ _ ident _) -> ident
    (ArgWithoutValue _ _ ident) -> ident
    (ArgRefWithoutValue _ _ ident) -> ident

forceAllArgsIdentsUnique :: [InitArg] -> TC ()
forceAllArgsIdentsUnique initArgs = case initArgs of
    [] -> return ()
    (arg:args) -> if all(\x -> getArgIdent arg /= getArgIdent x) args
        then forceAllArgsIdentsUnique args
        else throwError (genPosInfo (hasPosition arg) ++ "the name of agument repeats")

forceAllArgsValExpectedType :: [InitArg] -> TC ()
forceAllArgsValExpectedType initArgs = case initArgs of
    [] -> return ()
    (arg:args) -> case arg of
        (ArgWithValue _ t _ expr) -> do
            forceExprType expr (getTCArgType (convArgType arg))
            forceAllArgsValExpectedType args
        (ArgRefWithValue _ _ _ expr) -> case expr of
            (Evariable _ ident) -> do
                forceExprType expr (getTCArgType (convArgType arg))
                forceAllArgsValExpectedType args
            _ -> throwError (genPosInfo (hasPosition expr) ++ "default value for reference can be only variable")
        _ -> forceAllArgsValExpectedType args

getTCArgType :: TCArg -> TCType
getTCArgType tcArg = case tcArg of
    (TCArg t) -> t
    (TCArgRef t) -> t
    (TCArgVal t) -> t
    (TCArgRefVal t) -> t

getRefArgInfo :: TCArg -> Bool
getRefArgInfo tcArg = case tcArg of
    (TCArgRef _) -> True
    (TCArgRefVal _) -> True
    _ -> False

convType :: Type -> TCType
convType t = case t of
    Int _ -> TCInt
    Bool _ -> TCBool
    String _ -> TCString
    Tuple _ types -> TCTuple (map convType types)
    List _ t -> TCList (convType t)

addVariable :: Ident -> TCType -> TC Env
addVariable ident tcType = do
    store <- get
    (vEnv, fEnv, fIdent) <- ask
    let loc = (if M.null store then 0 else fst (M.findMax store) + 1)
    modify (M.insert loc tcType)
    return (M.insert ident loc vEnv, fEnv, fIdent)

forceUniqueIntMain :: Decl -> TC ()
forceUniqueIntMain decl = case decl of

    DeclFunc p t ident@(Ident "main") initArgs _ ->
        case convType t of
            TCInt ->
                case initArgs of
                    [] -> do
                        (_, fEnv, _) <- ask
                        case M.lookup ident fEnv of
                            Just _ -> throwError (genPosInfo p ++ "main duplicate")
                            Nothing -> return ()
                    _ -> throwError (genPosInfo p ++ "expected no arguments in main")
            _ -> throwError (genPosInfo p ++ "expected 'int' main")

    DeclProc p ident@(Ident "main") _ _ ->
        throwError (genPosInfo p ++ "main can't be global procedure")

    _ -> return ()

--------------------------------------------------------------------------------
-- TYPE CHECKER
--------------------------------------------------------------------------------

run :: Program -> Err String
run program = case runReader (runStateT (runExceptT (transProgram program)) M.empty) (M.empty, M.empty, Ident "void") of
    (Left err, _) -> Bad err
    (Right _, _) -> Ok "Type checker: passed."

transProgram :: Program -> TC ()
transProgram (Program p decls) = case decls of
    [] -> throwError "No main function"
    [lastDecl] -> do
        (_, fEnv, _) <- transDecl lastDecl
        case M.lookup (Ident "main") fEnv of
            Just _ -> return ()
            _ -> throwError "No main function"
    (decl:ds) -> do
        env <- transDecl decl
        local (const env) (transProgram (Program p ds))

transDecl :: Decl -> TC Env
transDecl decl = case decl of

    DeclFunc p t ident initArgs body -> do
        (vEnv, fEnv, fIdent) <- ask
        forceUniqueIntMain decl
        forceAllArgsIdentsUnique initArgs
        forceAllArgsValExpectedType initArgs

        let tcArgs = map convArgType initArgs
        forceDefTCArgsSuf tcArgs p

        let env = (vEnv, M.insert ident (TCFunc (convType t) tcArgs) fEnv, fIdent)
        let bodyEnv = (vEnv, M.insert ident (TCFunc (convType t) tcArgs) fEnv, ident)
        bodyEnv <- foldM (\tmpEnv initArg ->
            local (const tmpEnv) (addVariable (getArgIdent initArg) (getTCArgType (convArgType initArg)))) bodyEnv initArgs
        local (const bodyEnv) (transBody body)

        if lookUpReturnInBody body
            then return env
            else throwError (genPosInfo p ++ "no return statement in function")

    DeclProc p ident initArgs body -> do
        (vEnv, fEnv, fIdent) <- ask
        forceUniqueIntMain decl
        forceAllArgsIdentsUnique initArgs
        forceAllArgsValExpectedType initArgs

        let tcArgs = map convArgType initArgs
        forceDefTCArgsSuf tcArgs p

        let env = (vEnv, M.insert ident (TCProc tcArgs) fEnv, fIdent)
        let bodyEnv = (vEnv, M.insert ident (TCProc tcArgs) fEnv, ident)
        bodyEnv <- foldM (\tmpEnv initArg ->
            local (const tmpEnv) (addVariable (getArgIdent initArg) (getTCArgType (convArgType initArg)))) bodyEnv initArgs
        local (const bodyEnv) (transBody body)

        return env

    DeclVars p t initVars -> case initVars of
        [] -> ask
        (var:vars) -> do
            vIdent <- case var of
                VarWithValue _ ident expr -> do
                    forceExprType expr (convType t)
                    return ident
                VarWithoutValue _ ident -> return ident
            env <- addVariable vIdent (convType t)
            local (const env) (transDecl (DeclVars p t vars))

transBody :: Body -> TC ()
transBody (Body p instrs) = case instrs of
    [] -> return ()
    (instr:is) -> do
        env <- transInstr instr
        local (const env) (transBody (Body p is))

transInstr :: Instr -> TC Env
transInstr instr = case instr of

    LocalDeclFunc p t ident@(Ident stringName) initArgs body ->
        if stringName == "main"
            then throwError (genPosInfo p ++ "main can't be local function")
            else transDecl (DeclFunc p t ident initArgs body)

    LocalDeclProc p ident@(Ident stringName) initArgs body ->
        if stringName == "main"
            then throwError (genPosInfo p ++ "main can't be local procedure")
            else transDecl (DeclProc p ident initArgs body)

    LocalDeclVars p t initVars -> transDecl (DeclVars p t initVars)

    FuncReturn p expr -> do
        env@(_, fEnv, ident) <- ask
        case M.lookup ident fEnv of
            (Just (TCFunc t _)) -> do
                forceExprType expr t
                return env
            _ -> throwError (genPosInfo p ++ "return statement don't suit to body")

    ProcReturn p -> do
        env@(_, fEnv, ident) <- ask
        case M.lookup ident fEnv of
            (Just (TCProc _)) -> return env
            _ -> throwError (genPosInfo p ++ "return statement don't suit to body")

    Skip _ -> ask

    ExecProc p ident@(Ident stringName) exprs -> case stringName of
        "main" ->throwError (genPosInfo p ++ "can't call main function also as procedure")
        _ -> do
            (_, fEnv, _) <- ask
            tcArgs <- case M.lookup ident fEnv of
                Just (TCProc tcArgs) -> return tcArgs
                _ -> throwError (genPosInfo p ++ "undeclared procedure " ++ stringName)
            forceAppliedArgsTypes exprs tcArgs p
            ask

    While _ expr body -> do
        forceExprType expr TCBool
        transBody body
        ask

    If _ expr body -> do
        forceExprType expr TCBool
        transBody body
        ask

    IfElse _ expr body1 body2 -> do
        forceExprType expr TCBool
        transBody body1
        transBody body2
        ask

    For p t initVars expr var modif body -> do
        env <- transDecl (DeclVars p t initVars)
        local (const env) (forceExprType expr TCBool)
        local (const env) (transInstr (VarModif p var modif))
        local (const env) (transBody body)
        return env

    Break _ -> ask

    Countinue _ -> ask

    PushBack p ident expr -> do
        tl <- transExpr (Evariable p ident)
        case tl of
            TCList t -> do
                forceExprType expr t
                ask
            _ -> throwError (genPosInfo p
                            ++ "invalid type " ++ genTypeInfo tl
                            ++ ", expected 'List'")

    PopBack p ident -> do
        tl <- transExpr (Evariable p ident)
        case tl of
            TCList _ -> ask
            _ -> throwError (genPosInfo p
                            ++ "invalid type " ++ genTypeInfo tl
                            ++ ", expected 'List'")

    PushFront p ident expr -> do
        tl <- transExpr (Evariable p ident)
        case tl of
            TCList t -> do
                forceExprType expr t
                ask
            _ -> throwError (genPosInfo p
                            ++ "invalid type " ++ genTypeInfo tl
                            ++ ", expected 'List'")

    PopFront p ident -> do
        tl <- transExpr (Evariable p ident)
        case tl of
            TCList _ -> ask
            _ -> throwError (genPosInfo p
                            ++ "invalid type " ++ genTypeInfo tl
                            ++ ", expected 'List'")

    Cout _ coutArgs -> ask

    VarModif _ var modif -> case modif of

        Iinc _ -> do
            forceVarType var TCInt
            ask

        IaddValue _ expr -> do
            t <- forceVarOneOfTypes var [TCInt, TCString]
            forceExprType expr t
            ask

        Idec _ -> do
            forceVarType var TCInt
            ask

        IdecValue _ expr -> do
            forceVarType var TCInt
            forceExprType expr TCInt
            ask

        IandEq _ expr -> do
            forceVarType var TCBool
            forceExprType expr TCBool
            ask

        IorEq _ expr -> do
            forceVarType var TCBool
            forceExprType expr TCBool
            ask

        Imod _ expr -> do
            forceVarType var TCInt
            forceExprType expr TCInt
            ask

        Itimes _ expr -> do
            forceVarType var TCInt
            forceExprType expr TCInt
            ask

        Idiv _ expr -> do
            forceVarType var TCInt
            forceExprType expr TCInt
            ask

        Iassign _ expr -> do
            t <- transVar var
            forceExprType expr t
            ask

transVar :: Var -> TC TCType
transVar var = case var of

    Var p ident -> transExpr (Evariable p ident)

    VarStringEl p ident expr -> do
        forceIdentType ident p TCString
        forceExprType expr TCInt
        return TCString where
            forceIdentType ident@(Ident stringName) p forcedType = do
                t <- transExpr (Evariable p ident)
                if t == forcedType
                    then return t
                    else throwError (genPosInfo p
                                    ++ "invalid type " ++ genTypeInfo t
                                    ++ ", expected " ++ genTypeInfo forcedType)

    VarTie _ tieEls -> do
        tl <- mapM (transVar . (\(TieEl p var) -> var)) tieEls
        return (TCTuple tl)

transExpr :: Expr -> TC TCType
transExpr expr = case expr of

    Eor _ expr1 expr2 -> do
        forceExprType expr1 TCBool
        forceExprType expr2 TCBool
        return TCBool

    Eand _ expr1 expr2 -> do
        forceExprType expr1 TCBool
        forceExprType expr2 TCBool
        return TCBool

    Ecmp _ expr1 cmpOp expr2 -> case cmpOp of

        CmpEq _ -> do
            t1 <- transExpr expr1
            forceExprType expr2 t1
            return TCBool

        CmpNeq _ -> do
            t1 <- transExpr expr1
            forceExprType expr2 t1
            return TCBool

        _ -> do
            forceExprType expr1 TCInt
            forceExprType expr2 TCInt
            return TCBool

    Ecomplex _ complexVal -> case complexVal of

        ConstTuple _ exprs -> do
            typesList <- mapM transExpr exprs
            return (TCTuple typesList)

        ConstList _ t exprs -> do
            typesList <- mapM (\expr -> forceExprType expr (convType t)) exprs
            return (TCList (convType t))

    Eadd _ expr1 addOp expr2 -> case addOp of

            Plus _ -> do
                t1 <- forceExprOneOfTypes expr1 [TCInt, TCString]
                forceExprType expr2 t1
                return t1

            Minus _ -> do
                forceExprType expr1 TCInt
                forceExprType expr2 TCInt
                return TCInt

    Emul _ expr1 _ expr2 -> do
        forceExprType expr1 TCInt
        forceExprType expr2 TCInt
        return TCInt

    Eneg _ expr -> do
        forceExprType expr TCInt
        return TCInt

    Enot _ expr -> do
        forceExprType expr TCBool
        return TCBool

    Econst _ simpleVal -> case simpleVal of

        ConstInt _ integer -> return TCInt

        ConstBoolTrue _ -> return TCBool

        ConstBoolFalse _ -> return TCBool

        ConstString _ string -> return TCString

    EeasyMethod _ expr easyMethod -> case easyMethod of

        Size _ -> do
            forceExprType expr TCString
            return TCInt

        Empty _ -> do
            t <- transExpr expr
            case t of
                TCString -> return TCBool
                TCList _ -> return TCBool
                _ -> throwError (genPosInfo (hasPosition expr)
                                            ++ "invalid type " ++ genTypeInfo t
                                            ++ ", expected one of 'String', 'List'")

    EhardMethod _ expr hardMethod -> case hardMethod of

        Get _ exprInd -> do
            forceExprType exprInd TCInt
            t <- transExpr expr
            case t of
                TCTuple l -> return (head l) -- problem with the type
                _ -> throwError (genPosInfo (hasPosition expr)
                                ++ "invalid type " ++ genTypeInfo t
                                ++ ", expected 'Tuple'")

        Back _ -> do
            t <- transExpr expr
            case t of
                TCList tl -> return tl
                _ -> throwError (genPosInfo (hasPosition expr)
                                ++ "invalid type " ++ genTypeInfo t
                                ++ ", expected 'List'")

        Front _ -> do
            t <- transExpr expr
            case t of
                TCList tl -> return tl
                _ -> throwError (genPosInfo (hasPosition expr)
                                ++ "invalid type " ++ genTypeInfo t
                                ++ ", expected 'List'")

    EstringEl _ expr1 expr2 -> do
        forceExprType expr1 TCString
        forceExprType expr2 TCInt
        return TCString

    Evariable p ident@(Ident stringName) -> do
        store <- get
        (vEnv, fEnv, _) <- ask
        case M.lookup ident vEnv of
            Nothing -> throwError (genPosInfo p ++ "variable " ++ stringName ++ " not declared")
            Just loc -> case M.lookup loc store of
                Nothing -> throwError (genPosInfo p ++ "variable " ++ stringName ++ " undefined reference")
                Just t -> return t


    EexecFunc p ident@(Ident stringName) exprs -> case stringName of
        "main" -> throwError (genPosInfo p ++ "can't call main function")
        _ -> do
            (_, fEnv, _) <- ask
            (tcType, tcArgs) <- case M.lookup ident fEnv of
                Just (TCFunc tcType tcArgs) -> return (tcType, tcArgs)
                _ -> throwError (genPosInfo p ++ "undeclared function " ++ stringName)
            forceAppliedArgsTypes exprs tcArgs p
            return tcType
