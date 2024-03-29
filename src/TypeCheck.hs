{-# LANGUAGE FlexibleContexts #-}
module TypeCheck(checkType)
where
import Prelude
import Common
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Data.Functor
import Data.List
import qualified Absgramm as G
import qualified Data.Map as M

getDefVal::G.TypeIdent -> TypeChecker Value
getDefVal d = do 
    nt <- convertType d
    s <- ask
    evalStateT (getDefaultVal nt) s

expectValue::(Eq a, Show a) => TypeChecker a -> a -> TypeChecker()
expectValue reader val = do
    rval <- reader
    when (rval /= val) $
         throwError $ "Expected type '" ++ show val ++ "', given '" ++ show rval ++ "'"

matchType:: (a -> TypeChecker Type) -> a -> a -> [(Type, Type, Type)] -> TypeChecker Type
matchType f a1 a2 triples = do
    t1 <- f a1
    t2 <- f a2
    case filter (\(a,b,_) -> a == t1 && b == t2) triples of
        (_,_,r):_ -> return r
        _ -> throwError $ "Couldnt deduce type in " ++ show t1 ++ " ," ++ show t2 

loopVar = "$loop"

checkType::G.Prog -> Env -> IO (Either String ())
checkType p e = runExceptT $ runReaderT (checkTypeProg p) (declareVar loopVar (VInt 0) e) 

checkTypeProg::G.Prog -> TypeChecker ()
checkTypeProg (G.DProg decs) = checkTypeDecs decs
    
checkTypeDecs::[G.Declaration] -> TypeChecker ()
checkTypeDecs (G.DFunc fDecl@(G.SFuncDecl retType (G.Ident idt) params s):xa) = do
    identifierInUse idt
    nRet <- convertType retType
    retVal <- getDefVal retType
    nArgs <- convertListType (map (\(G.SFuncParam t _) -> t) params) `catchError` 
        (\e -> throwError $ "Error in param list of function '" ++ idt ++ "'. " ++ e) 
    let decFunc = declareFunc idt nArgs nRet (Fun $ const $ return retVal)
    decParamswo <- decParam params 
    let decParams = decParamswo . declareVar "$ret" retVal
    catchError (local (decFunc . decParams) $ (checkTypeStmt s >> checkHasReturn s))
        (\e -> throwError $ "Error in function '" ++ idt ++ "' :" ++ e)
    local decFunc $ checkTypeDecs xa
    where decParam = foldr (liftM2 (.) . toDecVar) (return id)
          toDecVar (G.SFuncParam t (G.Ident idt)) = declareVar idt <$> getDefVal t
    
checkTypeDecs (G.DVarDecl vDecl@(G.SVarDecl typ (G.Ident id) _) :xa) = do
    checkTypeVarDecl vDecl
    val <- getDefVal typ
    local (declareVar id val) $ checkTypeDecs xa
    
checkTypeDecs (G.DStructDecl (G.Ident ident) fields :xa) = do
    identifierInUse ident
    checkTypeFieldDecl fields `catchError` 
        (\e -> throwError $ "Error in struct delcaration '" ++ ident ++"'. " ++ e) 
    nfields <- M.fromList <$> mapM (\(G.SFieldDecl f (G.Ident fname)) -> (\t-> (fname,t)) <$> convertType f) fields
    local (declareStruct ident nfields ) $ checkTypeDecs xa

checkTypeDecs [] = return ()

checkTypeFieldDecl::[G.FieldDecl] -> TypeChecker ()
checkTypeFieldDecl list = do
    convertListType $ map (\(G.SFieldDecl t _) -> t) list
    let dist = length $ nub $ map (\(G.SFieldDecl _ n) -> n) list
    when (length list /= dist) $ throwError "Names of fields have to be unique."

checkTypeVarDecl::G.VarDecl -> TypeChecker Type
checkTypeVarDecl (G.SVarDecl typ (G.Ident ident) (G.EValInit expr)) = do
    identifierInUse ident 
    t <- convertType typ
    checkTypeExpr expr `expectValue` t
    return t

checkTypeVarDecl (G.SVarDecl typ (G.Ident ident) G.ENonInit) = do
    identifierInUse ident 
    convertType typ

checkHasReturn::[G.Stmt] -> TypeChecker ()
checkHasReturn [] = do
    rt <- getValType <$> getVar "$ret" 
    when (rt /= TVoid) $ throwError "There is a branch without return statement"

checkHasReturn (G.SReturn:_) = return ()
checkHasReturn (G.SReturnExpr _ :_) = return ()
checkHasReturn (G.SWhileS _ s :[]) = checkHasReturn s

checkHasReturn (G.SForS _ _ _ _ s : []) = checkHasReturn s
checkHasReturn (G.SIfS _ s1 elseS :[]) = 
    checkHasReturn s1 >> 
    (case elseS of 
        G.SElse es -> checkHasReturn es
        _ -> checkHasReturn [] )
checkHasReturn (_:xa) = checkHasReturn xa

plusOne name e@(d,l,_,_,_) =
    let (VInt i) = l M.! (d M.! name)
        in setVar name (VInt $ i +1) e

checkTypeStmt::[G.Stmt] -> TypeChecker ()
checkTypeStmt [] = return ()

checkTypeStmt (G.SVarDeclS decl@(G.SVarDecl typ (G.Ident ident) _) :xa)= do
    checkTypeVarDecl decl
    val <- getDefVal typ
    local (declareVar ident val) $ checkTypeStmt xa

checkTypeStmt (G.SValAssign bindExpr expr :xa)= do
    bType <- checkTypeBindExpr bindExpr
    checkTypeExpr expr `expectValue` bType
    checkTypeStmt xa

checkTypeStmt (G.SWhileS expr stmts :xa)= do
    checkTypeExpr expr `expectValue` TBool
    local (plusOne loopVar) $ checkTypeStmt stmts
    checkTypeStmt xa

checkTypeStmt (G.SForS (G.SForInit decl@(G.SVarDecl typ (G.Ident ident) _)) expr1 bindExpr expr2 stmts :xa) = do
    checkTypeVarDecl decl
    val <- getDefVal typ
    local (declareVar ident val) $ checkTypeStmt [G.SForS G.SSkip expr1 bindExpr expr2 stmts]
    checkTypeStmt xa
    
checkTypeStmt (G.SForS G.SSkip expr1 bindExpr expr2 stmts :xa) = do
    checkTypeExpr expr1 `expectValue` TBool
    bType <- checkTypeBindExpr bindExpr
    checkTypeExpr expr2 `expectValue` bType
    local (plusOne loopVar) $ checkTypeStmt stmts
    checkTypeStmt xa

checkTypeStmt (G.SIfS expr stmts elseStmt :xa)= do
    checkTypeExpr expr `expectValue` TBool
    checkTypeStmt stmts
    case elseStmt of
        G.SElseEmpty -> checkTypeStmt xa
        G.SElse stmts -> checkTypeStmt stmts >> checkTypeStmt xa

checkTypeStmt (G.SFuncInvS funcInvoke :xa)= do
    checkTypeExpr (G.EFuncInvoke funcInvoke)
    checkTypeStmt xa

checkTypeStmt (G.SReturnExpr expr :xa)= do
    ret <- getVar "$ret"
    let retType = getValType ret
    checkTypeExpr expr `expectValue` retType
    checkTypeStmt xa

checkTypeStmt (G.SReturn :xa)= do
    ret <- getVar "$ret"
    let retType = getValType ret
    case retType of
        TVoid -> checkTypeStmt xa
        _ -> throwError $ "This function returns type " ++ show retType

checkTypeStmt (G.SBreak:xa)= do
    (VInt i) <- getVar loopVar
    when (i == 0) $ throwError "Break outside of loop"

checkTypeStmt (G.SContinue:xa)= do
    (VInt i) <- getVar loopVar
    when (i == 0) $ throwError "Continue outside of loop"

notSupportedMsg stmtName = stmtName ++ " statement is not supported currently";

matchTypeExpr = matchType checkTypeExpr

makeTriple a = (a,a,a)
makeCompTriple a = (a,a,TBool)
makeTriples = map makeTriple
makeCompTriples = map makeCompTriple

checkTypeExpr ::G.Expr -> TypeChecker Type
checkTypeExpr (G.EAdd e1 e2) = 
    matchTypeExpr e1 e2 $ makeTriples [TInt,TString]

checkTypeExpr (G.ESub e1 e2)= 
    matchTypeExpr e1 e2 [makeTriple TInt]

checkTypeExpr (G.EOr e1 e2) = 
    matchTypeExpr e1 e2 [makeTriple TBool]

checkTypeExpr (G.EMul e1 e2) = 
    matchTypeExpr e1 e2 [makeTriple TInt]

checkTypeExpr (G.EDiv e1 e2) = 
    matchTypeExpr e1 e2 [makeTriple TInt]

checkTypeExpr (G.EMod e1 e2) = 
    matchTypeExpr e1 e2 [makeTriple TInt]

checkTypeExpr (G.EAnd e1 e2) = 
    matchTypeExpr e1 e2 [makeTriple TBool]

checkTypeExpr (G.EEq e1 e2) = do
    e1t <- checkTypeExpr e1
    checkTypeExpr e2 `expectValue` e1t
    if isFunc e1t then throwError "Cannot compare functions"
        else return TBool

checkTypeExpr (G.ENeq e1 e2) = 
    checkTypeExpr $ G.EEq e1 e2

checkTypeExpr (G.ELt e1 e2)  = 
    matchTypeExpr e1 e2 $ makeCompTriples [TInt, TString, TChar]

checkTypeExpr (G.EGt e1 e2) = 
    matchTypeExpr e1 e2 $ makeCompTriples [TInt, TString, TChar]

checkTypeExpr (G.ELEt e1 e2) = 
    matchTypeExpr e1 e2 $ makeCompTriples [TInt, TString, TChar]

checkTypeExpr (G.EGEt e1 e2) = 
    matchTypeExpr e1 e2 $ makeCompTriples [TInt, TString, TChar]

checkTypeExpr (G.EBNeg e1) = do 
    checkTypeExpr e1 `expectValue` TBool
    return TBool

checkTypeExpr (G.EBindEx bindExpr) = checkTypeBindExpr bindExpr

checkTypeExpr (G.ERef bindExpr) = TPtr <$> checkTypeBindExpr bindExpr

checkTypeExpr (G.EInt _) = return TInt

checkTypeExpr (G.EChar _) = return TChar

checkTypeExpr (G.EString _) = return TString

checkTypeExpr G.ETrue = return TBool

checkTypeExpr G.EFalse = return TBool

checkTypeExpr (G.EArrCr typ sizeExp) = do
    nt <- convertType typ
    checkTypeExpr sizeExp `expectValue` TInt
    return $ TArray nt 

checkTypeExpr (G.EFuncInvoke (G.FFuncInvoke (G.Ident ident) exprs)) = do
    (TFunc args ret) <- getFuncType ident
    checkArgsTypes args exprs `catchError` 
        (\e -> throwError $ "Error in invocation of function '" ++ ident ++ "'. " ++ e)
    return ret
    where checkArgsTypes [] [] = return ()
          checkArgsTypes (t:tx) (a:xa) = checkTypeExpr a `expectValue` t >> checkArgsTypes tx xa
          checkArgsTypes _ _ = throwError $ "Incorrect arguments count in function invocation '" ++ ident ++ "'"

checkTypeBindExpr::G.BindExpr -> TypeChecker Type
checkTypeBindExpr (G.EBVar (G.Ident ide)) = getValType <$> getVar ide

checkTypeBindExpr (G.EFldAccs bindExpr (G.Ident id)) = do
    t <- checkTypeBindExpr bindExpr
    case t of 
        TStruct n d -> case M.lookup id d of
                         Nothing -> throwError $ "Type '" ++ n ++ "' doesnt contain a field '" ++ id ++"'"
                         Just a -> return a
        _ -> throwError $ "Type '" ++ show t ++ "' is not a structure"

checkTypeBindExpr (G.EArrAccs bindExpr index) = do
    t <- checkTypeBindExpr bindExpr
    case t of 
        TArray inner -> checkTypeExpr index `expectValue` TInt >> return inner
        _ -> throwError "Type is not an array"

checkTypeBindExpr (G.EDeref innerB) = do
    t <- checkTypeBindExpr innerB
    case t of
        TPtr t -> return t
        _ -> throwError "Dereferenced value has to be a pointer"