{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Interpreter(interpret)
where

import Common
import Data.Functor
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Trans.Cont hiding(callCC)
import Data.Functor.Identity
import Data.Map
import VOperations
import qualified Absgramm as G

data FlowType = None | Break | Return | Continue

instance (MonadError e m) => MonadError e (ContT r m) where
    throwError = lift . throwError
    catchError op h = ContT $ \k -> catchError (runContT op k) (\e -> runContT (h e) k)

interpret::G.Prog -> Env -> IO(Either String Env)
interpret p e = 
    runExceptT $ interpretProg p `runContT` (\_ -> return ()) `execStateT` e `runReaderT` e

interpretProg::G.Prog -> Interpreter () ()
interpretProg (G.DProg decls) = 
    mapM_ interpretDecl decls

convertTypeLocal::(MonadReader Env m, MonadState Env m, MonadError String m)=>G.TypeIdent -> m Type
convertTypeLocal t = do
    s <- get
    local (const s) $ convertType t

getVarLocal::(MonadReader Env m, MonadState Env m, MonadError String m, MonadIO m)=>String -> m Value
getVarLocal t = do
    s <- get
    local (const s) $ getVar t

safeDA::(MonadError String m, Show a, Ord a) => Data.Map.Map a b -> a -> m b
safeDA m k =
    case Data.Map.lookup k m of
        Just r -> return r
        Nothing -> throwError $ "Couldnt find key: " ++ show k

fixFunc::G.FuncDecl ->([Value] -> StateEnv Value) -> [Value] -> StateEnv Value
fixFunc d@(G.SFuncDecl retT (G.Ident idt) params body) toFix args = do
    nRet <- convertTypeLocal retT
    nArgs <- mapM (\(G.SFuncParam _ t _) -> (convertTypeLocal t)) params    
    let pairs = Prelude.map (\(G.SFuncParam _ _ (G.Ident pN), v) -> (pN,v)) $ zip params args
    let mods = Prelude.map (\(p,v) e -> let (nv,ne) = deepCopy v e in declareVar p nv ne) pairs
    let modAggr = Prelude.foldl (.) id mods
    (l,oldd,dec,funcs,loc) <- get
    (retl, nEnv) <- gets $ alloc VVoid
    modify (const nEnv)
    modify (modAggr . declareVarLoc "$ret" retl . declareFunc idt nArgs nRet (Fun toFix))
    resetT (withContT (\c _ -> c None) $ interpStmts body) `runContT` return `catchError` 
        \e -> throwError $ "Error in func " ++ idt ++ ". " ++ e
    (_,d,_,_,nloc) <- get
    modify (const (l,d,dec,funcs,nloc))
    safeDA d retl

interpretDecl::G.Declaration -> Interpreter () ()
interpretDecl (G.DFunc d@(G.SFuncDecl retT (G.Ident idt) params body)) = do
    nRet <- convertTypeLocal retT
    nArgs <- mapM (\(G.SFuncParam _ t _) -> convertTypeLocal t) params
    modify (declareFunc idt nArgs nRet (Fun $ fix $ fixFunc d))

interpretDecl (G.DVarDecl (G.SVarDecl t (G.Ident id) initVal)) = do
    val <- case initVal of
            G.EValInit expr -> interpExpr expr >>= (state . deepCopy)
            G.ENonInit -> convertTypeLocal t >>= getDefaultVal 
    modify (declareVar id val)

interpretDecl (G.DStructDecl (G.Ident idt) fields) = do
    let flds = Prelude.map (\(G.SFieldDecl t (G.Ident id)) -> (id, t)) fields
    mappedFlds <- fromList <$> mapM (\(n,t) -> convertTypeLocal t >>= (\ct -> return (n,ct))) flds
    modify (declareStruct idt mappedFlds) 

applyInterpret f a b = liftM2 f (interpExpr a) (interpExpr b)
applyNonZero f a b opName = do
    bv@(VInt bval) <- interpExpr b
    case bval of
        0 -> throwError $ opName ++ " by 0"
        l -> flip f bv <$> interpExpr a 

interpExpr::G.Expr -> Interpreter r Value
interpExpr (G.EAdd e1 e2) = applyInterpret vAdd e1 e2
interpExpr (G.ESub e1 e2) = applyInterpret vSub e1 e2
interpExpr (G.EOr e1 e2) = applyInterpret vOr e1 e2
interpExpr (G.EMul e1 e2) = applyInterpret vMul e1 e2
interpExpr (G.EDiv e1 e2) = applyNonZero vDiv e1 e2 "Division"
interpExpr (G.EMod e1 e2) = applyNonZero vMod e1 e2 "Modulo"
interpExpr (G.EAnd e1 e2) = applyInterpret vAnd e1 e2
interpExpr (G.EEq e1 e2) = do
    v1 <- interpExpr e1
    v2 <- interpExpr e2
    vEEq v1 v2
interpExpr (G.ENeq e1 e2) = do
    v1 <- interpExpr e1
    v2 <- interpExpr e2
    vNEq v1 v2
interpExpr (G.ELt e1 e2) = applyInterpret vLt e1 e2
interpExpr (G.EGt e1 e2) = applyInterpret vGt e1 e2
interpExpr (G.ELEt e1 e2) = applyInterpret vLEt e1 e2
interpExpr (G.EGEt e1 e2) = applyInterpret vLEt e1 e2
interpExpr (G.EBNeg e) = interpExpr e >>= (\(VBool n) -> return $ VBool $ not n)
interpExpr (G.EChar c) = return $ VChar c 
interpExpr (G.EString s) = return $ VString s
interpExpr G.ETrue = return $ VBool True
interpExpr G.EFalse = return $ VBool False
interpExpr (G.EArrCr t size) = do
    nt <- convertTypeLocal t
    val <-  getDefaultVal nt
    (VInt vSize) <- interpExpr size
    nstate <- get
    let (l,s) = Prelude.foldl (\(l,e) i-> let (nl,ne) = alloc val e in ((i,nl):l,ne)) ([],nstate) [0..(vSize -1)] 
    modify (const s)
    return $ VArray nt $ fromList l 

interpExpr (G.ERef bindExpr) = do
    loc <- interpBindExpr bindExpr
    typ <- getValType <$> gets (\(_,d,_,_,_) -> d!loc)
    return $ VPtr typ loc

interpExpr (G.EFuncInvoke (G.FFuncInvoke (G.Ident id) args)) = do
    (VFunc _ (Fun f)) <- getVarLocal id
    margs <- mapM interpExpr args
    lift $ f margs

interpExpr (G.EInt i) = return $ VInt i

interpExpr (G.EBindEx (G.EBVar (G.Ident id))) =
     getVarLocal id

interpExpr (G.EBindEx bindExpr) = do
    loc <- interpBindExpr bindExpr
    (_,d,_,_,_) <- get
    return $ d ! loc

interpBindExpr::G.BindExpr -> Interpreter r Location
interpBindExpr (G.EBVar (G.Ident id)) = do 
    (v,_,_,_,_) <- get
    safeDA v id

interpBindExpr (G.EFldAccs inner (G.Ident fldName)) = do
    (VStruct _ v) <- interpExpr (G.EBindEx inner)
    safeDA v fldName

interpBindExpr (G.EArrAccs inner index) = do
    (VArray _ v) <- interpExpr (G.EBindEx inner)
    (VInt inx) <- interpExpr index
    safeDA v inx

interpBindExpr (G.EDeref bindExpr) = do
    (VPtr _ l) <- interpExpr (G.EBindEx bindExpr)
    return l

interpStmts::[G.Stmt] -> Interpreter FlowType FlowType
interpStmts l = mapM_ interpStmt l >> (return None)

interpStmt::G.Stmt -> Interpreter FlowType FlowType
interpStmt (G.SVarDeclS (G.SVarDecl t (G.Ident id) initVal)) = do
    val <- case initVal of
            G.EValInit expr -> interpExpr expr >>= (state . deepCopy)
            G.ENonInit -> convertTypeLocal t >>= getDefaultVal 
    modify (declareVar id val)
    return None

interpStmt (G.SValAssign bindExpr expr) = do
    val <- interpExpr expr >>= state . deepCopy
    loc <- interpBindExpr bindExpr
    modify (setValLoc loc val)
    return None

interpStmt w@(G.SWhileS expr body) = fix $ \loop -> do
    (VBool v) <- interpExpr expr
    if not v then return None
     else do 
        r <- resetT $ interpStmts body
        case r of
            Break ->return None
            Return -> shiftT $ \_ -> return Return
            _ -> loop

interpStmt (G.SForS G.SSkip boolCond bindE nValue body) = fix $ \loop -> do
    (VBool v) <- interpExpr boolCond
    if not v  then return None
      else do
        res <- resetT $ interpStmts body
        case res of 
            Break -> return None
            Return -> shiftT $ \_ -> return Return
            _ -> interpStmt (G.SValAssign bindE nValue) >> loop
            

interpStmt (G.SForS (G.SForInit decl) boolCond bindE nValue body) =
    interpStmt (G.SVarDeclS decl) >> 
        interpStmt (G.SForS G.SSkip boolCond bindE nValue body)

interpStmt (G.SIfS condExpr body elseStmt) = do
    (VBool cond) <- interpExpr condExpr
    if cond then interpStmts body
    else case elseStmt of
            G.SElse elseBody -> interpStmts elseBody
            G.SElseEmpty -> return None

interpStmt (G.SFuncInvS fInvoke) = do
    interpExpr $ G.EFuncInvoke fInvoke
    return None

interpStmt (G.SReturnExpr expr) = shiftT $ \_ -> do
    val <- interpExpr expr
    modify $ setVar "$ret" val
    return Return

interpStmt G.SReturn = shiftT $ \_ -> return Return
interpStmt G.SBreak = shiftT $ \_ -> return Break
interpStmt G.SContinue = shiftT $ \_ -> return Continue