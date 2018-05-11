{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter(interpret)
where

import Common
import Data.Functor
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Cont
import Data.Map
import VOperations
import qualified Absgramm as G

interpret::G.Prog -> Env -> Either String Env
interpret p e = 
    (<$>) snd $ runExcept $ interpretProg p `runContT` return `runStateT` e `runReaderT` e

interpretExpr::G.Expr -> Env -> Either String Value
interpretExpr p e = 
    (<$>) fst $ runExcept $ interpExpr p `runContT` return `runStateT` e `runReaderT` e

interpretStmts body e =(<$>) snd $ interpStmts body `runContT` return `runStateT` e

interpretProg::G.Prog -> Interpreter r ()
interpretProg (G.DProg decls) =
    mapM_ interpretDecl decls

fixFunc::G.FuncDecl ->([Value] -> StateEnv Value) -> [Value] -> StateEnv Value
fixFunc (G.SFuncDecl retT (G.Ident idt) params body) f args = do
    nRet <- lift $ convertType retT
    nArgs <- mapM (\(G.SFuncParam _ t _) -> (lift $ convertType t)) params    
    let pairs = Prelude.map (\(G.SFuncParam _ _ (G.Ident pN), v) -> (pN,v)) $ zip params args
    let mods = Prelude.map (uncurry declareVar) pairs
    let modAggr = Prelude.foldl (.) id mods
    cst <- get
    let reader = (<$>) snd $ interpStmts body `runContT` return `runStateT` cst
    (_,d,_,_,_) <- lift $ local (modAggr . (declareFunc idt nArgs nRet (Fun f))) reader
    modify (\(l,_,dec,f,loc) -> (l,d,dec,f,loc))
    return $ d!1

interpretDecl::G.Declaration -> Interpreter r ()
interpretDecl (G.DFunc d@(G.SFuncDecl retT (G.Ident idt) params body)) = do
    nRet <- lift $ lift $ convertType retT
    nArgs <- mapM (\(G.SFuncParam _ t _) -> (lift $ lift $ convertType t)) params
    modify (declareFunc idt nArgs nRet (Fun $ fix $ fixFunc d))
            
interpretDecl (G.DVarDecl (G.SVarDecl typ (G.Ident idt) initVal)) = do
    nt <- lift $ lift $ convertType typ
    val <- case initVal of
                G.ENonInit -> lift $ getDefaultVal nt
                G.EValInit expr -> interpExpr expr
    modify (declareVar idt val)

interpretDecl (G.DStructDecl (G.Ident idt) fields) = do
    let flds = Prelude.map (\(G.SFieldDecl t (G.Ident id)) -> (idt, t)) fields
    mappedFlds <- lift $ lift (fromList <$> mapM (\(n,t) -> convertType t >>= (\ct -> return (n,ct))) flds)
    modify (declareStruct idt mappedFlds) 

applyInterpret f a b = liftM2 f (interpExpr a) (interpExpr b)

interpExpr::G.Expr -> Interpreter r Value
interpExpr (G.EAdd e1 e2) = applyInterpret vAdd e1 e2
interpExpr (G.ESub e1 e2) = applyInterpret vSub e1 e2
interpExpr (G.EOr e1 e2) = applyInterpret vOr e1 e2
interpExpr (G.EMul e1 e2) = applyInterpret vMul e1 e2
interpExpr (G.EDiv e1 e2) = applyInterpret vDiv e1 e2
interpExpr (G.EMod e1 e2) = applyInterpret vMod e1 e2
interpExpr (G.EAnd e1 e2) = applyInterpret vAnd e1 e2
interpExpr (G.EEq e1 e2) = do
    v1 <- interpExpr e1
    v2 <- interpExpr e2
    lift $ lift $ vEEq v1 v2
interpExpr (G.ENeq e1 e2) = do
    v1 <- interpExpr e1
    v2 <- interpExpr e2
    lift $ lift $ vNEq v1 v2
interpExpr (G.ELt e1 e2) = applyInterpret vLt e1 e2
interpExpr (G.EGt e1 e2) = applyInterpret vGt e1 e2
interpExpr (G.ELEt e1 e2) = applyInterpret vLEt e1 e2
interpExpr (G.EGEt e1 e2) = applyInterpret vLEt e1 e2
interpExpr (G.EBNeg e) = interpExpr e >>= (\(VBool n) -> return $ VBool $ not n)
interpExpr (G.EChar c) = return $ VChar c 
interpExpr (G.EString s) = return $ VString s
interpExpr G.ETrue = return $ VBool True
interpExpr G.EFalse = return $ VBool False
interpExpr (G.EArrCr t size) = return VVoid
interpExpr (G.EFuncInvoke (G.FFuncInvoke (G.Ident id) args)) = do
    (VFunc _ (Fun f)) <- lift $ lift $ getVar id
    (x:args) <- mapM interpExpr args
    lift $ f args

interpExpr (G.EInt i) = return $ VInt i

interpExpr (G.EBindEx bindExpr) = do
    loc <- interpBindExpr bindExpr
    (_,d,_,_,_) <- get
    return $ d ! loc

interpBindExpr::G.BindExpr -> Interpreter r Location
interpBindExpr (G.EBVar (G.Ident id)) = 
    lift $ lift $ getVarLocation id

interpBindExpr (G.EFldAccs inner (G.Ident fldName)) = do
    (VStruct _ v) <- interpExpr (G.EBindEx inner)
    return $ v ! fldName

interpBindExpr (G.EArrAccs inner index) = do
    (VArray _ v) <- interpExpr (G.EBindEx inner)
    (VInt inx) <- interpExpr index
    return $ v ! inx

interpStmts::[G.Stmt] -> Interpreter r ()
interpStmts = mapM_ interpStmt

interpStmt::G.Stmt -> Interpreter r ()
interpStmt (G.SVarDeclS (G.SVarDecl _ (G.Ident id) (G.EValInit expr))) = do
    val <- interpExpr expr
    callCC $ \c -> local (declareVar id val) $ c ()

interpStmt (G.SVarDeclS (G.SVarDecl t (G.Ident id) G.ENonInit)) = do
    
    val <- lift $ (lift (convertType t)) >>= getDefaultVal 
    callCC $ \c -> local (declareVar id val) $ c ()
    
    
interpStmt (G.SValAssign bindExpr expr) = do
    val <- interpExpr expr
    loc <- interpBindExpr bindExpr
    modify (setValLoc loc val)

interpStmt w@(G.SWhileS expr body) = do
    (VBool v) <- interpExpr expr
    when v (interpStmts body >> interpStmt w)

interpStmt (G.SForS G.SSkip boolCond bindE nValue body) = 
    interpStmt (G.SWhileS boolCond $ body ++ [G.SValAssign bindE nValue])

interpStmt (G.SForS (G.SForInit decl) boolCond bindE nValue body) =
    interpStmt (G.SVarDeclS decl) >> 
        interpStmt (G.SForS G.SSkip boolCond bindE nValue body)

interpStmt (G.SIfS condExpr body elseStmt) = do
    (VBool cond) <- interpExpr condExpr
    if cond then interpStmts body
    else case elseStmt of
            G.SElse elseBody -> interpStmts elseBody
            G.SElseEmpty -> return ()

interpStmt (G.SFuncInvS fInvoke) =
    interpExpr (G.EFuncInvoke fInvoke) >> (return ())

interpStmt (G.SReturnExpr expr) = do
    val <- interpExpr expr
    callCC (\_ -> return ())

interpStmt G.SReturn = callCC (\_ -> return ())
interpStmt G.SBreak = callCC (\_ -> return ())
interpStmt G.SContinue = callCC (\_ -> return ())