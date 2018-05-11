{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses#-}

module Common(Type(TBool, TInt, TString, TChar, TArray, TStruct, TVoid, TFunc), 
    Location, Func(Fun), Value(VBool, VArray, VChar, VFunc, VInt, VString, VStruct, VVoid), Env,
    convertType, getDefaultVal, getValType, declareFunc, declareStruct, declareVar, getVar, setVar
    ,TypeChecker, convertListType, getFuncType, identifierInUse, initialEnv, Interpreter, getVarLocation,
    setValLoc, StateEnv, dump, declareVarLoc, alloc, deepCopy, isFunc)
where

import qualified Data.Map as M
import qualified Absgramm as G
import System.IO (putStrLn)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Cont
import Data.Functor
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import CommonTypes
import Stdlib

initialEnv::Env
initialEnv = foldl (\e (n,a,r,f) -> declareFunc n a r f e) emptyEnv stdFuncs

copyList (m,e@(_,d,_,_,_)) (k,v) =  
    let (dc, ne) = deepCopy (d M.! v) e in 
        let (nl,ne1) = alloc dc ne in 
            ((k,nl):m,ne1)

copyDict d e = let (l,ne) = foldl copyList ([], e) $ M.toList d in
    (M.fromList l, ne)

deepCopy::Value->Env->(Value,Env)
deepCopy (VArray t d) e = 
    let (nmap, ne) = copyDict d e in
    (VArray t nmap, ne) 

deepCopy (VStruct t d) e = 
    let (nmap, ne) = copyDict d e in
    (VStruct t nmap, ne)

deepCopy t e = (t,e)


alloc::Value -> Env ->(Location, Env)
alloc val (l,v,d,f,loc) = 
    (loc, (l,M.insert loc val v,d,f,loc +1))

declareVarLoc::String->Location->Env->Env
declareVarLoc ident loc (l,v,d,f,lo) = 
    (M.insert ident loc l,v,d,f,lo)

declareVar::String->Value->Env->Env
declareVar ident val e = 
    let (nl, (l,v,d,f,loc)) = alloc val e in
    (M.insert ident nl l, v, d, f, loc)

identifierInUse::String ->TypeChecker ()
identifierInUse ident = do
    (v,_,f,_,_) <- ask
    case (const 1 <$> M.lookup ident v) <|> (const 1 <$> M.lookup ident f) of
        Nothing -> return ()
        _ -> throwError $ "Identifier '" ++ ident ++ "' is already in use"

getVarLocation::String->TypeChecker Location
getVarLocation ident = do
    (l,_,_,_,_) <- ask
    return $ l M.! ident

getVar::(MonadReader Env m, MonadError String m, MonadIO m) => String->m Value
getVar ident = do
    (l,v,gl,f,_) <- ask
    case ( (v M.!) <$> M.lookup ident l) <|> (flip VFunc (f M.! ident) <$> M.lookup ident gl) of
        Nothing -> throwError $ "Variable '" ++ ident ++ "' wasnt declared" 
        Just v -> return v 

getFuncType::String -> TypeChecker Type
getFuncType ident = 
    getValType <$> getVar ident `catchError` \_ -> throwError $ "Function '" ++ ident ++ "' is not defined" 

setValLoc::Location->Value->Env->Env
setValLoc local val (l,v,d,f,loc) = 
    (l, M.insert local val v, d, f, loc)

setVar::String->Value->Env->Env
setVar ident val e@(l,_,_,_,_) = 
    setValLoc (l M.! ident) val e

declareStruct::String-> M.Map String Type -> Env -> Env
declareStruct sName map (l,v,d,f,loc) = 
    (l,v, M.insert sName (TStruct sName map) d, f, loc)

declareFunc::String-> [Type] -> Type -> Func -> Env -> Env
declareFunc fName argTypes retType func (l,v,d,f,loc) = 
    (l,v, M.insert fName (TFunc argTypes retType) d, M.insert fName func f, loc)
    
convertType::(MonadReader Env m, MonadError String m) => G.TypeIdent-> m Type
convertType G.TInt = return TInt
convertType G.TBool = return TBool
convertType G.TChar = return TChar
convertType G.TString = return TString
convertType G.TVoid = return TVoid
convertType (G.TStruct (G.Ident ide)) = do
    (_,_,d,_,_) <- ask
    case M.lookup ide d of
        Nothing -> throwError $ "Type '" ++ ide ++ "' is undefined" 
        Just a -> return a

convertType (G.TFunc args ret) = do
    rtt <- convertType ret
    argst <- convertListType args
    return $ TFunc argst rtt

convertType (G.TArray innerType) = TArray <$> convertType innerType

convertListType::(MonadReader Env m, MonadError String m) => [G.TypeIdent] -> m [Type]
convertListType = mapM convertType

getDefaultVal::(MonadState Env m) => Type -> m Value
getDefaultVal TBool = return $ VBool False
getDefaultVal TInt = return $ VInt 0
getDefaultVal TString = return $ VString ""
getDefaultVal TChar = return $ VChar '\0'
getDefaultVal (TArray typ) = return $ VArray typ M.empty 
getDefaultVal typ@(TStruct _ mapFlds) = do
    let mapF (n,t) = getDefaultVal t >>= (state . alloc) >>= (\l -> return (n,l))
    list <- mapM mapF (M.toList mapFlds) 
    let mapped :: M.Map String Location = M.fromList list 
    let val = VStruct typ mapped
    return val
getDefaultVal TVoid = return VVoid
getDefaultVal func@(TFunc _ ret) = 
    return $ VFunc func $ Fun (\v -> getDefaultVal ret) 
