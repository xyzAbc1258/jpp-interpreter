{-# LANGUAGE ScopedTypeVariables #-}

module Common(Type(TBool, TInt, TString, TChar, TArray, TStruct, TVoid, TFunc), 
	Location, Func(Fun), Value(VBool, VArray, VChar, VFunc, VInt, VString, VStruct, VVoid), Env,
	convertType, getDefaultVal, getValType, declareFunc, declareStruct, declareVar, getVar, setVar
	,TypeChecker, convertListType, getFuncType, identifierInUse, initialEnv, Interpreter, getVarLocation,
	setValLoc, StateEnv)
where

import qualified Data.Map as M
import qualified Absgramm as G

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Cont
import Data.Functor
import Control.Monad.State

type Interpreter r = ContT r StateEnv
type TypeChecker = ReaderT Env (Except String)
type StateEnv = StateT Env TypeChecker


type FuncDefinition = (String, [Type], Type, Func)

data Type = TBool
	| TInt
	| TString
	| TChar
	| TArray Type
	| TStruct String (M.Map String Type)
	| TVoid
	| TFunc [Type] Type
	deriving(Eq)

instance Show Type where
	show TInt = "int"
	show TString = "string"
	show TChar = "char"
	show (TArray inner) = "Array<" ++ show inner ++ ">"
	show (TStruct name _) = name
	show TVoid = "void"
	show (TFunc a b) = "(" ++ (foldl (\a b -> a ++ "," ++ b) ""  $ map show a) ++ ") -> " ++ show b

type Location = Integer
newtype Func = Fun ([Value] -> StateEnv Value)

data Value = VBool Bool
	| VInt Integer
	| VString String
	| VChar Char
	| VArray Type (M.Map Integer Location)
	| VStruct Type (M.Map String Location)
	| VVoid
	| VFunc Type Func

instance Show Value where
	show (VBool b) = show b
	show (VInt i) = show i
	show (VString s) = "'" ++ s ++ "'"
	show (VArray _ m) = show m
	show (VStruct _ m) = show m
	show VVoid = "void"
	show (VFunc t _) = show t


type Env = (M.Map String Location, -- zmienne lokacje
	M.Map Location Value, -- lokacje wartości
	M.Map String Type, -- struktury i funkcje 
	M.Map String Func, -- funkcje 
	Location) -- obecna lokalizacja

emptyEnv:: Env
emptyEnv = (M.empty, M.empty, M.empty, M.empty, 0)

initialEnv::Env
initialEnv = foldl (\e (n,a,r,f) -> declareFunc n a r f e) emptyEnv stdFuncs

alloc::Value -> Env ->(Location, Env)
alloc val (l,v,d,f,loc) = 
	(loc, (l,M.insert loc val v,d,f,loc +1))

declareVar::String->Value->Env->Env
declareVar ident val e = 
	let (nl, (l,v,d,f,loc)) = alloc val e in
	(M.insert ident nl l, v, d, f, loc)

identifierInUse::String ->TypeChecker ()
identifierInUse ident = do
	(v,_,f,_,_) <- ask
	case M.lookup ident v of
		Nothing -> case M.lookup ident f of
					   Nothing -> return ()
					   _ -> throwError $ "Identifier '" ++ ident ++ "' is already in use"
		_ -> throwError $ "Identifier '" ++ ident ++ "' is already in use"

getVarLocation::String->TypeChecker Location
getVarLocation ident = do
	(l,_,_,_,_) <- ask
	return $ l M.! ident

getVar::String->TypeChecker Value
getVar ident = do
	(l,v,gl,f,_) <- ask
	case M.lookup ident l of
		Nothing -> case M.lookup ident gl of
					   Just ft -> return $ VFunc ft $ f M.! ident
					   Nothing -> throwError $ "Variable '" ++ ident ++ "' wasnt declared"
		Just l -> return $ v M.! l 

getFuncType::String -> TypeChecker Type
getFuncType ident = do
	(v,l,f,_,_) <- ask
	case M.lookup ident v of 
		Just loc -> return $ getValType $ l M.! loc
		_ -> case M.lookup ident f of
				 Just a -> return a
				 _ -> throwError $ "Function '" ++ ident ++ "' is not defined" 

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
	
convertType:: G.TypeIdent-> TypeChecker Type
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

convertListType::[G.TypeIdent] -> TypeChecker [Type]
convertListType = mapM convertType

getDefaultVal::Type -> StateEnv Value
getDefaultVal TBool = return $ VBool False
getDefaultVal TInt = return $ VInt 0
getDefaultVal TString = return $ VString ""
getDefaultVal TChar = return $ VChar '\0'
getDefaultVal (TArray typ) = return $ VArray typ M.empty 
getDefaultVal typ@(TStruct _ mapFlds) = do
	let mapF (n,t) = getDefaultVal t >>= (\v-> state (alloc v)) >>= (\l -> return (n,l))
	list <- mapM mapF (M.toList mapFlds) 
	let mapped :: M.Map String Location = M.fromList list 
	let val = VStruct typ mapped
	modify (snd . (alloc val))
	return val
getDefaultVal TVoid = return VVoid
getDefaultVal func@(TFunc _ ret) = 
	return $ VFunc func $ Fun (\v -> getDefaultVal ret) 

getValType::Value -> Type
getValType (VBool _) = TBool
getValType (VInt _) = TInt
getValType (VString _) = TString
getValType (VChar _) = TChar
getValType (VArray typ _) = TArray typ
getValType (VStruct typ _) = typ
getValType VVoid = TVoid
getValType (VFunc typ _) = typ


stdFuncs::[FuncDefinition]
stdFuncs = [printStr, printInt]

printStr = ("printStr", [TString], TVoid, Fun $ \[x] -> liftIO (print x) >> (return VVoid))

printInt = ("printInt", [TInt], TVoid, Fun $ \[x] -> liftIO (print x) >> (return VVoid))