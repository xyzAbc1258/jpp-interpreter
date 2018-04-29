module Common(Type(TBool, TInt, TString, TChar, TArray, TStruct, TVoid, TFunc), 
	Location, Func, Value(VBool, VArray, VChar, VFunc, VInt, VString, VStruct, VVoid), Env,
	convertType, getDefaultVal, getValType, declareFunc, declareStruct, declareVar, getVar, setVar
	,TypeChecker, convertListType, getFuncType, identifierInUse, initialEnv)
where

import qualified Data.Map as M
import qualified Absgramm as G

import Control.Monad.Reader
import Control.Monad.Except
import Data.Functor
type TypeChecker = ReaderT Env (Except String)

data Type = TBool
	| TInt
	| TString
	| TChar
	| TArray Type
	| TStruct String (M.Map String Type)
	| TVoid
	| TFunc [Type] Type
	deriving(Show, Eq)

type Location = Integer
type Func = [Value] -> Value

data Value = VBool Bool
	| VInt Integer
	| VString String
	| VChar Char
	| VArray Type (M.Map Integer Location)
	| VStruct Type (M.Map String Location)
	| VVoid
	| VFunc Type Func


type Env = (M.Map String Location, -- zmienne lokacje
	M.Map Location Value, -- lokacje wartości
	M.Map String Type, -- struktury i funkcje 
	M.Map String Func, -- funkcje 
	Location) -- obecna lokalizacja

type Cont = Env -> Env
	
initialEnv::Env
initialEnv = (M.empty, M.empty, M.empty, M.empty, 0)

declareVar::String->Value->Env->Env
declareVar ident val (l,v,d,f,loc) = 
	(M.insert ident loc l, M.insert loc val v, d, f, loc +1)

identifierInUse::String ->TypeChecker ()
identifierInUse ident = do
	(v,_,f,_,_) <- ask
	case M.lookup ident v of
		Nothing -> case M.lookup ident f of
					   Nothing -> return ()
					   _ -> throwError $ "Identifier '" ++ ident ++ "' is already in use"
		_ -> throwError $ "Identifier '" ++ ident ++ "' is already in use"

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
	 

setVar::String->Value->Env->Env
setVar ident val (l,v,d,f,loc) = 
	(l, M.insert (l M.! ident) val v,d,f,loc)

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
convertListType (a:xa) = do
	t <- convertType a
	tail <- convertListType xa
	return $ t:tail
convertListType [] = return []

getDefaultVal::Type -> Value
getDefaultVal TBool = VBool False
getDefaultVal TInt = VInt 0
getDefaultVal TString = VString ""
getDefaultVal TChar = VChar '\0'
getDefaultVal (TArray typ) = VArray typ M.empty 
getDefaultVal typ@(TStruct _ _) = VStruct typ M.empty
getDefaultVal TVoid = VVoid
getDefaultVal func@(TFunc _ ret) = VFunc func (\v -> getDefaultVal ret) 

getValType::Value -> Type
getValType (VBool _) = TBool
getValType (VInt _) = TInt
getValType (VString _) = TString
getValType (VChar _) = TChar
getValType (VArray typ _) = TArray typ
getValType (VStruct typ _) = typ
getValType VVoid = TVoid
getValType (VFunc typ _) = typ