module CommonTypes
where 

import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Cont
import Data.Functor
import Control.Monad.State

type Location = Integer
newtype Func = Fun ([Value] -> StateEnv Value)    

type Interpreter r a = ContT r StateEnv a
type TypeChecker = ReaderT Env (ExceptT String IO)
type StateEnv = StateT Env TypeChecker

type Env = (M.Map String Location, -- zmienne lokacje
    M.Map Location Value, -- lokacje wartości
    M.Map String Type, -- struktury i funkcje 
    M.Map String Func, -- funkcje 
    Location) -- obecna lokalizacja
    
emptyEnv:: Env
emptyEnv = (M.empty, M.empty, M.empty, M.empty, 0)
    

data Type = TBool
    | TInt
    | TString
    | TChar
    | TArray Type
    | TStruct String (M.Map String Type)
    | TVoid
    | TFunc [Type] Type
    | TAll -- used only to simplified polymorphism

instance Eq Type where
    a /= b = not $ a == b
    TAll == _ = True
    _ == TAll = True
    TBool == TBool = True
    TInt == TInt = True
    TChar == TChar = True
    TVoid == TVoid = True
    TString == TString = True
    (TStruct n1 d1) == (TStruct n2 d2) = n1 == n2 && d1 == d2
    (TFunc a1 r1) == (TFunc a2 r2) = a1 == a2 && r1 == r2
    (TArray t1) == (TArray t2) = t1 == t2
    _ == _ = False

isFunc::Type -> Bool
isFunc (TFunc _ _) = True
isFunc _ = False

instance Show Type where
    show TBool = "bool"
    show TInt = "int"
    show TString = "string"
    show TChar = "char"
    show (TArray inner) = "Array<" ++ show inner ++ ">"
    show (TStruct name d) = name ++ " " ++ show d
    show TVoid = "void"
    show (TFunc a b) = "(" ++ foldl (\a b -> a ++ "," ++ b) ""  (map show a) ++ ") -> " ++ show b
    show TAll = "*"


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
    show (VChar c) = show c
    show (VString s) = "'" ++ s ++ "'"
    show (VArray _ m) = show m
    show (VStruct _ m) = show $ M.toList m
    show VVoid = "void"
    show (VFunc t _) = show t

print::Value -> String
print (VBool b) = show b
print (VInt i) = show i
print (VString s) = s
print (VChar c) = [c]
print (VArray t _) = show t
print (VStruct t _) = show t
print VVoid = "void"
print (VFunc t _) = show t


getValType::Value -> Type
getValType (VBool _) = TBool
getValType (VInt _) = TInt
getValType (VString _) = TString
getValType (VChar _) = TChar
getValType (VArray typ _) = TArray typ
getValType (VStruct typ _) = typ
getValType VVoid = TVoid
getValType (VFunc typ _) = typ


printLn::(Show a, MonadIO m) => a -> m()
printLn a = liftIO $ Prelude.print a

dump::(MonadIO m) => Env -> m()
dump (v,l,t,f,loc) = do
    printLn "Env dump:"
    printLn "Var -> Loc"
    printLn v
    printLn "Loc -> Val"
    printLn l
    printLn "Types"
    printLn t
    printLn "Funcs"
    printLn $ M.keys f