module TypeCheck(checkType)
where
import Prelude
import Common
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Functor
import qualified Absgramm as G
import qualified Data.Map as M

getDefVal::G.TypeIdent -> TypeChecker Value
getDefVal d = getDefaultVal <$> convertType d

expectValue::(Eq a, Show a) => TypeChecker a -> a -> TypeChecker()
expectValue reader val = do
	rval <- reader
	case rval == val of
		True -> return ()
		False -> throwError $ "Expected type '" ++ show val ++ "', given '" ++ show rval ++ "'"

matchType:: (a -> TypeChecker Type) -> a -> a -> [(Type, Type, Type)] -> TypeChecker Type
matchType f a1 a2 triples = do
	t1 <- f a1
	t2 <- f a2
	let ok = filter (\(a,b,c) -> a == t1 && b == t2) triples
	case ok of
		(_,_,r):_ -> return r
		_ -> throwError $ "Couldnt deduce type in " ++ show t1 ++ " ," ++ show t2 

checkType::G.Prog -> Env -> Either String ()
checkType p e = runExcept $ runReaderT (checkTypeProg p) e 

checkTypeProg::G.Prog -> TypeChecker ()
checkTypeProg (G.DProg decs) = checkTypeDecs decs
	
checkTypeDecs::[G.Declaration] -> TypeChecker ()
checkTypeDecs ((G.DFunc fDecl@(G.SFuncDecl retType (G.Ident idt) params s)):xa) = do
	identifierInUse idt
	nRet <- convertType retType
	let retVal = getDefaultVal nRet
	nArgs <- convertListType $ map (\(G.SFuncParam _ t _) -> t) params
	let decFunc = declareFunc idt nArgs nRet (const retVal)
	decParamswo <- decParam params 
	let decParams = decParamswo . (declareVar "$ret" $ getDefaultVal nRet)
	local (decFunc . decParams) $ checkTypeStmt s
	local decFunc $ checkTypeDecs xa
	where decParam (a:xa) = liftM2 (.) (toDecVar a) (decParam xa)
	      decParam [] = return id
	      toDecVar (G.SFuncParam _ t (G.Ident idt)) = declareVar idt <$> getDefVal t
	
checkTypeDecs ((G.DVarDecl vDecl@(G.SVarDecl typ (G.Ident id) _)):xa) = do
	checkTypeVarDecl vDecl
	val <- getDefVal typ
	local (declareVar id val) $ checkTypeDecs xa
	
checkTypeDecs ((G.DStructDecl (G.Ident ident) fields):xa) = do
	identifierInUse ident
	checkTypeFieldDecl fields
	nfields <- convert fields M.empty
	local (declareStruct ident nfields )  $ checkTypeDecs xa
	where convert ((G.SFieldDecl ftype (G.Ident fname)):xs) m = do
			nType <- convertType ftype
			convert xs (M.insert fname nType m)
	      convert [] m = return m

checkTypeDecs [] = return ()

checkTypeFieldDecl::[G.FieldDecl] -> TypeChecker ()
checkTypeFieldDecl ((G.SFieldDecl typ _):xa) = do
	convertType typ
	checkTypeFieldDecl xa
checkTypeFieldDecl [] = return ()

-- TODO
checkTypeVarDecl::G.VarDecl -> TypeChecker Type
checkTypeVarDecl (G.SVarDecl typ (G.Ident ident) (G.EValInit expr)) = do
	identifierInUse ident 
	t <- convertType typ
	checkTypeExpr expr `expectValue` t
	return t

checkTypeVarDecl (G.SVarDecl typ (G.Ident ident) G.ENonInit) = do
	identifierInUse ident 
	convertType typ

checkTypeStmt::[G.Stmt] -> TypeChecker ()
checkTypeStmt [] = return ()

checkTypeStmt ((G.SVarDeclS decl@(G.SVarDecl typ (G.Ident ident) _)):xa)= do
	checkTypeVarDecl decl
	val <- getDefVal typ
	local (declareVar ident val) $ checkTypeStmt xa

checkTypeStmt ((G.SValAssign bindExpr expr):xa)= do
	bType <- checkTypeBindExpr bindExpr
	checkTypeExpr expr `expectValue` bType
	checkTypeStmt xa

checkTypeStmt ((G.SWhileS expr stmts):xa)= do
	checkTypeExpr expr `expectValue` TBool
	local id $ checkTypeStmt stmts
	checkTypeStmt xa

checkTypeStmt ((G.SForS (G.SForInit decl@(G.SVarDecl typ (G.Ident ident) _)) expr1 bindExpr expr2 stmts):xa) = do
	checkTypeVarDecl decl
	val <- getDefVal typ
	local (declareVar ident val) $ checkTypeStmt $ (G.SForS G.SSkip expr1 bindExpr expr2 stmts):[]
	checkTypeStmt xa
	

checkTypeStmt ((G.SForS G.SSkip expr1 bindExpr expr2 stmts):xa) = do
	checkTypeExpr expr1 `expectValue` TBool
	bType <- checkTypeBindExpr bindExpr
	checkTypeExpr expr2 `expectValue` bType
	checkTypeStmt stmts
	checkTypeStmt xa

checkTypeStmt ((G.SIfS expr stmts elseStmt):xa)= do
	checkTypeExpr expr `expectValue` TBool
	local id checkTypeStmt stmts
	case elseStmt of
		G.SElseEmpty -> checkTypeStmt xa
		G.SElse stmts -> checkTypeStmt stmts >> checkTypeStmt xa

checkTypeStmt ((G.SFuncInvS funcInvoke):xa)= do
	checkTypeExpr (G.EFuncInvoke funcInvoke)
	checkTypeStmt xa

checkTypeStmt ((G.SReturnExpr expr):xa)= do
	ret <- getVar "$ret"
	let retType = getValType ret
	checkTypeExpr expr `expectValue` retType
	checkTypeStmt xa

checkTypeStmt (G.SReturn :xa)= do
	ret <- getVar "$ret"
	let retType = getValType ret
	case retType of
		TVoid -> checkTypeStmt xa
		_ -> throwError "This function returns type void"

checkTypeStmt (G.SBreak:xa)= return ()
checkTypeStmt (G.SContinue:xa)= return ()

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
	return TBool

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
	checkArgsTypes args exprs
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