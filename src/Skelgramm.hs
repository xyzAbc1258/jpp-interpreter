module Skelgramm where

-- Haskell module generated by the BNF converter

import Absgramm
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transProg :: Prog -> Result
transProg x = case x of
  DProg declarations  -> failure x


transDeclaration :: Declaration -> Result
transDeclaration x = case x of
  DFunc funcdecl  -> failure x
  DVarDecl vardecl  -> failure x
  DStructDecl id fielddecls  -> failure x


transFuncDecl :: FuncDecl -> Result
transFuncDecl x = case x of
  SFuncDecl typeident id funcparams stmts  -> failure x


transFieldDecl :: FieldDecl -> Result
transFieldDecl x = case x of
  SFieldDecl typeident id  -> failure x


transTypeIdent :: TypeIdent -> Result
transTypeIdent x = case x of
  TInt  -> failure x
  TBool  -> failure x
  TChar  -> failure x
  TString  -> failure x
  TVoid  -> failure x
  TStruct id  -> failure x
  TFunc typeidents typeident  -> failure x
  TPtr typeident  -> failure x
  TArray typeident  -> failure x


transFuncParam :: FuncParam -> Result
transFuncParam x = case x of
  SFuncParam typeident id  -> failure x


transVarDecl :: VarDecl -> Result
transVarDecl x = case x of
  SVarDecl typeident id initexpr  -> failure x


transInitExpr :: InitExpr -> Result
transInitExpr x = case x of
  ENonInit  -> failure x
  EValInit expr  -> failure x


transStmt :: Stmt -> Result
transStmt x = case x of
  SVarDeclS vardecl  -> failure x
  SValAssign bindexpr expr  -> failure x
  SWhileS expr stmts  -> failure x
  SForS forinit expr1 bindexpr2 expr3 stmts4  -> failure x
  SIfS expr stmts elsestmt  -> failure x
  SFuncInvS funcinvoke  -> failure x
  SReturnExpr expr  -> failure x
  SReturn  -> failure x
  SBreak  -> failure x
  SContinue  -> failure x


transElseStmt :: ElseStmt -> Result
transElseStmt x = case x of
  SElse stmts  -> failure x
  SElseEmpty  -> failure x


transBindExpr :: BindExpr -> Result
transBindExpr x = case x of
  EDeref bindexpr  -> failure x
  EBVar id  -> failure x
  EFldAccs bindexpr id  -> failure x
  EArrAccs bindexpr expr  -> failure x


transForInit :: ForInit -> Result
transForInit x = case x of
  SForInit vardecl  -> failure x
  SSkip  -> failure x


transExpr :: Expr -> Result
transExpr x = case x of
  EAdd expr1 expr2  -> failure x
  ESub expr1 expr2  -> failure x
  EOr expr1 expr2  -> failure x
  EMul expr1 expr2  -> failure x
  EDiv expr1 expr2  -> failure x
  EMod expr1 expr2  -> failure x
  EAnd expr1 expr2  -> failure x
  EEq expr1 expr2  -> failure x
  ENeq expr1 expr2  -> failure x
  ELt expr1 expr2  -> failure x
  EGt expr1 expr2  -> failure x
  ELEt expr1 expr2  -> failure x
  EGEt expr1 expr2  -> failure x
  EBNeg expr  -> failure x
  EBindEx bindexpr  -> failure x
  ERef bindexpr  -> failure x
  EInt n  -> failure x
  EChar c  -> failure x
  EString str  -> failure x
  ETrue  -> failure x
  EFalse  -> failure x
  EArrCr typeident expr  -> failure x
  EFuncInvoke funcinvoke  -> failure x


transFuncInvoke :: FuncInvoke -> Result
transFuncInvoke x = case x of
  FFuncInvoke id exprs  -> failure x



