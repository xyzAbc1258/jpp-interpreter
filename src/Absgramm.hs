module Absgramm where

-- Haskell module generated by the BNF converter


newtype Ident = Ident String deriving (Eq,Ord,Show)
data Prog =
   DProg [Declaration]
  deriving (Eq,Ord,Show)

data Declaration =
   DFunc FuncDecl
 | DVarDecl VarDecl
 | DStructDecl Ident [FieldDecl]
  deriving (Eq,Ord,Show)

data FuncDecl =
   SFuncDecl TypeIdent Ident [FuncParam] [Stmt]
  deriving (Eq,Ord,Show)

data FieldDecl =
   SFieldDecl TypeIdent Ident
  deriving (Eq,Ord,Show)

data TypeIdent =
   TInt
 | TBool
 | TChar
 | TString
 | TVoid
 | TStruct Ident
 | TFunc [TypeIdent] TypeIdent
 | TPtr TypeIdent
 | TArray TypeIdent
  deriving (Eq,Ord,Show)

data FuncParam =
   SFuncParam TypeIdent Ident
  deriving (Eq,Ord,Show)

data VarDecl =
   SVarDecl TypeIdent Ident InitExpr
  deriving (Eq,Ord,Show)

data InitExpr =
   ENonInit
 | EValInit Expr
  deriving (Eq,Ord,Show)

data Stmt =
   SVarDeclS VarDecl
 | SValAssign BindExpr Expr
 | SWhileS Expr [Stmt]
 | SForS ForInit Expr BindExpr Expr [Stmt]
 | SIfS Expr [Stmt] ElseStmt
 | SFuncInvS FuncInvoke
 | SReturnExpr Expr
 | SReturn
 | SBreak
 | SContinue
  deriving (Eq,Ord,Show)

data ElseStmt =
   SElse [Stmt]
 | SElseEmpty
  deriving (Eq,Ord,Show)

data BindExpr =
   EDeref BindExpr
 | EBVar Ident
 | EFldAccs BindExpr Ident
 | EArrAccs BindExpr Expr
  deriving (Eq,Ord,Show)

data ForInit =
   SForInit VarDecl
 | SSkip
  deriving (Eq,Ord,Show)

data Expr =
   EAdd Expr Expr
 | ESub Expr Expr
 | EOr Expr Expr
 | EMul Expr Expr
 | EDiv Expr Expr
 | EMod Expr Expr
 | EAnd Expr Expr
 | EEq Expr Expr
 | ENeq Expr Expr
 | ELt Expr Expr
 | EGt Expr Expr
 | ELEt Expr Expr
 | EGEt Expr Expr
 | EBNeg Expr
 | EBindEx BindExpr
 | ERef BindExpr
 | EInt Integer
 | EChar Char
 | EString String
 | ETrue
 | EFalse
 | EArrCr TypeIdent Expr
 | EFuncInvoke FuncInvoke
  deriving (Eq,Ord,Show)

data FuncInvoke =
   FFuncInvoke Ident [Expr]
  deriving (Eq,Ord,Show)

