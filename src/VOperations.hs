{-# LANGUAGE FlexibleContexts #-}
module VOperations(vAdd
, vSub
, vMul
, vDiv
, vMod
, vOr
, vAnd
, vEEq
, vNEq
, vLt
, vGt
, vLEt
, vGEt)
where

import Control.Monad.Reader
import Prelude hiding (negate)
import Common
import Data.Map

type BinVOp = Value -> Value -> Value

vAdd::BinVOp
vAdd (VInt a) (VInt b) = VInt $ a + b
vAdd (VString a) (VString b) = VString $ a ++ b

vSub::BinVOp
vSub (VInt a) (VInt b) = VInt $ a - b

vMul::BinVOp
vMul (VInt a) (VInt b) = VInt $ a * b

vDiv::BinVOp
vDiv (VInt a) (VInt b) = VInt $ div a b

vMod::BinVOp
vMod (VInt a) (VInt b) = VInt $ mod a b

vOr::BinVOp
vOr (VBool a) (VBool b) = VBool $ a || b

vAnd::BinVOp
vAnd (VBool a) (VBool b) = VBool $ a && b

retBool::(Monad m) => Bool -> m Value
retBool = return . VBool

vEEq::(MonadReader Env m) => Value -> Value -> m Value
vEEq (VInt a) (VInt b) = retBool $ a == b
vEEq (VString a) (VString b) = retBool $ a == b
vEEq (VBool a) (VBool b) = retBool $ a == b
vEEq (VChar a) (VChar b) = retBool $ a == b

vEEq (VArray t1 a) (VArray t2 b) | t1 == t2 =
    if size a == size b then vCompDicts a b else retBool False

vEEq (VStruct t1 a) (VStruct t2 b) | t1 == t2 = 
    vCompDicts a b

vCompDicts::(MonadReader Env m, Ord a, Eq a) => Map a Location -> Map a Location -> m Value
vCompDicts a b = do
    (_,vl,_,_,_) <- ask
    let akeys= keys a
    let avals = Prelude.map (\k -> vl! (a!k)) akeys
    let bvals = Prelude.map (\k -> vl! (b!k)) akeys
    let pairs = zip avals bvals
    vbools <- mapM (uncurry vEEq) pairs 
    let bools = Prelude.map (\(VBool v) -> v) vbools
    retBool $ and bools

negate ret (VBool v) = ret $ VBool $ not v

vNEq::(MonadReader Env m) => Value -> Value -> m Value
vNEq a b = vEEq a b >>= (negate return)
vLt::BinVOp
vLt (VInt a) (VInt b) = VBool $ a < b
vLt (VString a) (VString b) = VBool $ a < b
vLt (VChar a) (VChar b) = VBool $ a < b
vGt::BinVOp
vGt (VInt a) (VInt b) = VBool $ a > b
vGt (VString a) (VString b) = VBool $ a > b
vGt (VChar a) (VChar b) = VBool $ a > b
vLEt::BinVOp
vLEt a b = negate id $ vGt a b
vGEt::BinVOp
vGEt a b = negate id $ vLt a b