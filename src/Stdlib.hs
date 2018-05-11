module Stdlib(stdFuncs, FuncDefinition)
where 

import CommonTypes
import System.IO
import Text.Read
import Control.Monad.Trans
import Control.Applicative
import Data.Map as M
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.Except

type FuncDefinition = (String, [Type], Type, Func)

stdFuncs::[FuncDefinition]
stdFuncs = [pprint, toString, ctoi, itoc, readInt, readString, arrLength, assert]

pprint = ("print", [TAll], TVoid, Fun $ \[x] -> liftIO (Prelude.print $ CommonTypes.print x) >> (return VVoid))

toString = ("toString", [TAll], TString, Fun $ \[x] -> return $ VString $ CommonTypes.print x)

ctoi = ("ctoi", [TChar], TInt, Fun $ \[VChar c] -> return $ VInt $ fromIntegral $ ord c)
itoc = ("itoc", [TInt], TChar, Fun $ \[VInt i] -> return $ VChar $ chr $ fromIntegral i)

readInt = ("readInt", [], TInt, Fun $ \[] -> do
    line <- liftIO getLine
    let res = (readMaybe line) <|> (Just 0)
    return $ VInt $ fromJust res)

readString = ("readString", [], TString, Fun $ \[] -> VString <$> liftIO getLine)

arrLength = ("length", [TArray TAll], TInt, Fun $ \[VArray _ d] -> return $ VInt $ fromIntegral $ M.size d)

assert = ("assert", [TBool], TVoid, Fun $ \[VBool v] -> if v then return VVoid else throwError "assert failed")