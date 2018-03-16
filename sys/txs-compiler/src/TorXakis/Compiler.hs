{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler where

import           Data.Text            ( Text )
import qualified Data.Text            as T
import           Control.Arrow        ( (|||), left, right )
import           Control.Monad        ( replicateM )
import           Control.Monad.State  ( State, get, put, evalState )
import           Control.Monad.Reader ( ReaderT, ask )
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import qualified Data.HashMap.Strict  as HMap
import           Control.Monad.Trans.Class (lift)
import           Data.Semigroup ((<>))

import           TxsDefs (TxsDefs, sortDefs, cstrDefs, union)
import qualified TxsDefs
import           Sigs    (Sigs, uniqueCombine)
import           VarId   (VarId (VarId))
import           SortId  (SortId (SortId), sortIdBool, sortIdInt, sortIdString)
import           CstrId  (CstrId (CstrId))
import           CstrDef (CstrDef (CstrDef))
import           FuncDef (FuncDef (FuncDef))
import           Id  (Id (Id))
import           FuncId  (FuncId (FuncId))
import           SortDef  (SortDef (SortDef))
import           ValExpr (cstrAccess, cstrVar)
import           TorXakis.Sort.ADTDefs ( ADTDefs, addADTDefs, emptyADTDefs, getDefs
                                       , ADTDef, Sort, adtConstructors, adtDefsToMap
                                       , adtSort, adtDefsToList
                                       , Sort (SortBool, SortInt, SortString)
                                       )
import           TorXakis.Sort.ConstructorDefs ( ConstructorDefs, ConstructorDef
                                               , constructorName, fields, cDefsToMap
                                               , constructors                                               
                                               )
import           TorXakis.Sort.FieldDefs (FieldDef, sort, fDefsToList)
import           TorXakis.Sort.Name (Name, toText, getName)
import           TorXakis.Sort.Ref  (Ref, mkRef)

import           TorXakis.Compiler.Error (Error)
import           TorXakis.Compiler.Data
import           TorXakis.Parser (ParsedDefs, adts, fdefs)
import           TorXakis.Compiler.SortId
import           TorXakis.Compiler.CstrId
import           TorXakis.Compiler.Defs.TxsDefs
import           TorXakis.Compiler.Sigs

-- | Compile a string into a TorXakis model.
--
-- TODO: for now we use String's to be able to leverage on the 'haskell' token
-- parser, in the future we might want to change this to text, and benchmark
-- what is the performance gain.
compile :: String -> Either Error (Id, TxsDefs, Sigs VarId)
compile = undefined

-- | Legacy compile function, used to comply with the old interface. It should
-- be deprecated in favor of @compile@.
compileLegacy :: String -> (Id, TxsDefs, Sigs VarId)
compileLegacy = (throwOnLeft ||| id) . compile
    where throwOnLeft = error . show

compileParsedDefs :: ParsedDefs -> CompilerM (Id, TxsDefs, Sigs VarId)
compileParsedDefs pd = do
    -- First construct the @SortId@'s
    sMap <- compileToSortId (adts pd)
    -- Then construct the @CstrId@'s
    cMap <- compileToCstrId (emptyEnv {sortsMap = sMap}) (adts pd)
    -- Finally construct the TxsDefs.
    let e = Env sMap cMap undefined undefined undefined undefined
    txsDefs <- toTxsDefs e pd 
    sigs    <- toSigs    e pd
    St i    <- get
    return (Id i, txsDefs, sigs)
    
toTxsDefs :: Env -> ParsedDefs -> CompilerM TxsDefs
toTxsDefs e pd = do
    ad <- adtsToTxsDefs e (adts pd)
    fd <- funcsToTxsDef e (fdefs pd)
    return $ union ad fd

toSigs :: Env -> ParsedDefs -> CompilerM (Sigs VarId)
toSigs e pd = do
    let ts = sortsToSigs (sortsMap e)
    as <- adtDeclsToSigs e (adts pd)
    fs <- funDeclsToSigs e (fdefs pd)
    return $ uniqueCombine ts (uniqueCombine as fs)
