{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler where

import           Data.Text            ( Text )
import qualified Data.Text            as T
import           Control.Arrow        ( (|||), left, right )
import           Control.Monad        ( replicateM )
import           Control.Monad.State  ( State, get, put, evalStateT )
import           Control.Monad.Reader ( ReaderT, ask )
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import qualified Data.HashMap.Strict  as HMap
import           Control.Monad.Trans.Class (lift)
import           Data.Semigroup ((<>))

import           TxsDefs (TxsDefs, sortDefs, cstrDefs, union, funcDefs, empty)
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
import           TorXakis.Parser 
import           TorXakis.Parser.Data hiding (St)
import           TorXakis.Compiler.SortId
import           TorXakis.Compiler.CstrId
import           TorXakis.Compiler.Defs.TxsDefs
import           TorXakis.Compiler.Sigs
import           TorXakis.Compiler.ValExpr.VarId
import           TorXakis.Compiler.ValExpr.FuncDef
import           TorXakis.Compiler.ExpDecl

-- | Compile a string into a TorXakis model.
--
compileFile :: String -> IO (Either Error (Id, TxsDefs, Sigs VarId))
compileFile fp = do
    ePd <- parseFile fp
    case ePd of
        Left err -> return . Left $ err
        Right pd -> return $
            evalStateT (runCompiler . compileParsedDefs $ pd) newState

-- | Legacy compile function, used to comply with the old interface. It should
-- be deprecated in favor of @compile@.
compileLegacy :: String -> (Id, TxsDefs, Sigs VarId)
compileLegacy = (throwOnLeft ||| id) . compile
    where throwOnLeft = error . show
          compile :: String -> Either Error (Id, TxsDefs, Sigs VarId)
          compile = undefined

compileParsedDefs :: ParsedDefs -> CompilerM (Id, TxsDefs, Sigs VarId)
compileParsedDefs pd = do
    -- First construct the @SortId@'s
    sMap <- compileToSortId (adts pd)
    -- Then construct the @CstrId@'s
    let pdsMap = Map.fromList [ ("Int", sortIdInt)
                              , ("Bool", sortIdBool)
                              ]
        e0 = emptyEnv { sortIdT = Map.union pdsMap sMap }
    cMap <- compileToCstrId e0 (adts pd)
    let e1 = e0 { cstrIdT = cMap }
    vMap <- generateVarIds e1 (fdefs pd)
    let e2 = e1 { varIdT = vMap }
    dMap <- generateVarDecls (fdefs pd)
    let e3 = e2 { varDeclT = dMap }
    (lFIdMap, lFDefMap) <- funcDeclsToFuncDefs e3 (fdefs pd)
    let e4 = e3 { funcIdT = lFIdMap, funcDefT = lFDefMap }
    -- Finally construct the TxsDefs.
    txsDefs <- toTxsDefs e4 pd 
    sigs    <- toSigs    e4 pd
    St i    <- get
    return (Id i, txsDefs, sigs)
    
toTxsDefs :: (HasSortIds e, HasCstrIds e, HasFuncIds e, HasFuncDefs e)
          => e -> ParsedDefs -> CompilerM TxsDefs
toTxsDefs e pd = do
    ad <- adtsToTxsDefs e (adts pd)
    let fd = empty { funcDefs = getFuncDefT e }
    return $ union ad fd

toSigs :: (HasSortIds e, HasCstrIds e, HasFuncIds e, HasFuncDefs e)
       => e -> ParsedDefs -> CompilerM (Sigs VarId)
toSigs e pd = do
    let ts = sortsToSigs (getSortIdMap e)
    as <- adtDeclsToSigs e (adts pd)
    fs <- funDeclsToSigs e (fdefs pd)
    return $ uniqueCombine ts (uniqueCombine as fs)
