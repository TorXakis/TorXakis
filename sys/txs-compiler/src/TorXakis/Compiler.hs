{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler where

import           Control.Arrow        ( (|||) )
import           Control.Monad.State  ( get, evalStateT )
import qualified Data.Map.Strict      as Map

import           TxsDefs (TxsDefs, union, funcDefs, empty, fromList)
import           StdTDefs (stdTDefs)
import           Sigs    (Sigs, uniqueCombine)
import           Id  (Id (Id))
import           SortId  (sortIdBool, sortIdInt, sortIdRegex, sortIdString)
import           VarId   (VarId)

import           TorXakis.Compiler.Error (Error)
import           TorXakis.Compiler.Data
import           TorXakis.Parser 
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.CstrId
import           TorXakis.Compiler.Defs.TxsDefs
import           TorXakis.Compiler.Defs.Sigs
import           TorXakis.Compiler.ValExpr.VarId
import           TorXakis.Compiler.ValExpr.FuncDef
import           TorXakis.Compiler.ValExpr.ExpDecl

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
    -- Construct the @SortId@'s lookup table.
    sMap <- compileToSortId (adts pd)
    -- Construct the @CstrId@'s lookup table.
    let pdsMap = Map.fromList [ ("Bool", sortIdBool)
                              , ("Int", sortIdInt)
                              , ("Regex", sortIdRegex)
                              , ("String", sortIdString)                              
                              ]
        e0 = emptyEnv { sortIdT = Map.union pdsMap sMap }
    cMap <- compileToCstrId e0 (adts pd)
    let e1 = e0 { cstrIdT = cMap }
    -- Construct the @VarId@'s lookup table.
    vMap <- generateVarIds e1 (fdefs pd)
    let e2 = e1 { varIdT = vMap }
    -- Construct the variable declarations table.
    dMap <- generateVarDecls (fdefs pd)
    let e3 = e2 { varDeclT = dMap }
    -- Construct the function tables.
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
    return $ ad `union` fd `union` fromList stdTDefs

toSigs :: (HasSortIds e, HasCstrIds e, HasFuncIds e, HasFuncDefs e)
       => e -> ParsedDefs -> CompilerM (Sigs VarId)
toSigs e pd = do
    let ts = sortsToSigs (getSortIdMap e)
    as <- adtDeclsToSigs e (adts pd)
    fs <- funDeclsToSigs e (fdefs pd)
    return $ uniqueCombine ts (uniqueCombine as fs)
