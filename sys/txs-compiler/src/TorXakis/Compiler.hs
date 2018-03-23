{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler where

import           Control.Arrow                     ((|||))
import           Control.Monad.State               (evalStateT, get)
import qualified Data.Map.Strict                   as Map
import           Lens.Micro                        ((^.))

import           Id                                (Id (Id))
import           Sigs                              (Sigs, func, uniqueCombine)
import qualified Sigs                              (empty)
import           SortId                            (sortIdBool, sortIdInt,
                                                    sortIdRegex, sortIdString)
import           StdTDefs                          (stdFuncTable, stdTDefs)
import           TxsDefs                           (TxsDefs, fromList, funcDefs,
                                                    union)
import qualified TxsDefs                           (empty)
import           VarId                             (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.Sigs
import           TorXakis.Compiler.Defs.TxsDefs
import           TorXakis.Compiler.Error           (Error)
import           TorXakis.Compiler.ValExpr.CstrId
import           TorXakis.Compiler.ValExpr.ExpDecl
import           TorXakis.Compiler.ValExpr.FuncDef
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.VarId
import           TorXakis.Parser

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
    sMap <- compileToSortId (pd ^. adts)
    -- Construct the @CstrId@'s lookup table.
    let pdsMap = Map.fromList [ ("Bool", sortIdBool)
                              , ("Int", sortIdInt)
                              , ("Regex", sortIdRegex)
                              , ("String", sortIdString)
                              ]
        e0 = emptyEnv { sortIdT = Map.union pdsMap sMap }
    cMap <- compileToCstrId e0 (pd ^. adts)
    let e1 = e0 { cstrIdT = cMap }
    -- Construct the @VarId@'s lookup table.
    vMap <- generateVarIds e1 (pd ^. funcs ++ pd ^. consts)
    let e2 = e1 { varIdT = vMap }
    -- Construct the variable declarations table.
    dMap <- generateVarDecls (pd ^. funcs ++ pd ^. consts)
    let e3 = e2 { varDeclT = dMap }
    -- Construct the function tables.
    (lFIdMap, lFDefMap) <- funcDeclsToFuncDefs e3 (pd ^. funcs ++ pd ^. consts)
    let e4 = e3 { funcIdT = lFIdMap, funcDefT = lFDefMap }
    -- Finally construct the TxsDefs.
    txsDefs <- toTxsDefs e4 pd
    sigs    <- toSigs    e4 pd
    St i    <- get
    return (Id i, txsDefs, sigs)

toTxsDefs :: (HasSortIds e, HasCstrIds e, HasFuncIds e, HasFuncDefs e)
          => e -> ParsedDefs -> CompilerM TxsDefs
toTxsDefs e pd = do
    ad <- adtsToTxsDefs e (pd ^. adts)
    let fd = TxsDefs.empty { funcDefs = getFuncDefT e }
    return $ ad `union` fd `union` fromList stdTDefs

toSigs :: (HasSortIds e, HasCstrIds e, HasFuncIds e, HasFuncDefs e)
       => e -> ParsedDefs -> CompilerM (Sigs VarId)
toSigs e pd = do
    let ts = sortsToSigs (getSortIdMap e)
    as <- adtDeclsToSigs e (pd ^. adts)
    fs <- funDeclsToSigs e (pd ^. funcs)
    cs <- funDeclsToSigs e (pd ^. consts)
    let ss = Sigs.empty { func = stdFuncTable }
    return $ ts `uniqueCombine` as
        `uniqueCombine` fs
        `uniqueCombine` cs
        `uniqueCombine` ss
