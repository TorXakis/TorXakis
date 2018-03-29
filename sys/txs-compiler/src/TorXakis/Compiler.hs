{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler where

import           Control.Arrow                     ((|||))
import           Control.Monad.State               (evalStateT, get)
import qualified Data.Map.Strict                   as Map
import qualified Data.Set                          as Set
import           Lens.Micro                        ((^.), (^..))

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
import           TorXakis.Compiler.ValExpr.FuncId
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.VarId
import           TorXakis.Parser
import           TorXakis.Parser.Data

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
        allFuncs = pd ^. funcs ++ pd ^. consts
    -- Construct the variable declarations table.
    dMap <- generateVarDecls allFuncs
    -- Construct the function declaration to function id table.
    lFIdMap <- funcDeclsToFuncIds e1 allFuncs
    let e2 = e1 { varDeclT = dMap
                , funcIdT = lFIdMap }
    -- Infer the types of all variable declarations.
    vdSortMap <- inferTypes e2 allFuncs
    let e3 = e2 { varSortIdT = vdSortMap }
    -- Construct the variable declarations to @VarId@'s lookup table.
    vMap <- generateVarIds e3 allFuncs
    let e4 = e3 { varIdT = vMap }
    lFDefMap <- funcDeclsToFuncDefs e4 allFuncs
    let e5 = e4 { funcDefT = lFDefMap }
    -- Finally construct the TxsDefs.
    txsDefs <- toTxsDefs e5 pd
    sigs    <- toSigs    e5 pd
    St i    <- get
    return (Id i, txsDefs, sigs)

toTxsDefs :: (HasSortIds e, HasCstrIds e, HasFuncIds e, HasFuncDefs e)
          => e -> ParsedDefs -> CompilerM TxsDefs
toTxsDefs e pd = do
    ad <- adtsToTxsDefs e (pd ^. adts)
    -- Get the function id's of all the constants.
    cfIds <- traverse (findFuncIdM e) (pd ^.. consts . traverse . loc')
    let fd = TxsDefs.empty {
            -- TODO: we have to remove the constants to comply with what TorXakis generates :/
            funcDefs = Map.withoutKeys (getFuncDefT e) (Set.fromList cfIds)
            }
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
