{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module TorXakis.Compiler where

import           Debug.Trace

import           Control.Arrow                      (first, second, (|||))
import           Control.Lens                       (over, (^.), (^..))
import           Control.Monad.State                (evalStateT, get)
import           Data.Data.Lens                     (uniplate)
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Maybe                         (catMaybes, fromMaybe)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)

import           ChanId                             (ChanId)
import           CstrId                             (CstrId)
import           FuncDef                            (FuncDef (FuncDef))
import           FuncId                             (FuncId (FuncId), name)
import           FuncTable                          (FuncTable,
                                                     Signature (Signature),
                                                     toMap)
import           Id                                 (Id (Id))
import           Sigs                               (Sigs, chan, func, pro,
                                                     uniqueCombine)
import qualified Sigs                               (empty)
import           SortId                             (SortId, sortIdBool,
                                                     sortIdInt, sortIdRegex,
                                                     sortIdString)
import           StdTDefs                           (stdFuncTable, stdTDefs)
import           TxsDefs                            (ProcDef, ProcId, TxsDefs,
                                                     fromList, funcDefs,
                                                     modelDefs, procDefs, union)
import qualified TxsDefs                            (empty)
import           ValExpr                            (ValExpr,
                                                     ValExprView (Vfunc, Vite),
                                                     cstrITE, cstrVar, view)
import           VarId                              (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.ChanId
import           TorXakis.Compiler.Defs.ProcDef
import           TorXakis.Compiler.Defs.Sigs
import           TorXakis.Compiler.Defs.TxsDefs
import           TorXakis.Compiler.Error            (Error)
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Maps.DefinesAMap
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Simplifiable
import           TorXakis.Compiler.ValExpr.Common
import           TorXakis.Compiler.ValExpr.CstrId
import           TorXakis.Compiler.ValExpr.ExpDecl
import           TorXakis.Compiler.ValExpr.FuncDef
import           TorXakis.Compiler.ValExpr.FuncId
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.VarId

import           TorXakis.Compiler.Data.ProcDecl

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
    sIds <- compileToSortIds pd
    cstrIds <- compileToCstrId sIds (pd ^. adts)
    fIds <- compileToFuncIds sIds pd
    let lfDefs = compileToFuncLocs fIds
    decls <- compileToDecls lfDefs pd
    -- Infer the types of all variable declarations.
    let emptyVdMap = Map.empty :: Map (Loc VarDeclE) SortId
    -- We pass to 'inferTypes' an empty map from 'Loc VarDeclE' to 'SortId'
    -- since no variables can be declared at the top level.
    vdSortMap <- inferTypes (sIds :& decls :& fIds :& emptyVdMap) (allFuncs pd)
    -- Construct the variable declarations to @VarId@'s lookup table.
    vIds <- generateVarIds (vdSortMap :& fIds) (allFuncs pd)
    fdefs <- funcDeclsToFuncDefs (vIds :& fIds :& decls) (allFuncs pd)
    -- pdefs <- procDeclsToProcDefMap (sIds :& cstrIds :& fIds :& fdefs :& decls)
    --                                (pd ^. procs)
    pdefs <- compileToProcDefs (sIds :& cstrIds :& fIds :& fdefs :& decls) pd
    chIds <- getMap sIds (pd ^. chdecls) :: CompilerM (Map (Loc ChanDeclE) ChanId)
    let mm = sIds :& pdefs :& cstrIds :& fIds :& fdefs
    sigs    <- toSigs (mm :& chIds) pd
    -- We need the map from channel names to the locations in which these
    -- channels are declared, because the model definitions rely on channels
    -- declared outside its scope.
    chNames <-  getMap () (pd ^. chdecls) :: CompilerM (Map Text (Loc ChanDeclE))
    txsDefs <- toTxsDefs (func sigs) (mm :& decls :& vIds :& vdSortMap :& chNames :& chIds) pd
    St i    <- get
    return (Id i, txsDefs, sigs)

toTxsDefs :: ( MapsTo Text        SortId mm
             , MapsTo (Loc CstrE) CstrId mm
             , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [(Loc FuncDeclE)]) mm
             , MapsTo (Loc FuncDeclE) FuncId mm
             , MapsTo FuncId (FuncDef VarId) mm
             , MapsTo ProcId ProcDef mm
             , MapsTo Text (Loc ChanDeclE) mm
             , MapsTo (Loc ChanDeclE) ChanId mm
             , MapsTo (Loc VarDeclE) VarId mm
             , MapsTo (Loc VarDeclE) SortId mm
             , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False
             , In (ProcId, ()) (Contents mm) ~ 'False )
          => FuncTable VarId -> mm -> ParsedDefs -> CompilerM TxsDefs
toTxsDefs ft mm pd = do
    ads <- adtsToTxsDefs mm (pd ^. adts)
    -- Get the function id's of all the constants.
    cfIds <- traverse (mm .@) (pd ^.. consts . traverse . loc')
    let
        -- TODO: we have to remove the constants to comply with what TorXakis generates :/
        funcDefsNoConsts = Map.withoutKeys (innerMap mm) (Set.fromList cfIds)
        -- TODO: we have to simplify to comply with what TorXakis generates.
        fn = idefsNames mm ++ fmap name cfIds
        fds = TxsDefs.empty {
            funcDefs = simplify ft fn funcDefsNoConsts
            }
        pds = TxsDefs.empty {
            procDefs = simplify ft fn (innerMap mm)
            }
    mDefMap <- modelDeclsToTxsDefs mm (pd ^. models)
    let mds = TxsDefs.empty { modelDefs = simplify ft fn mDefMap }
    return $ ads
        `union` fds
        `union` pds
        `union` fromList stdTDefs
        `union` mds

toSigs :: ( MapsTo Text        SortId mm
          , MapsTo (Loc CstrE) CstrId mm
          , MapsTo (Loc FuncDeclE) FuncId mm
          , MapsTo FuncId (FuncDef VarId) mm
          , MapsTo ProcId ProcDef mm
          , MapsTo (Loc ChanDeclE) ChanId mm)
       => mm -> ParsedDefs -> CompilerM (Sigs VarId)
toSigs mm pd = do
    let ts   = sortsToSigs (innerMap mm)
    as  <- adtDeclsToSigs mm (pd ^. adts)
    fs  <- funDeclsToSigs mm (pd ^. funcs)
    cs  <- funDeclsToSigs mm (pd ^. consts)
    let pidMap :: Map ProcId ProcDef
        pidMap = innerMap mm
        ss = Sigs.empty { func = stdFuncTable
                        , chan = values @(Loc ChanDeclE) mm
                        , pro  = Map.keys pidMap
                        }
    return $ ts `uniqueCombine` as
        `uniqueCombine` fs
        `uniqueCombine` cs
        `uniqueCombine` ss

funcDefInfoNamesMap :: [(Loc FuncDeclE)] -> Map Text [(Loc FuncDeclE)]
funcDefInfoNamesMap fdis =
    groupByName $ catMaybes $ asPair <$> fdis
    where
      asPair :: (Loc FuncDeclE) -> Maybe (Text, (Loc FuncDeclE))
      asPair fdi = (, fdi) <$> fdiName fdi
      groupByName :: [(Text, (Loc FuncDeclE))] -> Map Text [(Loc FuncDeclE)]
      groupByName = Map.fromListWith (++) . fmap (second pure)

-- | Get a dictionary from sort names to their @SortId@. The sorts returned
-- include all the sorts defined by a 'TYPEDEF' (in the parsed definitions),
-- and the predefined sorts ('Bool', 'Int', 'Regex', 'String').
compileToSortIds :: ParsedDefs -> CompilerM (Map Text SortId)
compileToSortIds pd = do
    -- Construct the @SortId@'s lookup table.
    sMap <- compileToSortId (pd ^. adts)
    let pdsMap = Map.fromList [ ("Bool", sortIdBool)
                              , ("Int", sortIdInt)
                              , ("Regex", sortIdRegex)
                              , ("String", sortIdString)
                              ]
    return $ Map.union pdsMap sMap

-- | Get a dictionary from locations of function definitions (included
-- locations of predefined functions), to their function id's.
compileToFuncIds :: Map Text SortId ->  ParsedDefs -> CompilerM (Map (Loc FuncDeclE) FuncId)
compileToFuncIds sIds pd = do
    stdFuncIds <- getStdFuncIds
    cstrFuncIds <- adtsToFuncIds sIds (pd ^. adts)
    -- Construct the function declaration to function id table.
    fIds <- funcDeclsToFuncIds sIds (allFuncs pd)
    -- Join `lFIdMap` and  `stdFuncIds`.
    return $ Map.fromList
        $  fIds
        ++ stdFuncIds
        ++ cstrFuncIds

-- | Get all the functions in the parsed definitions.
allFuncs :: ParsedDefs -> [FuncDecl]
allFuncs pd = pd ^. funcs ++ pd ^. consts

-- | Get a dictionary from the function names to the locations in which these
-- functions are defined.
--
compileToFuncLocs :: Map (Loc FuncDeclE) FuncId -> Map Text [(Loc FuncDeclE)]
compileToFuncLocs fIds = Map.fromListWith (++) $
    fmap mkPair (Map.toList fIds)
    where
      mkPair :: ((Loc FuncDeclE), FuncId) -> (Text, [(Loc FuncDeclE)])
      mkPair (fdi, fId) = (name fId, [fdi])

-- | Get a dictionary from variable references to the possible location in
-- which these variables are declared. Due to overloading a syntactic reference
-- to a variable can refer to a variable, or multiple functions.
compileToDecls :: Map Text [Loc FuncDeclE]
               -> ParsedDefs
               -> CompilerM (Map (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]))
compileToDecls lfDefs pd = do
    let eVdMap = Map.empty :: Map Text (Loc VarDeclE)
    fRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& lfDefs) (allFuncs pd)
    pRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& lfDefs) (pd ^. procs)
    sRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& lfDefs) (pd ^. stauts)
    mRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& lfDefs) (pd ^. models)
    return $ fRtoDs `Map.union` pRtoDs `Map.union` sRtoDs `Map.union` mRtoDs

-- | Generate the map from process id's definitions to process definitions.
compileToProcDefs :: ( MapsTo Text SortId mm
                     , MapsTo FuncId (FuncDef VarId) mm
                     , MapsTo (Loc FuncDeclE) FuncId mm
                     , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                     , In (Loc ChanDeclE, ChanId) (Contents mm) ~ 'False
                     , In (Loc VarDeclE, VarId) (Contents mm) ~ 'False
                     , In (Text, ChanId) (Contents mm) ~ 'False
                     , In (Loc ProcDeclE, ProcInfo) (Contents mm) ~ 'False
                     , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False
                     , In (ProcId, ()) (Contents mm) ~ 'False
                     , In (Loc VarDeclE, SortId) (Contents mm) ~ 'False)
                  => mm -> ParsedDefs -> CompilerM (Map ProcId ProcDef)
compileToProcDefs mm pd = do
    pmsP <- getMap mm (pd ^. procs)  :: CompilerM (Map (Loc ProcDeclE) ProcInfo)
    pmsS <- getMap mm (pd ^. stauts) :: CompilerM (Map (Loc ProcDeclE) ProcInfo)
    let pms = pmsP `Map.union` pmsS -- TODO: we might consider detecting for duplicated process here.
    procPDefMap  <- procDeclsToProcDefMap (pms :& mm) (pd ^. procs)
    stautPDefMap <- stautDeclsToProcDefMap (pms :& mm) (pd ^. stauts)
    trace (show stautPDefMap) $ return $ procPDefMap `Map.union` stautPDefMap
