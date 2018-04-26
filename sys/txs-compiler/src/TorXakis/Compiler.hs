{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
module TorXakis.Compiler where

import           Control.Arrow                     (first, second, (|||))
import           Control.Lens                      (over, (^.), (^..))
import           Control.Monad.State               (evalStateT, get)
import           Data.Data.Lens                    (uniplate)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (catMaybes, fromMaybe)
import qualified Data.Set                          as Set
import           Data.Text                         (Text)

import           FuncDef                           (FuncDef (FuncDef))
import           FuncId                            (FuncId (FuncId), name)
import           FuncTable                         (FuncTable,
                                                    Signature (Signature),
                                                    toMap)
import           Id                                (Id (Id))
import           Sigs                              (Sigs, chan, func, pro,
                                                    uniqueCombine)
import qualified Sigs                              (empty)
import           SortId                            (sortIdBool, sortIdInt,
                                                    sortIdRegex, sortIdString)
import           ChanId                 (ChanId)                 
import           StdTDefs                          (stdFuncTable, stdTDefs)
import           TxsDefs                           (TxsDefs, fromList, funcDefs, procDefs, modelDefs,
                                                    union, ProcDef, ProcId)
import qualified TxsDefs                           (empty)
import           ValExpr                           (ValExpr,
                                                    ValExprView (Vfunc, Vite),
                                                    cstrITE, cstrVar, view)
import           VarId                             (VarId)
import           SortId                             (SortId)
import           CstrId (CstrId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.ChanId
import           TorXakis.Compiler.Defs.Sigs
import           TorXakis.Compiler.Defs.TxsDefs
import           TorXakis.Compiler.Error           (Error)
import           TorXakis.Compiler.ValExpr.CstrId
import           TorXakis.Compiler.ValExpr.ExpDecl
import           TorXakis.Compiler.ValExpr.FuncDef
import           TorXakis.Compiler.ValExpr.FuncId
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.VarId
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Simplifiable
import           TorXakis.Compiler.Defs.ProcDef
import           TorXakis.Compiler.ValExpr.Common

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
        allSortsMap = Map.union pdsMap sMap
    chs <- Map.fromList <$> chanDeclsToChanIds allSortsMap (pd ^. chdecls)        
    cMap <- compileToCstrId allSortsMap (pd ^. adts)
    let --  e1 = e0 { cstrIdT = cMap }
        allFuncs = pd ^. funcs ++ pd ^. consts
    stdFuncIds <- getStdFuncIds
    cstrFuncIds <- adtsToFuncIds allSortsMap (pd ^. adts)
    -- Construct the variable declarations table.
    let -- Predefined functions:
        predefFuncs = funcDefInfoNamesMap $
            (fst <$> stdFuncIds) ++ (fst <$> cstrFuncIds)
        -- User defined functions:
        userDefFuncs = Map.fromListWith (++) $
                           zip (funcName <$> allFuncs)
                               (return . FDefLoc . getLoc <$> allFuncs)
        --  All the functions available at the top level. Note the union is
        -- left biased, so functions defined by the user will have precedence
        -- over predefined functions.
        nameToFDefs = Map.unionWith (++) predefFuncs userDefFuncs
        -- There are no variable declarations at the top level.
        eVdMap = Map.empty :: Map Text (Loc VarDeclE) 
    fRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& nameToFDefs) allFuncs
    pRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& nameToFDefs) (pd ^. procs)
    mRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& nameToFDefs :& chs) (pd ^. models)
    let dMap = fRtoDs `Map.union` pRtoDs `Map.union` mRtoDs
    -- Construct the function declaration to function id table.
    lFIdMap <- funcDeclsToFuncIds allSortsMap allFuncs
    -- Join `lFIdMap` and  `stdFuncIds`.
    let completeFidMap = Map.fromList $ --
            fmap (first FDefLoc) (Map.toList lFIdMap)
            ++ stdFuncIds
            ++ cstrFuncIds
    -- Infer the types of all variable declarations.
    let emptyVdMap = Map.empty :: Map (Loc VarDeclE) SortId
    -- We pass to 'inferTypes' an empty map from 'Loc VarDeclE' to 'SortId'
    -- since no variables can be declared at the top level.
    vdSortMap <- inferTypes (allSortsMap :& dMap :& completeFidMap :& emptyVdMap) allFuncs
    -- Construct the variable declarations to @VarId@'s lookup table.
    vMap <- generateVarIds (vdSortMap :& completeFidMap ) allFuncs
    lFDefMap <- funcDeclsToFuncDefs (vMap :& completeFidMap :& dMap) allFuncs
     -- Construct the @ProcId@ to @ProcDef@ map:
    pdefMap <- procDeclsToProcDefMap (allSortsMap :& cMap :& completeFidMap :& lFDefMap :& dMap) (pd ^. procs)
    -- Finally construct the TxsDefs.
    let mm = allSortsMap :& pdefMap :& chs :& cMap :& completeFidMap :& lFDefMap
    sigs    <- toSigs                mm pd
    txsDefs <- toTxsDefs (func sigs) (mm :& dMap :& vMap :& vdSortMap) pd
    St i    <- get
    return (Id i, txsDefs, sigs)

toTxsDefs :: ( MapsTo Text        SortId mm
             , MapsTo (Loc CstrE) CstrId mm
             , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
             , MapsTo FuncDefInfo FuncId mm
             , MapsTo FuncId (FuncDef VarId) mm 
             , MapsTo ProcId ProcDef mm
             , MapsTo Text ChanId mm
             , MapsTo (Loc VarDeclE) VarId mm
             , MapsTo (Loc VarDeclE) SortId mm
             , In (ProcId, ()) (Contents mm) ~ 'False )
          => FuncTable VarId -> mm -> ParsedDefs -> CompilerM TxsDefs
toTxsDefs ft mm pd = do
    ads <- adtsToTxsDefs mm (pd ^. adts)
    -- Get the function id's of all the constants.
    cfIds <- traverse (findFuncIdForDeclM mm) (pd ^.. consts . traverse . loc')
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
          , MapsTo FuncDefInfo FuncId mm
          , MapsTo FuncId (FuncDef VarId) mm
          , MapsTo ProcId ProcDef mm
          , MapsTo Text ChanId mm)
       => mm -> ParsedDefs -> CompilerM (Sigs VarId)
toSigs mm pd = do
    let ts   = sortsToSigs (innerMap mm)
    as  <- adtDeclsToSigs mm (pd ^. adts)
    fs  <- funDeclsToSigs mm (pd ^. funcs)
    cs  <- funDeclsToSigs mm (pd ^. consts)
    let pidMap :: Map ProcId ProcDef
        pidMap = innerMap mm
        ss = Sigs.empty { func = stdFuncTable
                        , chan = Map.elems (innerMap mm :: Map Text ChanId)
                        , pro  = Map.keys pidMap
                        }
    return $ ts `uniqueCombine` as
        `uniqueCombine` fs
        `uniqueCombine` cs
        `uniqueCombine` ss

funcDefInfoNamesMap :: [FuncDefInfo] -> Map Text [FuncDefInfo]
funcDefInfoNamesMap fdis =
    groupByName $ catMaybes $ asPair <$> fdis
    where
      asPair :: FuncDefInfo -> Maybe (Text, FuncDefInfo)
      asPair fdi = (, fdi) <$> fdiName fdi
      groupByName :: [(Text, FuncDefInfo)] -> Map Text [FuncDefInfo]
      groupByName = Map.fromListWith (++) . fmap (second pure)
