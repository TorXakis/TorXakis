{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
module TorXakis.Compiler.Defs.TxsDefs where

import           Data.List                          (nub, sortBy)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Ord                           (compare)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)

import           ChanId                             (ChanId, name, unid)
import           CstrId                             (CstrId)
import           FuncDef                            (FuncDef)
import           FuncId                             (FuncId)
import           Id                                 (Id (Id))
import           ProcId                             (ProcId)
import           PurpId                             (PurpId (PurpId))
import           SortDef                            (SortDef (SortDef))
import           SortId                             (SortId)
import           StdTDefs                           (chanIdHit, chanIdMiss,
                                                     chanIdQstep)
import           TxsDefs                            (ModelDef, ModelId, ProcDef,
                                                     PurpDef (PurpDef), TxsDefs,
                                                     cstrDefs, empty, modelDefs,
                                                     sortDefs)
import           VarId                              (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.ChanId
import           TorXakis.Compiler.Defs.ModelDef
import           TorXakis.Compiler.Defs.ModelId
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Maps.DefinesAMap
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.CstrDef
import           TorXakis.Parser.Data

adtsToTxsDefs :: ( MapsTo Text SortId mm
                 , MapsTo (Loc CstrE) CstrId mm)
              => mm -> [ADTDecl] -> CompilerM TxsDefs
adtsToTxsDefs mm ds = do
    lCstrDefs <- compileToCstrDefs mm ds
    return $ empty
        { sortDefs = envToSortDefs mm
        , cstrDefs = lCstrDefs
        }

envToSortDefs :: ( MapsTo Text SortId mm )
              => mm -> Map SortId SortDef
envToSortDefs mm = Map.fromList $
    zip (values @Text mm) (repeat SortDef)

modelDeclsToTxsDefs :: ( MapsTo Text SortId mm
                       , MapsTo Text (Loc ChanDeclE) mm
                       -- , MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
                       , MapsTo (Loc ChanDeclE) ChanId mm
                       , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [(Loc FuncDeclE)]) mm
                       , MapsTo (Loc FuncDeclE) FuncId mm
                       , MapsTo FuncId (FuncDef VarId) mm
                       , MapsTo ProcId ProcDef mm
                       , MapsTo (Loc VarDeclE) SortId mm
                       , MapsTo (Loc VarDeclE) VarId mm
                       -- , In (Loc ChanDeclE, ChanId) (Contents mm) ~ 'False
                       , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False
                       , In (ProcId, ()) (Contents mm) ~ 'False )
                    => mm -> [ModelDecl] -> CompilerM (Map ModelId ModelDef)
modelDeclsToTxsDefs mm mds =
    Map.fromList <$> (zip <$> traverse modelDeclToModelId  mds
                          <*> traverse (modelDeclToModelDef mm') mds)
    where
      procIdsOnly :: Map ProcId ()
      procIdsOnly = Map.fromList $ zip (keys @ProcId @ProcDef mm) (repeat ())
      mm' = mm :& procIdsOnly

purpDeclsToTxsDefs :: ( MapsTo Text SortId mm
                      , MapsTo Text (Loc ChanDeclE) mm
                      , MapsTo (Loc ChanDeclE) ChanId mm
                      , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                      , MapsTo (Loc FuncDeclE) FuncId mm
                      , MapsTo FuncId (FuncDef VarId) mm
                      , MapsTo ProcId ProcDef mm
                      , MapsTo (Loc VarDeclE) SortId mm
                      , MapsTo (Loc VarDeclE) VarId mm
                      , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False )
                      =>  mm -> [PurpDecl] -> CompilerM (Map PurpId PurpDef)
purpDeclsToTxsDefs mm pps =
    Map.fromList <$> (zip <$> traverse purpDeclToPurpId pps
                          <*> traverse purpDeclToPurpDef pps)

    where
      procIdsOnly :: Map ProcId ()
      procIdsOnly = Map.fromList $ zip (keys @ProcId @ProcDef mm) (repeat ())

      purpDeclToPurpId :: PurpDecl -> CompilerM PurpId
      purpDeclToPurpId pd = do
          pId <- getNextId
          return $ PurpId (purpDeclName pd) (Id pId)

      purpDeclToPurpDef :: PurpDecl -> CompilerM PurpDef
      purpDeclToPurpDef pd = do
          -- Map the channel references to the places in which they are declared.
          chDecls <- getMap mm pd :: CompilerM (Map (Loc ChanRefE) (Loc ChanDeclE))
          -- Add the channel declaration introduced by the hide operator.
          purpChIds <- getMap mm pd :: CompilerM (Map (Loc ChanDeclE) ChanId)
          let mm' = chDecls :& (purpChIds <.+> mm) :& procIdsOnly
          -- TODO: reduce duplication w.r.t 'ModelDef.modelDeclToModelDef' when
          -- determining 'insyncs', 'outsyncs', and 'splsyncs'.
          let
              usedChIds :: [Set ChanId]
              usedChIds = fmap Set.singleton (sortByUnid . nub . Map.elems $ usedChIdMap mm')

              sortByUnid :: [ChanId] -> [ChanId]
              sortByUnid = sortBy cmpChUnid
                  where
                    cmpChUnid c0 c1 = unid c0 `compare` unid c1
          syncs <- maybe (return usedChIds)
                         (traverse (chRefsToChIdSet mm'))
                         (purpDeclSyncs pd)
          ins  <- Set.fromList <$> traverse (lookupChId mm') (getLoc <$> purpDeclIns pd)
          outs <- Set.fromList <$> traverse (lookupChId mm') (getLoc <$> purpDeclOuts pd)
          let
              insyncs  = filter (`Set.isSubsetOf` ins) syncs
              outsyncs = filter (`Set.isSubsetOf` outs) syncs
          let splsyncs = [ Set.singleton chanIdQstep
                         , Set.singleton chanIdHit
                         , Set.singleton chanIdMiss
                         ]
          gls <- undefined pd mm'
          return $ PurpDef insyncs outsyncs splsyncs gls
