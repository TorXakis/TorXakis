{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module TorXakis.Compiler.Defs.TxsDefs where

import           Control.Monad.Error.Class          (throwError)
import           Data.List                          (nub, sortBy)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Ord                           (compare)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T

import           BehExprDefs                        (BExpr,
                                                     ChanOffer (Exclam, Quest),
                                                     Offer (Offer))
import           ChanId                             (ChanId, name, unid)
import           CnectId                            (CnectId (CnectId))
import           ConstDefs                          (Const (Cstring))
import           CstrId                             (CstrId)
import           FuncDef                            (FuncDef)
import           FuncId                             (FuncId)
import           Id                                 (Id (Id))
import           ProcId                             (ProcId)
import           PurpId                             (PurpId (PurpId))
import           SortDef                            (SortDef (SortDef))
import           SortId                             (SortId, sortIdString)
import           StdTDefs                           (chanIdHit, chanIdMiss,
                                                     chanIdQstep)
import           TxsDefs                            (CnectDef (CnectDef), ConnDef (ConnDfroW, ConnDtoW),
                                                     GoalId (GoalId), ModelDef,
                                                     ModelId, ProcDef,
                                                     PurpDef (PurpDef), TxsDefs,
                                                     VExpr, cstrDefs, empty,
                                                     modelDefs, sortDefs)
import qualified TxsDefs
import           ValExpr                            (cstrConst)
import           VarId                              (VarId (VarId))

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.BehExprDefs
import           TorXakis.Compiler.Defs.ChanId
import           TorXakis.Compiler.Defs.ModelDef
import           TorXakis.Compiler.Defs.ModelId
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Maps.DefinesAMap
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.CstrDef
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.VarId
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
                       , MapsTo (Loc ChanDeclE) ChanId mm
                       , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [(Loc FuncDeclE)]) mm
                       , MapsTo (Loc FuncDeclE) FuncId mm
                       , MapsTo FuncId (FuncDef VarId) mm
                       , MapsTo ProcId ProcDef mm
                       , MapsTo (Loc VarDeclE) SortId mm
                       , MapsTo (Loc VarDeclE) VarId mm
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
                      , In (ProcId, ()) (Contents mm) ~ 'False
                      , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False )
                      =>  mm -> [PurpDecl] -> CompilerM (Map PurpId PurpDef)
purpDeclsToTxsDefs mm pds =
    Map.fromList <$> (zip <$> traverse purpDeclToPurpId pds
                          <*> traverse purpDeclToPurpDef pds)

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
          -- Compile the goals
          bTypes <- Map.fromList <$> inferVarTypes mm' (purpDeclGoals pd)
          bvIds  <- Map.fromList <$> mkVarIds bTypes (purpDeclGoals pd)
          let mm'' = bTypes <.+> (bvIds <.+> mm')
              compileTestGoalDecl gd = do
                  gId <- getNextId
                  be  <- toBExpr mm'' (testGoalDeclBExp gd)
                  return (GoalId (testGoalDeclName gd) (Id gId), be)
          gls <- traverse compileTestGoalDecl (purpDeclGoals pd)
          return $ PurpDef insyncs outsyncs splsyncs gls

cnectDeclsToTxsDefs :: ( MapsTo Text SortId mm
                       , MapsTo Text (Loc ChanDeclE) mm
                       , MapsTo (Loc ChanDeclE) ChanId mm
                       , MapsTo (Loc VarDeclE) SortId mm
                       , MapsTo (Loc VarDeclE) VarId mm
                       , MapsTo (Loc FuncDeclE) FuncId mm
                       , MapsTo FuncId (FuncDef VarId) mm
                       , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                       , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False )
                    => mm -> [CnectDecl] -> CompilerM (Map CnectId CnectDef)
cnectDeclsToTxsDefs mm cds =
    Map.fromList <$> (zip <$> traverse cnectDeclToCnectId cds
                          <*> traverse cnectDeclToCnectDef cds)
    where
      cnectDeclToCnectId :: CnectDecl -> CompilerM CnectId
      cnectDeclToCnectId cd = do
          cId <- getNextId
          return $ CnectId (cnectDeclName cd) (Id cId)

      cnectDeclToCnectDef :: CnectDecl -> CompilerM CnectDef
      cnectDeclToCnectDef cd = do
          chDecls <- getMap mm cd :: CompilerM (Map (Loc ChanRefE) (Loc ChanDeclE))
          let
              mm' = chDecls :& mm

              cnectItemToConnDef :: CnectItem -> CompilerM ConnDef
              cnectItemToConnDef (CnectItem cr ChanIn h p) = do
                  chId <- lookupChId mm' (getLoc cr)
                  return $ ConnDfroW chId h p (VarId "" (-1) sortIdString) []

              cnectItemToConnDef (CnectItem cr ChanOut h p) = do
                  chId <- lookupChId mm' (getLoc cr)
                  return $ ConnDtoW chId h p [] (cstrConst (Cstring ""))

              cnectCodecToConnDef :: CodecItem -> CompilerM ConnDef
              cnectCodecToConnDef (CodecItem offr chOffr@(QuestD iv) Decode) = do
                  -- We know the type of 'iv' must be a string.
                  let ivSortMap = Map.singleton (getLoc iv) sortIdString
                  vIdMap   <- Map.fromList <$> mkVarIds ivSortMap chOffr
                  vId <- vIdMap .@ getLoc iv
                  -- TODO: we should assert that offr does not contain question
                  -- marks, otherwise the user might get a different error (for
                  -- instance because a variable might not be declared).
                  Offer chId chOffrs <- toOffer (ivSortMap <.+> (vIdMap <.+> mm')) offr
                  vExps <- traverse exclamExp chOffrs
                  return $ ConnDfroW chId "" (-1) vId vExps
              cnectCodecToConnDef (CodecItem _ (ExclD _) Decode) =
                  throwError Error
                  { _errorType = InvalidExpression
                  , _errorLoc  = getErrorLoc cd -- TODO: add a location to CodecItem to be more precise.
                  , _errorMsg = "DECODE domain shall be one of '?' of String\n"
                  }

              exclamExp :: ChanOffer -> CompilerM VExpr
              exclamExp = undefined

              asCnectType :: CnectType -> TxsDefs.CnectType
              asCnectType CTClient = TxsDefs.ClientSocket
              asCnectType CTServer = TxsDefs.ServerSocket

          cds0    <- traverse cnectItemToConnDef (cnectDeclCnectItems cd)
          cds1    <- traverse cnectCodecToConnDef (cnectDeclCodecs cd)
          return $ CnectDef (asCnectType $ cnectDeclType cd) (cds0 ++ cds1)

