{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Defs.TxsDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation functions related to 'TorXakis' definitions.
--------------------------------------------------------------------------------
module TorXakis.Compiler.Defs.TxsDefs
    ( adtsToTxsDefs
    , cnectDeclsToTxsDefs
    , mapperDeclsToTxsDefs
    , modelDeclsToTxsDefs
    , purpDeclsToTxsDefs
    )
where

import           Control.Monad                      (unless, when)
import           Control.Monad.Except               (liftEither, throwError)
import           Data.List.Unique                   (repeated)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Map.Merge.Strict              (mergeA, traverseMissing,
                                                     zipWithAMatched)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T

import           BehExprDefs                        (ChanOffer (Exclam, Quest),
                                                     Offer (Offer), chanIdHit,
                                                     chanIdMiss, chanIdQstep)
import           ChanId                             (ChanId)
import           CnectId                            (CnectId (CnectId))
import           CstrId                             (CstrId)
import           FuncTable                          (Handler, Signature)
import           Id                                 (Id (Id))
import           ProcId                             (ExitSort (NoExit), ProcId)
import           PurpId                             (PurpId (PurpId))
import           SortDef                            (SortDef (SortDef))
import           SortId                             (SortId, sortIdString)
import           TxsDefs                            (CnectDef (CnectDef), ConnDef (ConnDfroW, ConnDtoW),
                                                     GoalId (GoalId),
                                                     MapperDef (MapperDef),
                                                     MapperId (MapperId),
                                                     ModelDef,
                                                     ModelId (ModelId), ProcDef,
                                                     PurpDef (PurpDef), TxsDefs,
                                                     VExpr, cstrDefs, empty,
                                                     sortDefs)
import qualified TxsDefs
import           VarId                              (VarId)

import           TorXakis.Compiler.Data             (CompilerM, getNextId)
import           TorXakis.Compiler.Defs.BehExprDefs (toBExpr, toOffer)
import           TorXakis.Compiler.Defs.ModelDef    (chRefsToChIdSet,
                                                     modelDeclToModelDef)
import           TorXakis.Compiler.Error            (Error (Error), ErrorType (InvalidExpression, TypeMismatch),
                                                     getErrorLoc, _errorLoc, 
                                                     ErrorLoc(NoErrorLoc), Entity(Model),
                                                     _errorMsg, _errorType)
import           TorXakis.Compiler.Maps             (dropHandler, lookupChId,
                                                     usedChIds, (.@@))
import           TorXakis.Compiler.Maps.DefinesAMap (getMap)
import           TorXakis.Compiler.Maps.VarRef      (varDefsFromExp)
import           TorXakis.Compiler.MapsTo           ((:&) ((:&)), Contents, In,
                                                     MapsTo, innerMap, keys,
                                                     values, (<.+>))
import           TorXakis.Compiler.Validation       (checkUnique)
import           TorXakis.Compiler.ValExpr.CstrDef  (compileToCstrDefs)
import           TorXakis.Compiler.ValExpr.SortId   (exitSort, inferVarTypes)
import           TorXakis.Compiler.ValExpr.ValExpr  (expDeclToValExpr)
import           TorXakis.Compiler.ValExpr.VarId    (mkVarIds)
import           TorXakis.Parser.Data               (ADTDecl, ChanDeclE, ChanOfferDecl (ExclD, QuestD),
                                                     ChanRefE, CnectDecl,
                                                     CnectItem,
                                                     CnectItemType (ChanIn, ChanOut),
                                                     CnectType (CTClient, CTServer),
                                                     CodecItem (CodecItem),
                                                     CodecType (Decode, Encode),
                                                     CstrE, FuncDeclE, Loc,
                                                     MapperDecl, ModelDecl,
                                                     PurpDecl, VarDeclE,
                                                     VarRefE, chanRefName,
                                                     chanRefOfOfferDecl,
                                                     cnectCh,
                                                     cnectDeclCnectItems,
                                                     cnectDeclCodecs,
                                                     cnectDeclName,
                                                     cnectDeclType, cnectType,
                                                     codecOffer, codecType,
                                                     getLoc, host, mapperBExp,
                                                     mapperIns, mapperName,
                                                     mapperOuts, mapperSyncs,
                                                     modelName, port,
                                                     purpDeclGoals, purpDeclIns,
                                                     purpDeclName, purpDeclOuts,
                                                     purpDeclSyncs,
                                                     testGoalDeclBExp,
                                                     testGoalDeclName)

-- | Compile a list of ADT declarations into @TxsDefs@.
adtsToTxsDefs :: ( MapsTo Text SortId mm
                 , MapsTo (Loc CstrE) CstrId mm)
              => mm -> [ADTDecl] -> CompilerM TxsDefs
adtsToTxsDefs mm ds = do
    lCstrDefs <- compileToCstrDefs mm ds
    return $ empty
        { sortDefs = envToSortDefs mm
        , cstrDefs = lCstrDefs
        }

-- | Extract the @SortDef@'s from the composite map.
envToSortDefs :: ( MapsTo Text SortId mm )
              => mm -> Map SortId SortDef
envToSortDefs mm = Map.fromList $
    zip (values @Text mm) (repeat SortDef)

-- | Compile a list of model declarations into a map from model id's to model
-- definitions.
modelDeclsToTxsDefs :: ( MapsTo Text SortId mm
                       , MapsTo Text (Loc ChanDeclE) mm
                       , MapsTo (Loc ChanDeclE) ChanId mm
                       , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                       , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm
                       , MapsTo ProcId ProcDef mm
                       , MapsTo (Loc VarDeclE) SortId mm
                       , MapsTo (Loc VarDeclE) VarId mm
                       , In (Loc FuncDeclE, Signature) (Contents mm) ~ 'False
                       , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False
                       , In (ProcId, ()) (Contents mm) ~ 'False )
                    => mm -> [ModelDecl] -> CompilerM (Map ModelId ModelDef)
modelDeclsToTxsDefs mm mds = do
    checkUnique (NoErrorLoc, Model, "Model") (map modelName mds) 
    Map.fromList <$> (zip <$> traverse modelDeclToModelId  mds
                          <*> traverse (modelDeclToModelDef mm') mds)
    where
      procIdsOnly :: Map ProcId ()
      procIdsOnly = Map.fromList $ zip (keys @ProcId @ProcDef mm) (repeat ())
      mm' = mm :& procIdsOnly

-- | Compile a list of purpose declarations into a map from purpose id's to
-- purpose definitions.
purpDeclsToTxsDefs :: ( MapsTo Text SortId mm
                      , MapsTo Text (Loc ChanDeclE) mm
                      , MapsTo (Loc ChanDeclE) ChanId mm
                      , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                      , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm
                      , MapsTo ProcId ProcDef mm
                      , MapsTo (Loc VarDeclE) SortId mm
                      , MapsTo (Loc VarDeclE) VarId mm
                      , In (ProcId, ()) (Contents mm) ~ 'False
                      , In (Loc FuncDeclE, Signature) (Contents mm) ~ 'False
                      , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False )
                      =>  mm -> [PurpDecl] -> CompilerM (Map PurpId PurpDef)
purpDeclsToTxsDefs mm pds =
    Map.fromList <$> (zip <$> traverse purpDeclToPurpId pds
                          <*> traverse purpDeclToPurpDef pds)

    where
      procIdsOnly :: Map ProcId ()
      procIdsOnly = Map.fromList $ zip (keys @ProcId @ProcDef mm) (repeat ())

      purpDeclToPurpId :: PurpDecl -> CompilerM PurpId
      purpDeclToPurpId pd = PurpId (purpDeclName pd) . Id <$> getNextId

      purpDeclToPurpDef :: PurpDecl -> CompilerM PurpDef
      purpDeclToPurpDef pd = do
          -- Map the channel references to the places in which they are declared.
          chDecls <- getMap mm pd :: CompilerM (Map (Loc ChanRefE) (Loc ChanDeclE))
          -- Add the channel declaration introduced by the hide operator.
          purpChIds <- getMap mm pd :: CompilerM (Map (Loc ChanDeclE) ChanId)
          let mm' = chDecls :& (purpChIds <.+> mm) :& procIdsOnly
          syncs <- maybe (return (usedChIds mm'))
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
          let fss = dropHandler (innerMap mm)
          bTypes <- Map.fromList <$> inferVarTypes (fss :& mm') (purpDeclGoals pd)
          bvIds  <- Map.fromList <$> mkVarIds bTypes (purpDeclGoals pd)
          let mm'' = bTypes <.+> (bvIds <.+> mm')
          evds <- liftEither $ varDefsFromExp mm'' pd
          let compileTestGoalDecl gd = do
                  gId <- getNextId
                  be  <- toBExpr mm'' evds (testGoalDeclBExp gd)
                  return (GoalId (testGoalDeclName gd) (Id gId), be)
          gls <- traverse compileTestGoalDecl (purpDeclGoals pd)
          return $ PurpDef insyncs outsyncs splsyncs gls

-- | Compile a list of connect declarations into a map from connect id's to
-- connect definitions.
cnectDeclsToTxsDefs :: ( MapsTo Text SortId mm
                       , MapsTo Text (Loc ChanDeclE) mm
                       , MapsTo (Loc ChanDeclE) ChanId mm
                       , MapsTo (Loc VarDeclE) SortId mm
                       , MapsTo (Loc VarDeclE) VarId mm
                       , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm
                       , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                       , In (Loc FuncDeclE, Signature) (Contents mm) ~ 'False
                       , In (ProcId, ()) (Contents mm) ~ 'False
                       , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False )
                    => mm -> [CnectDecl] -> CompilerM (Map CnectId CnectDef)
cnectDeclsToTxsDefs mm cds =
    Map.fromList <$> (zip <$> traverse cnectDeclToCnectId cds
                          <*> traverse cnectDeclToCnectDef cds)
    where
      cnectDeclToCnectId :: CnectDecl -> CompilerM CnectId
      cnectDeclToCnectId cd = CnectId (cnectDeclName cd) . Id <$> getNextId

      cnectDeclToCnectDef :: CnectDecl -> CompilerM CnectDef
      cnectDeclToCnectDef cd = do
          chDecls <- getMap mm cd :: CompilerM (Map (Loc ChanRefE) (Loc ChanDeclE))
          let
              mm' = chDecls :& mm
              toConnDef :: (Text, Integer, CodecItem) -> CompilerM ConnDef
              toConnDef (h, p, ci@(CodecItem offr chOffr@(QuestD iv) Decode)) = do
                  -- We know the type of 'iv' must be a string.
                  let ivSortMap = Map.singleton (getLoc iv) sortIdString
                  vIdMap   <- Map.fromList <$> mkVarIds ivSortMap chOffr
                  vId <- vIdMap .@@ getLoc iv
                  -- NOTE: in the future smart constructors should assert that
                  -- offr does not contain question marks, otherwise the user
                  -- might get a different error (for instance because a
                  -- variable might not be declared).
                  let mm'' = ivSortMap <.+> (vIdMap <.+> mm')
                  civds <- liftEither $ varDefsFromExp mm'' ci
                  Offer chId chOffrs <- toOffer mm'' civds offr
                  vExps <- traverse exclamExp chOffrs
                  return $ ConnDfroW chId h p vId vExps
              toConnDef (_, _, CodecItem _ (ExclD _) Decode) =
                  throwError Error
                  { _errorType = InvalidExpression
                  , _errorLoc  = getErrorLoc cd -- NOTE: we could add a location to CodecItem to be more precise.
                  , _errorMsg = "DECODE domain shall be one '?' of String\n"
                  }
              toConnDef(h, p, ci@(CodecItem offr (ExclD e) Encode)) = do
                  let
                      -- We don't need the proc ids' to infer the variable types of the offer.
                      -- NOTE: 'HasTypedVars' could be replaced by 'DefinesAMap'.
                      emptyProcIds :: Map ProcId ()
                      emptyProcIds = Map.empty
                      fss = dropHandler (innerMap mm)
                  offrSIdMap <- Map.fromList <$> inferVarTypes (fss :& emptyProcIds :& mm') offr
                  offrVIdMap <- Map.fromList <$> mkVarIds offrSIdMap offr
                  let mm'' = offrSIdMap <.+> (offrVIdMap <.+> mm')
                  civds <- liftEither $ varDefsFromExp mm'' ci
                  Offer chId chOffrs <- toOffer mm'' civds offr
                  vIds <- traverse questVIds chOffrs
                  vExp <- liftEither $ expDeclToValExpr civds sortIdString e
                  return $ ConnDtoW chId h p vIds vExp
              toConnDef (_, _, CodecItem _ (QuestD _) Encode) =
                  throwError Error
                  { _errorType = InvalidExpression
                  , _errorLoc  = getErrorLoc cd
                  , _errorMsg = "ENCODE domain shall be one '!' of String\n"
                  }

              exclamExp :: ChanOffer -> CompilerM VExpr
              exclamExp (Exclam vExp) = return vExp
              exclamExp (Quest _) = throwError Error
                  { _errorType = InvalidExpression
                  , _errorLoc = getErrorLoc cd
                  , _errorMsg = "No '?' offer allowed here."
                  }

              questVIds :: ChanOffer -> CompilerM VarId
              questVIds (Quest vId) = return vId
              questVIds (Exclam _)  = throwError Error
                  { _errorType = InvalidExpression
                  , _errorLoc = getErrorLoc cd
                  , _errorMsg = "No '!' offer allowed here."
                  }

              asCnectType :: CnectType -> TxsDefs.CnectType
              asCnectType CTClient = TxsDefs.ClientSocket
              asCnectType CTServer = TxsDefs.ServerSocket

              -- | Check that the (host, port) mapping is unique across all
              -- @CnectItem@s.
              checkUniqueHostPorts :: [CnectItem] -> CompilerM ()
              checkUniqueHostPorts cs =
                  unless (null $ repeated $ zip3 (cnectType <$> cs) (host <$> cs) (port <$> cs)) $
                      throwError Error
                      { _errorType = InvalidExpression
                      , _errorLoc = getErrorLoc cd
                      , _errorMsg = "HOST-PORT pairs are not unique"
                      }

              -- | Check that the channels used in the @CnetItem@s are unique.
              checkUniqueChannels :: [CnectItem] -> CompilerM ()
              checkUniqueChannels cs =
                  unless (null $ repeated $ cnectCh <$> cs) $
                      throwError Error
                      { _errorType = InvalidExpression
                      , _errorLoc = getErrorLoc cd
                      , _errorMsg = "Channels in the connections are not unique"
                      }

              cnectItems = cnectDeclCnectItems cd
              cnectCodecs = cnectDeclCodecs cd

          checkUniqueHostPorts cnectItems
          checkUniqueChannels  cnectItems
          let
              crToCnectItem :: Map Text CnectItem
              crToCnectItem = Map.fromList $
                  zip (chanRefName . cnectCh <$> cnectItems) cnectItems

              crToCodecItem :: Map Text CodecItem
              crToCodecItem = Map.fromList $
                  zip (chanRefName . chanRefOfOfferDecl . codecOffer <$> cnectCodecs) cnectCodecs

              errorOnMissingCodec :: Text -> CnectItem -> CompilerM (Text, Integer, CodecItem)
              errorOnMissingCodec _ cnect = throwError Error
                    { _errorType = InvalidExpression
                    , _errorLoc = getErrorLoc cd
                    , _errorMsg = "Missing codec for " <> T.pack (show cnect)
                    }

              errorOnMissingCnect :: Text ->  CodecItem -> CompilerM (Text, Integer, CodecItem)
              errorOnMissingCnect _ codec = throwError Error
                    { _errorType = InvalidExpression
                    , _errorLoc = getErrorLoc cd
                    , _errorMsg = "Missing connect for " <> T.pack (show codec)
                    }

              mergeCnectCodec :: Text  -> CnectItem ->  CodecItem -> CompilerM (Text, Integer, CodecItem)
              mergeCnectCodec _ cnect codec
                  | cnectType cnect == ChanIn && codecType codec == Decode =
                        return (host cnect, port cnect, codec)
                  | cnectType cnect == ChanOut && codecType codec == Encode =
                        return (host cnect, port cnect, codec)
                  | otherwise = throwError Error
                    { _errorType = InvalidExpression
                    , _errorLoc = getErrorLoc cd
                    , _errorMsg = "'CHAN IN' must correspond to 'DECODE'"
                                  <> ", and  'CHAN OUT must correspond to 'ENCODE'"
                    }


          -- Merge the @CnectItem@s and the @CodecItem@s based on the channel
          -- references.
          hostPortCnects <- mergeA
              (traverseMissing errorOnMissingCodec)
              (traverseMissing errorOnMissingCnect)
              (zipWithAMatched mergeCnectCodec)
              crToCnectItem
              crToCodecItem
          connDefs <- traverse toConnDef hostPortCnects
          return $ CnectDef (asCnectType $ cnectDeclType cd) (Map.elems connDefs)

-- | Compile a list of mapper declarations into a map from mapper id's to
-- mapper definitions.
mapperDeclsToTxsDefs :: ( MapsTo Text SortId mm
                        , MapsTo Text (Loc ChanDeclE) mm
                        , MapsTo (Loc ChanDeclE) ChanId mm
                        , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                        , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm
                        , MapsTo ProcId ProcDef mm
                        , MapsTo (Loc VarDeclE) SortId mm
                        , MapsTo (Loc VarDeclE) VarId mm
                        , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False
                        , In (Loc FuncDeclE, Signature) (Contents mm) ~ 'False
                        , In (ProcId, ()) (Contents mm) ~ 'False )
                     => mm -> [MapperDecl] -> CompilerM (Map MapperId MapperDef)
mapperDeclsToTxsDefs mm mds =
    Map.fromList <$> (zip <$> traverse mapperDeclToMapperId mds
                          <*> traverse mapperDeclToMapperDef mds)
    where
      mapperDeclToMapperId :: MapperDecl -> CompilerM MapperId
      mapperDeclToMapperId md = MapperId (mapperName md) . Id <$> getNextId

      mapperDeclToMapperDef :: MapperDecl -> CompilerM MapperDef
      mapperDeclToMapperDef md = do
          chDecls  <- getMap mm md :: CompilerM (Map (Loc ChanRefE) (Loc ChanDeclE))
          mprChIds <- getMap mm md :: CompilerM (Map (Loc ChanDeclE) ChanId)
          let mm' = chDecls :& (mprChIds <.+> mm) :& procIdsOnly

          let fss = dropHandler (innerMap mm)
          bTypes <- Map.fromList <$> inferVarTypes (fss :& mm') (mapperBExp md)
          bvIds  <- Map.fromList <$> mkVarIds bTypes (mapperBExp md)
          let mm'' = bTypes <.+> (bvIds <.+> mm')

          evds <- liftEither $ varDefsFromExp mm'' md
          be   <- toBExpr mm'' evds (mapperBExp md)

          ins   <- traverse (lookupChId mm') (getLoc <$> mapperIns md)
          outs  <- traverse (lookupChId mm') (getLoc <$> mapperOuts md)
          syncs <- maybe (return (usedChIds mm'))
                         (traverse (chRefsToChIdSet mm'))
                         (mapperSyncs md)

          eSort <- exitSort (fss :& mm'') (mapperBExp md)
          when (eSort /= NoExit)
              (throwError Error
                  { _errorType = TypeMismatch
                  , _errorLoc = getErrorLoc md
                  , _errorMsg = "Exit sort of a mapper must be 'NoExit'."
                  }
              )

          return $ MapperDef ins outs syncs be

      procIdsOnly :: Map ProcId ()
      procIdsOnly = Map.fromList $ zip (keys @ProcId @ProcDef mm) (repeat ())

-- | Create a model id from a model declaration.
modelDeclToModelId :: ModelDecl -> CompilerM ModelId
modelDeclToModelId md = ModelId (modelName md) . Id <$>  getNextId
