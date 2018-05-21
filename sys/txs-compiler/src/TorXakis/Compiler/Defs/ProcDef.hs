{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module TorXakis.Compiler.Defs.ProcDef where

import           Control.Arrow                      (second)
import           Control.Monad.Error.Class          (liftEither, throwError)
import           Data.List                          (find)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text)
import qualified Data.Text                          as T

import qualified BehExprDefs                        as Beh
import           ChanId                             (ChanId)
import           FuncDef                            (FuncDef)
import           FuncId                             (FuncId)
import           Id                                 (Id (Id))
import           SortId                             (SortId)
import qualified SortId
import           StatId                             (StatId (StatId))
import qualified StatId
import           TxsDefs                            (ExitSort (..),
                                                     ProcDef (ProcDef),
                                                     ProcId (ProcId), VEnv)
import           ValExpr                            (ValExpr)
import           VarId                              (VarId, varsort)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.BehExprDefs
import           TorXakis.Compiler.Defs.ChanId
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Maps.DefinesAMap
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.Common
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.ValExpr
import           TorXakis.Compiler.ValExpr.VarId
import           TorXakis.Parser.Data

import           TorXakis.Compiler.Data.ProcDecl
import           TorXakis.Compiler.Data.VarDecl

procDeclsToProcDefMap :: ( MapsTo Text SortId mm
                         , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                         , MapsTo (Loc FuncDeclE) FuncId mm
                         , MapsTo FuncId (FuncDef VarId) mm
                         , MapsTo (Loc ProcDeclE) ProcInfo mm
                         , In (Loc VarDeclE, VarId) (Contents mm) ~ 'False
                         , In (Text, ChanId) (Contents mm) ~ 'False
                         , In (Loc ChanDeclE, ChanId) (Contents mm) ~ 'False
                         , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False
                         , In (ProcId, ()) (Contents mm) ~ 'False
                         , In (Loc VarDeclE, SortId) (Contents mm) ~ 'False )
                      => mm
                      -> [ProcDecl]
                      -> CompilerM (Map ProcId ProcDef)
procDeclsToProcDefMap mm ps = do
    let pms = innerMap mm
    gProcDeclsToProcDefMap pms emptyMpd ps
    where
      emptyMpd = Map.empty :: Map ProcId ProcDef
      gProcDeclsToProcDefMap :: Map (Loc ProcDeclE) ProcInfo
                             -> Map ProcId ProcDef
                             -> [ProcDecl]
                             -> CompilerM (Map ProcId ProcDef)
      gProcDeclsToProcDefMap pms mpd rs = do
          (ls, rs') <- traverseCatch (mkpIdPDefM pms) rs
          case (ls, rs') of
              ([], _) -> return $ Map.fromList rs' <> innerMap mpd
              (_, []) -> throwError Error
                  { _errorType = ProcessNotDefined
                  , _errorLoc = NoErrorLoc
                  , _errorMsg = T.pack (show (snd <$> ls))
                  }
              (_, _ ) -> gProcDeclsToProcDefMap pms (Map.fromList rs' <.+> mpd) (fst <$> ls)

      mkpIdPDefM :: Map (Loc ProcDeclE) ProcInfo
                 -> ProcDecl
                 -> CompilerM (ProcId, ProcDef)
      mkpIdPDefM pms pd = do
          ProcInfo pId chIds pvIds <- pms .@ getLoc pd :: CompilerM ProcInfo
          -- Scan for channel references and declarations
          chDecls <- getMap () pd :: CompilerM (Map (Loc ChanRefE) (Loc ChanDeclE))
          let chIdsM = Map.fromList chIds
          let body = procDeclBody pd
              mpd' = Map.fromList $ zip (allProcIds pms) (repeat ())
          bTypes  <- Map.fromList <$>
              inferVarTypes (  Map.fromList pvIds
                            :& mm
                            :& Map.fromList (fmap (second varsort) pvIds)
                            :& mpd'
                            :& chDecls
                            :& chIdsM ) body
          bvIds   <- mkVarIds bTypes body
          b       <- toBExpr (  bTypes
                             :& Map.fromList (pvIds ++ bvIds)
                             :& mm
                             :& mpd'
                             :& chDecls
                             :& chIdsM
                             ) body
          -- NOTE that it is crucial that the order of the channel parameters
          -- declarations is preserved!
          procChIds <- traverse (chIdsM .@) (getLoc <$> procDeclChParams pd)
          let pvIds' = snd <$> pvIds
          return ( pId, ProcDef procChIds pvIds' b )

-- | TODO: remove the unnecessary constraints.
stautDeclsToProcDefMap :: ( MapsTo Text SortId mm
                          , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                          , MapsTo (Loc FuncDeclE) FuncId mm
                          , MapsTo FuncId (FuncDef VarId) mm
                          , MapsTo (Loc ProcDeclE) ProcInfo mm
                          , In (Loc VarDeclE, VarId) (Contents mm) ~ 'False
                          , In (Text, ChanId) (Contents mm) ~ 'False
                          , In (Loc ChanDeclE, ChanId) (Contents mm) ~ 'False
                          , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False
                          , In (ProcId, ()) (Contents mm) ~ 'False
                          , In (Loc VarDeclE, SortId) (Contents mm) ~ 'False )
                       => mm
                       -> [StautDecl]
                       -> CompilerM (Map ProcId ProcDef)
stautDeclsToProcDefMap mm ts = Map.fromList . concat <$>
    traverse (stautDeclsToProcDefs (innerMap mm)) ts
    where
      stautDeclsToProcDefs :: Map (Loc ProcDeclE) ProcInfo
                           -> StautDecl
                           -> CompilerM [(ProcId, ProcDef)]
      stautDeclsToProcDefs pms staut = do
          p@(ProcInfo pId chIds pvIds) <- mm .@ asProcDeclLoc staut :: CompilerM ProcInfo
          let chIdsM = Map.fromList chIds
          procChIds <- traverse (chIdsM .@) (getLoc <$> stautDeclChParams staut)
          let pvIds' = snd <$> pvIds
          beStaut <- uncurry3 Beh.stAut <$> stautItemToBExpr p
          return [( pId, ProcDef procChIds pvIds' beStaut )
                 -- , undefined -- TODO: defined proc defs for "std"
                 -- , undefined -- TODO: defined proc defs for "stdi"
                 ]
          where
            -- | Compile the list of @StautItem@'s to a triple of the initial state,
            -- a local variables map, and a list of transitions.
            stautItemToBExpr:: ProcInfo -> CompilerM (StatId, VEnv, [Beh.Trans])
            stautItemToBExpr (ProcInfo pId chIds pvIds) = do
                -- Collect all the explicit variable declarations.
                innerVSIds <- getMap mm (stautDeclInnerVars staut)
                             :: CompilerM (Map (Loc VarDeclE) SortId)
                innerVIds <- getMap (innerVSIds :& mm) (stautDeclInnerVars staut)
                             :: CompilerM (Map (Loc VarDeclE) VarId)
                -- Collect all the implicit variable declarations (declared in offers).
                let mpd = Map.fromList $ zip (allProcIds pms) (repeat ())
                chDecls <- getMap () staut
                implVSIds <- Map.fromList <$>
                    inferVarTypes ( Map.fromList pvIds
                                  :& mm
                                  :& Map.fromList (fmap (second varsort) pvIds) -- TODO: reduce dup.
                                  :& mpd
                                  :& chDecls
                                  :& Map.fromList chIds
                                  )
                                  (stautTrans staut)
                implVids  <- mkVarIds implVSIds (stautTrans staut)
                let extraVIds = innerVIds `Map.union` Map.fromList implVids
                -- Collect all the state declarations.
                -- TODO: check that there are no duplicated states.
                stIds    <- traverse stateDeclToStateId (stautDeclStates staut)
                -- Make sure there is a unique initial state.
                InitStateDecl iniStRef uds <- getUniqueInitialStateDecl
                iniSt    <- lookupStatId iniStRef stIds
                initVEnv <- stUpdatesToVEnv extraVIds uds
                trans    <- mkTransitions stIds chDecls extraVIds
                return (iniSt, initVEnv, trans)
                where
                  stateDeclToStateId :: StateDecl -> CompilerM StatId
                  stateDeclToStateId st = do
                      stId <- getNextId
                      return $ StatId (stateDeclName st) (Id stId) pId

                  getUniqueInitialStateDecl :: CompilerM InitStateDecl
                  getUniqueInitialStateDecl =
                      case stautInitStates staut of
                          [initStDecl] -> return initStDecl
                          [] -> throwError Error
                              { _errorType = NoDefinition
                              , _errorLoc   = getErrorLoc staut
                              , _errorMsg  = "No initial state declaration"
                              }
                          _  -> throwError Error
                              { _errorType = MultipleDefinitions
                              , _errorLoc  = getErrorLoc staut
                              , _errorMsg  = "Multiple initial state declarations"
                              }

                  lookupStatId :: StateRef -> [StatId] -> CompilerM StatId
                  lookupStatId sr sts = maybe throwE return mElem
                      where mElem = find ((stateRefName sr ==) . StatId.name) sts
                            throwE = throwError Error
                                { _errorType = UndefinedRef
                                , _errorLoc = getErrorLoc sr
                                , _errorMsg = "Could not find the state"
                                }

                  stUpdatesToVEnv :: Map (Loc VarDeclE) VarId
                                  -> [StUpdate]
                                  -> CompilerM VEnv
                  stUpdatesToVEnv lVIds uds = Map.fromList . concat <$>
                      -- TODO: check that there are no overlapping updates
                      -- (more than one update to the same variable).
                      traverse (stUpdateToVEnv lVIds) uds

                  stUpdateToVEnv :: Map (Loc VarDeclE) VarId
                                 -> StUpdate
                                 -> CompilerM [(VarId, ValExpr VarId)]
                  stUpdateToVEnv lVIds (StUpdate vrs e) = do
                      -- Lookup the variable declarations that correspond to the references.
                      vIds <- traverse (findVarIdM (lVIds :& mm)) (getLoc <$> vrs)
                      case vIds of
                          [] -> return []
                          v:_ -> do
                              vExp <- liftEither $
                                  expDeclToValExpr ((pvIds <.++> lVIds) :& mm) (varsort v) e
                              return (zip vIds (repeat vExp))

                  mkTransitions :: [StatId]
                                -> Map (Loc ChanRefE) (Loc ChanDeclE)
                                -> Map (Loc VarDeclE) VarId
                                -> CompilerM [Beh.Trans]
                  mkTransitions stIds chDecls lVIds =
                      traverse mkTransition (stautTrans staut)
                      where
                        mkTransition :: Transition -> CompilerM Beh.Trans
                        mkTransition (Transition fSr ofrD updD tSr) = do
                            from <- lookupStatId fSr stIds
                            to   <- lookupStatId tSr stIds
                            let
                                allVIds = pvIds <.++> lVIds
                                allVSIds = Map.map varsort allVIds
                            ofr  <- toActOffer (  (chIds .& allVIds)
                                               :& allVSIds
                                               :& chDecls
                                               :& mm
                                               )
                                               ofrD
                            upd  <- stUpdatesToVEnv allVIds updD
                            return $ Beh.Trans from ofr upd to

      uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
      uncurry3 f (a, b, c) = f a b c
