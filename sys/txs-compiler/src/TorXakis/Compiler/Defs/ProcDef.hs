{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
module TorXakis.Compiler.Defs.ProcDef where

import           Control.Arrow                      (second)
import           Control.Monad.Error.Class          (throwError)
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
import           StatId                             (StatId)
import           TxsDefs                            (ExitSort (..),
                                                     ProcDef (ProcDef),
                                                     ProcId (ProcId), VEnv)
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
    traverse stautDeclsToProcDefs ts
    where
      stautDeclsToProcDefs :: StautDecl -> CompilerM [(ProcId, ProcDef)]
      stautDeclsToProcDefs staut = do
          ProcInfo pId chIds pvIds <- mm .@ asProcDeclLoc staut :: CompilerM ProcInfo
          let chIdsM = Map.fromList chIds
          procChIds <- traverse (chIdsM .@) (getLoc <$> stautDeclChParams staut)
          let pvIds' = snd <$> pvIds
          beStaut <- uncurry3 Beh.stAut <$> stautItemToBExpr
          return [( pId, ProcDef procChIds pvIds' beStaut )]
          where
            -- | Compile the list of @StautItem@'s to a triple of the initial state,
            -- a local variables map, and a list of transitions.
            stautItemToBExpr:: CompilerM (StatId, VEnv, [Beh.Trans])
            stautItemToBExpr = do
                -- Collect all the state declarations.
                stIds <- traverse stateDeclToStateId (stautDeclStates staut)
                -- Make sure there is a unique initial state.
                undefined stIds

            stateDeclToStateId :: StateDecl -> CompilerM StatId
            stateDeclToStateId = undefined

      uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
      uncurry3 f (a, b, c) = f a b c
