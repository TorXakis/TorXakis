{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications  #-}
module TorXakis.Compiler.Defs.ProcDef where

import           Control.Arrow (second)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.Error.Class (throwError)
import           Data.Semigroup ((<>))

import           TxsDefs (ProcId (ProcId), ProcDef (ProcDef), ExitSort (..))
import           SortId (SortId)
import           qualified SortId
import           Id (Id (Id))
import           VarId (VarId, varsort)
import           FuncId (FuncId)
import           FuncDef (FuncDef)
import           ChanId (ChanId)

import TorXakis.Compiler.MapsTo
import TorXakis.Compiler.Maps
import TorXakis.Compiler.Data
import TorXakis.Parser.Data
import TorXakis.Compiler.Defs.ChanId
import TorXakis.Compiler.ValExpr.VarId
import TorXakis.Compiler.ValExpr.SortId
import TorXakis.Compiler.Defs.BehExprDefs
import TorXakis.Compiler.Error
import           TorXakis.Compiler.ValExpr.Common
import TorXakis.Compiler.Maps.DefinesAMap

import           TorXakis.Compiler.Data.VarDecl
import           TorXakis.Compiler.Data.ProcDecl

procDeclsToProcDefMap :: ( MapsTo Text SortId mm
                         , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
                         , MapsTo FuncDefInfo FuncId mm
                         , MapsTo FuncId (FuncDef VarId) mm
--                         , MapsTo Text [ProcId] mm
                         -- , MapsTo (Loc ProcDeclE) [(Loc VarDeclE, VarId)] mm
--                         , MapsTo (Loc ProcDeclE) [(Loc ChanDeclE, ChanId)] mm
                         -- , MapsTo Double Int mm
--                         , DefinesAMap (Loc VarDeclE) VarId ProcDecl mm
                         , In (Loc VarDeclE, VarId) (Contents mm) ~ 'False
                         , In (Text, ChanId) (Contents mm) ~ 'False
                         , In (Loc ChanDeclE, ChanId) (Contents mm) ~ 'False
                         , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False
                         , In (ProcId, ()) (Contents mm) ~ 'False
                         , In (Loc VarDeclE, SortId) (Contents mm) ~ 'False )
                      => mm
                      -> [ProcDecl]
                      -> CompilerM (Map ProcId ProcDef)
procDeclsToProcDefMap mm ps =
    gProcDeclsToProcDefMap emptyMpd ps
    where
      emptyMpd = Map.empty :: Map ProcId ProcDef
      gProcDeclsToProcDefMap :: Map ProcId ProcDef
                             -> [ProcDecl]
                             -> CompilerM (Map ProcId ProcDef)
      gProcDeclsToProcDefMap mpd rs = do
          pms <- getMap mm rs
          (ls, rs') <- traverseCatch (mkpIdPDefM mpd pms) rs
          case (ls, rs') of
              ([], _) -> return $ Map.fromList rs' <> innerMap mpd
              (_, []) -> throwError Error
                  { _errorType = ProcessNotDefined
                  , _errorLoc = NoErrorLoc
                  , _errorMsg = T.pack (show (snd <$> ls))
                  }
              (_, _ ) -> gProcDeclsToProcDefMap (Map.fromList rs' <.+> mpd) (fst <$> ls)

      mkpIdPDefM :: Map ProcId ProcDef
                 -> Map (Loc ProcDeclE) ProcDeclC
                 -> ProcDecl
                 -> CompilerM (ProcId, ProcDef)
      mkpIdPDefM mpd pms pd = do
--          pId         <- getNextId
          (newPid, chIds, pvIds) <- pms .@ getLoc pd
              :: CompilerM ProcDeclC
          -- Scan for channel references and declarations
          chDecls <- getMap () pd :: CompilerM (Map (Loc ChanRefE) (Loc ChanDeclE))
--          chIds   <- getKVs mm pd :: CompilerM [(Loc ChanDeclE, ChanId)]
          let chIdsM = Map.fromList chIds
          procChIds <- traverse (chIdsM .@) (getLoc <$> procDeclChParams pd)
          -- List of VarId's associated to the variables declared by a formal parameter.
--          pvIds       <- mm .@ getLoc pd :: CompilerM [(Loc VarDeclE, VarId)]
--          pvIds <- getKVs mm pd :: CompilerM [(Loc VarDeclE, VarId)]
--          e           <- declExitSort (procRetSort . procDeclComps $ pd) <!!> pd
          let body = procDeclBody pd
              pvIds' = snd <$> pvIds
              -- NOTE: we need to introduce a temporary ProcId to deal with the case of recursive process!
--              newPid = ProcId (procDeclName pd) (Id pId) procChIds pvIds' e
              -- We add the newly defined process to the map.
              mpd' = Map.fromList $ (newPid, ()) : zip (keys @ProcId @ProcDef mpd) (repeat ())
          -- List of VarId's associated to the variables declared by a question mark offer.
          bTypes      <- Map.fromList <$> inferVarTypes (  Map.fromList pvIds
                                                        :& mm
                                                        :& Map.fromList (fmap (second varsort) pvIds)
                                                        :& mpd'
                                                        :& chDecls
                                                        :& chIdsM ) body
          bvIds       <- mkVarIds bTypes body
          b       <- toBExpr (  bTypes
--                             :& (allChIds .&.
                             :& Map.fromList (pvIds ++ bvIds)
                             :& mm
                             :& mpd'
                             :& chDecls
                             :& chIdsM
                             ) body
          -- NOTE that it is crucial that the order of the channel parameters
          -- declarations is preserved!
          return ( newPid
                 , ProcDef procChIds pvIds' b
                 )
--              where
                -- declExitSort NoExitD    = return NoExit
                -- declExitSort HitD       = return Hit
                -- declExitSort (ExitD xs) = Exit <$> sortIds mm xs
