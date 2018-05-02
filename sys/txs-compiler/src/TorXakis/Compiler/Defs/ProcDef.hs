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
                         , In (Loc VarDeclE, VarId) (Contents mm) ~ 'False
                         , In (Text, ChanId) (Contents mm) ~ 'False
                         , In (Loc ChanDeclE, ChanId) (Contents mm) ~ 'False
                         , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False
                         , In (ProcId, ()) (Contents mm) ~ 'False
                         , In (Loc VarDeclE, SortId) (Contents mm) ~ 'False )
                      => mm
                      -> [ProcDecl]
                      -> CompilerM (Map ProcId ProcDef)
procDeclsToProcDefMap mm  = gProcDeclsToProcDefMap emptyMpd
    where
      emptyMpd = Map.empty :: Map ProcId ProcDef
      gProcDeclsToProcDefMap :: Map ProcId ProcDef
                             -> [ProcDecl]
                             -> CompilerM (Map ProcId ProcDef)
      gProcDeclsToProcDefMap mpd rs = do
          pms <- getMap mm rs
          (ls, rs') <- traverseCatch (mkpIdPDefM pms) rs
          case (ls, rs') of
              ([], _) -> return $ Map.fromList rs' <> innerMap mpd
              (_, []) -> throwError Error
                  { _errorType = ProcessNotDefined
                  , _errorLoc = NoErrorLoc
                  , _errorMsg = T.pack (show (snd <$> ls))
                  }
              (_, _ ) -> gProcDeclsToProcDefMap (Map.fromList rs' <.+> mpd) (fst <$> ls)

      mkpIdPDefM :: Map (Loc ProcDeclE) ProcDeclC
                 -> ProcDecl
                 -> CompilerM (ProcId, ProcDef)
      mkpIdPDefM pms pd = do
          (pId, chIds, pvIds) <- pms .@ getLoc pd :: CompilerM ProcDeclC
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
