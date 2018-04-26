{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications  #-}
module TorXakis.Compiler.Defs.ProcDef where

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
import           VarId (VarId)
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
    
procDeclsToProcDefMap :: ( MapsTo Text SortId mm
                         , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
                         , MapsTo FuncDefInfo FuncId mm
                         , MapsTo FuncId (FuncDef VarId) mm
                         , In (Loc VarDeclE, VarId) (Contents mm) ~ 'False
                         , In (Text, ChanId) (Contents mm) ~ 'False
                         , In (ProcId, ()) (Contents mm) ~ 'False 
                         , In (Loc VarDeclE, SortId) (Contents mm) ~ 'False )
                      => mm
                      -> [ProcDecl]
                      -> CompilerM (Map ProcId ProcDef)
procDeclsToProcDefMap mm ps = 
    -- Map.fromList <$> traverse mkpIdPDefM ps
    gProcDeclsToProcDefMap emptyMpd ps
    where
      emptyMpd = Map.empty :: Map ProcId ProcDef
      gProcDeclsToProcDefMap :: Map ProcId ProcDef
                             -> [ProcDecl]
                             -> CompilerM (Map ProcId ProcDef)
      gProcDeclsToProcDefMap mpd rs = do
          (ls, rs') <- traverseCatch (mkpIdPDefM mpd) rs
          case (ls, rs') of
              ([], _) -> return $ Map.fromList rs' <> innerMap mpd
              (_, []) -> throwError Error
                  { _errorType = ProcessNotDefined
                  , _errorLoc = NoErrorLoc
                  , _errorMsg = T.pack (show (snd <$> ls))
                  }
              (_, _ ) -> gProcDeclsToProcDefMap (Map.fromList rs' <.+> mpd) (fst <$> ls)
      
      mkpIdPDefM :: Map ProcId ProcDef
                 -> ProcDecl
                 -> CompilerM (ProcId, ProcDef)
      mkpIdPDefM mpd pd = do
          pId         <- getNextId
          chIds       <- chanDeclsToChanIds mm (procChParams . procDeclComps $ pd)
          exitChan    <- mkChanIds mm (procRetSort . procDeclComps $ pd)
          let allChIds = chIds ++ predefinedChans ++ exitChan
          pdVdSortMap <- procDefVdSortMap
          -- List of VarId's associated to the variables declared by a formal parameter.
          pvIds       <- traverse (varIdFromVarDecl pdVdSortMap) vdecls
          e           <- declExitSort (procRetSort . procDeclComps $ pd) <!!> pd
          let body = procBody . procDeclComps $ pd
              chIds' = snd <$> chIds
              pvIds' = snd <$> pvIds
              -- NOTE: we need to introduce a temporary ProcId to deal with the case of recursive process!
              newPid = ProcId (procDeclName pd) (Id pId) chIds' pvIds' e
              -- We add the newly defined process to the map.
              mpd' = Map.fromList $ (newPid, ()) : zip (keys @ProcId @ProcDef mpd) (repeat ())
          -- List of VarId's associated to the variables declared by a question mark offer.
          bTypes      <- Map.fromList <$> inferVarTypes (allChIds .& (pvIds .& mm) :& pdVdSortMap :& mpd') body
          bvIds       <- mkVarIds bTypes body
          b           <- toBExpr (bTypes :& (allChIds .&. (pvIds ++ bvIds)) :& mm :& mpd') body
          -- NOTE that it is crucial that the order of the channel parameters
          -- declarations is preserved!
          return ( newPid 
                 , ProcDef chIds' pvIds' b
                 )
              where
                vdecls = procParams . procDeclComps $ pd
                procDefVdSortMap :: CompilerM (Map (Loc VarDeclE) SortId)
                procDefVdSortMap = Map.fromList <$> traverse vdToSortId vdecls
                    where
                      vdToSortId :: VarDecl -> CompilerM (Loc VarDeclE, SortId)
                      vdToSortId vd =
                          (getLoc vd, ) <$>  mm .@!! varDeclSort vd
                declExitSort NoExitD    = return NoExit
                declExitSort HitD       = return Hit
                declExitSort (ExitD xs) = Exit <$> sortIds mm xs
