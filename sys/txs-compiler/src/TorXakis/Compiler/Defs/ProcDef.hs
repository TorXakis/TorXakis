{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies          #-}
module TorXakis.Compiler.Defs.ProcDef where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

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
                         , In (Loc VarDeclE, SortId) (Contents mm) ~ 'False )
                      => mm
                      -> [ProcDecl]
                      -> CompilerM (Map ProcId ProcDef)
procDeclsToProcDefMap mm ps = Map.fromList <$>
    traverse procDeclToProcDefMap ps
    where
      procDeclToProcDefMap :: ProcDecl -> CompilerM (ProcId, ProcDef)
      procDeclToProcDefMap pd = do
          pId         <- getNextId
          chIds       <- chanDeclsToChanIds mm (procChParams . procDeclComps $ pd)
          let allChIds = chIds ++ predefinedChans
          pdVdSortMap <- procDefVdSortMap
          -- List of VarId's associated to the variables declared by a formal parameter.
          pvIds       <- traverse (varIdFromVarDecl pdVdSortMap) vdecls
          let body = procBody . procDeclComps $ pd
          -- List of VarId's associated to the variables declared by a question mark offer.
          bTypes      <- Map.fromList <$> inferVarTypes (allChIds .& (pvIds .& mm) :& pdVdSortMap) body
          bvIds       <- mkVarIds bTypes body
          e           <- exitSort (procRetSort . procDeclComps $ pd) <!!> pd
          -- TODO: we need to generate a Loc VarRefE to Loc VarDeclE mapping
          -- for the variables inside a process definition. Here the trickiest
          -- part is to consider that question offers introduce a local variable (In ? x >-> Out ! x + 1)
          b           <- toBExpr (allChIds .&. (pvIds ++ bvIds) :& mm) body
          -- NOTE that it is crucial that the order of the channel parameters
          -- declarations is preserved!
          let chIds' = snd <$> chIds
              pvIds'   = snd <$> pvIds
          return ( ProcId (procDeclName pd) (Id pId) chIds' pvIds' e
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
                exitSort :: ExitSortDecl -> CompilerM ExitSort
                exitSort NoExitD   = return NoExit
                exitSort HitD      = return Hit
                exitSort (ExitD xs) = fmap Exit $
                    traverse (mm .@!!) $ zip (sortRefName <$> xs) (getLoc <$> xs)
