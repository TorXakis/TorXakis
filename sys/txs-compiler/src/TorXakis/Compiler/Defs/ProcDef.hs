{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TupleSections       #-}
module TorXakis.Compiler.Defs.ProcDef where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
    
import TxsDefs (ProcId (ProcId), ProcDef (ProcDef), ExitSort (..))
import SortId (SortId)
import qualified SortId 
import Id (Id (Id))

import TorXakis.Compiler.MapsTo
import TorXakis.Compiler.Data hiding (lookupWithLocM, lookupM)
import TorXakis.Parser.Data
import TorXakis.Compiler.Defs.ChanId
import TorXakis.Compiler.ValExpr.VarId
import TorXakis.Compiler.Defs.BehExprDefs

procDeclsToProcDefMap :: ( MapsTo Text SortId mm )
-- TODO: from the environment we need:
--
-- - the Loc VarDeclE to VarId map 
                     => mm
                     -> [ProcDecl]
                     -> CompilerM (Map ProcId ProcDef)
procDeclsToProcDefMap mm ps = Map.fromList <$>
    traverse procDeclToProcDefMap ps
    where
      procDeclToProcDefMap :: ProcDecl -> CompilerM (ProcId, ProcDef)
      procDeclToProcDefMap pd = do
          pId   <- getNextId
          chIdsMap <- 
              chanDeclsToChanIds mm (procChParams . procDeclComps $ pd)
          pdVdSortMap <- procDefVdSortMap
          vIdsMap  <- 
              let e = SEnv pdVdSortMap  in 
                  traverse (varIdsFromVarDecl e) vdecls                  
          e <- exitSort (procRetSort . procDeclComps $ pd)
          b <- toBExpr (chIdsMap .&. vIdsMap) (procBody . procDeclComps $ pd)
          let chIds = snd <$> chIdsMap
              vIds  = snd <$> vIdsMap
          return ( ProcId (procDeclName pd) (Id pId) chIds vIds e
                 , ProcDef chIds vIds b
                 )
              where
                vdecls = procParams . procDeclComps $ pd
                procDefVdSortMap :: CompilerM (Map (Loc VarDeclE) SortId)
                procDefVdSortMap = Map.fromList <$> traverse vdToSortId vdecls 
                    where
                      vdToSortId :: VarDecl -> CompilerM (Loc VarDeclE, SortId)
                      vdToSortId vd = 
                          (getLoc vd, ) <$> lookupWithLocM (varDeclSort vd) mm
                exitSort :: ExitSortDecl -> CompilerM ExitSort
                exitSort NoExitD   = return NoExit
                exitSort HitD      = return Hit
                exitSort (ExitD xs) = Exit <$>
                    traverse (`lookupM` mm) (sortRefName <$> xs)
                    
