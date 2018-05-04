{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Process declarations.
module TorXakis.Compiler.Data.ProcDecl where

import           Control.Lens                       ((^.), (^..))
import           Data.Data.Lens                     (biplate)

import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Text                          (Text)

import           ChanId                             (ChanId)
import           Id                                 (Id (Id))
import           ProcId                             (ProcId (ProcId))
import           SortId                             (SortId)
import           TxsDefs                            (ExitSort (..),
                                                     ProcDef (ProcDef),
                                                     ProcId (ProcId))
import           VarId                              (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Maps.DefinesAMap
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.VarId
import           TorXakis.Parser.Data

import           TorXakis.Compiler.Data.VarDecl

-- | Information about a process.
data ProcInfo = ProcInfo ProcId [(Loc ChanDeclE, ChanId)] [(Loc VarDeclE, VarId)]

getPId :: ProcInfo -> ProcId
getPId (ProcInfo pId _ _) = pId

instance ( MapsTo Text SortId mm
         , In (Loc VarDeclE, SortId) (Contents mm) ~ 'False
         ) => DefinesAMap (Loc ProcDeclE) ProcInfo ProcDecl mm where
    uGetKVs mm pd = do
        pId    <- getNextId
        allPChIds <- uGetKVs mm pd
            :: CompilerM [(Loc ChanDeclE, ChanId)]
        let
            pChLocs = getLoc <$> procDeclChParams pd
            pChIds = snd <$>
                filter ((`elem` pChLocs) . fst) allPChIds
        vdSids <- getMap mm (procDeclParams pd)
            :: CompilerM (Map (Loc VarDeclE) SortId)
        pVIds  <- uGetKVs (vdSids :& mm) (procDeclParams pd)
            :: CompilerM [(Loc VarDeclE, VarId)]
        eSort  <- declExitSort (procDeclRetSort pd) <!!> pd
        return [( getLoc pd
                , ProcInfo ( ProcId
                             (procDeclName pd)
                             (Id pId)
                             pChIds
                             (snd <$> pVIds)
                             eSort
                           )
                           allPChIds
                           pVIds
                )]
            where
              declExitSort NoExitD    = return NoExit
              declExitSort HitD       = return Hit
              declExitSort (ExitD xs) = Exit <$> sortIds mm xs

-- -- | A process declaration introduces variable id's in its parameters.
-- instance ( MapsTo Text SortId mm
--          ) => DefinesAMap (Loc VarDeclE) VarId ProcDecl mm where
--     uGetKVs mm pd = do
--         pdSIds <- getMap mm (procDeclParams pd) :: CompilerM (Map (Loc VarDeclE) SortId)
--         traverse (varIdFromVarDecl pdSIds) (procDeclParams pd)
--         --return [(getLoc pd, pVIds)]

allProcIds :: MapsTo (Loc ProcDeclE) ProcInfo mm => mm -> [ProcId]
allProcIds mm = getPId <$> values @(Loc ProcDeclE) @ProcInfo mm
