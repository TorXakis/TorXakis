{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- TODO: We are not exporting 'DefinesAMap' outside the compiler, so the
-- declaration of orphan instances is not likely to cause problems. As an
-- alternative we can move the 'ProcDecl' parser data type here. However I
-- don't know how good that design decision is, since the parser and the
-- compiler are sharing the same module.

-- | Process declarations.
module TorXakis.Compiler.Data.ProcDecl where

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

type ProcDeclC = (ProcId, [(Loc ChanDeclE, ChanId)], [(Loc VarDeclE, VarId)])

instance ( MapsTo Text SortId mm
         , In (Loc VarDeclE, SortId) (Contents mm) ~ 'False
         ) => DefinesAMap (Loc ProcDeclE) ProcDeclC ProcDecl mm where
    getKVs mm pd = do
        pId    <- getNextId
        allPChIds <- getKVs mm pd
            :: CompilerM [(Loc ChanDeclE, ChanId)]
        let
            pChLocs = getLoc <$> procDeclChParams pd
            pChIds = snd <$>
                filter ((`elem` pChLocs) . fst) allPChIds
        vdSids <- getMap mm (procDeclParams pd)
            :: CompilerM (Map (Loc VarDeclE) SortId)
        pVIds  <- getKVs (vdSids :& mm) (procDeclParams pd)
            :: CompilerM [(Loc VarDeclE, VarId)]
        eSort  <- declExitSort (procDeclRetSort pd) <!!> pd
        return [( getLoc pd
                , ( ProcId
                      (procDeclName pd)
                      (Id pId)
                      pChIds
                      (snd <$> pVIds)
                      eSort
                  , allPChIds
                  , pVIds
                  )
                )]
            where
              declExitSort NoExitD    = return NoExit
              declExitSort HitD       = return Hit
              declExitSort (ExitD xs) = Exit <$> sortIds mm xs

-- -- | A process declaration introduces variable id's in its parameters.
-- instance ( MapsTo Text SortId mm
--          ) => DefinesAMap (Loc VarDeclE) VarId ProcDecl mm where
--     getKVs mm pd = do
--         pdSIds <- getMap mm (procDeclParams pd) :: CompilerM (Map (Loc VarDeclE) SortId)
--         traverse (varIdFromVarDecl pdSIds) (procDeclParams pd)
--         --return [(getLoc pd, pVIds)]

allProcIds :: MapsTo (Loc ProcDeclE) ProcDeclC mm => mm -> [ProcId]
allProcIds mm = (\(pId, _, _) -> pId) <$> values @(Loc ProcDeclE) @ProcDeclC mm
