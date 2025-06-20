{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Data.ProcDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compiler functions and instances on process declarations.
--------------------------------------------------------------------------------
module TorXakis.Compiler.Data.ProcDecl
    (ProcInfo (ProcInfo), allProcIds, getPId)
where

import           Data.Map                           (Map)
import           Data.Text                          (Text)

import           ChanId                             (ChanId)
import           Id                                 (Id (Id))
import           ProcId                             (ProcId (ProcId), toChanSort)
import           SortId                             (SortId, sortIdInt)
import           StautDef                           (combineParameters)
import           TxsDefs                            (ExitSort (Exit, Hit, NoExit))
import           VarId                              (VarId (VarId), varsort)

import           TorXakis.Compiler.Data             (CompilerM, getNextId)
import           TorXakis.Compiler.Data.VarDecl     ()
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps             ((<!!>))
import           TorXakis.Compiler.Maps.DefinesAMap (DefinesAMap, getKVs,
                                                     getMap, predefChDecls,
                                                     uGetKVs)
import           TorXakis.Compiler.MapsTo           ((:&) ((:&)), Contents, In,
                                                     MapsTo, values, (<.+>))
import           TorXakis.Compiler.ValExpr.SortId   (sortIds)
import           TorXakis.Compiler.Validation
import           TorXakis.Parser.Data               (ChanDeclE, ChanRefE, ExitSortDecl (ExitD, HitD, NoExitD),
                                                     Loc (ExtraAut), ProcDecl,
                                                     ProcDeclE, StautDecl,
                                                     Transition (Transition),
                                                     VarDeclE, asProcDeclLoc,
                                                     chanDeclName, getLoc,
                                                     procDeclChParams,
                                                     procDeclName,
                                                     procDeclParams,
                                                     procDeclRetSort,
                                                     stautDeclChParams,
                                                     stautDeclInnerVars,
                                                     stautDeclParams,
                                                     stautDeclRetSort,
                                                     stautName, stautTrans)

-- | Information about a process.
data ProcInfo = ProcInfo ProcId [(Loc ChanDeclE, ChanId)] [(Loc VarDeclE, VarId)]

-- | Get the @ProcId@ of a process info value.
getPId :: ProcInfo -> ProcId
getPId (ProcInfo pId _ _) = pId

instance ( MapsTo Text SortId mm
         , In (Loc VarDeclE, SortId) (Contents mm) ~ 'False
         ) => DefinesAMap (Loc ProcDeclE) ProcInfo ProcDecl mm where
    uGetKVs mm pd = do
        checkUnique (getErrorLoc pd, Channel, "Process channel parameter")
                    (chanDeclName <$> procDeclChParams pd)
        pId    <- getNextId
        allPChIds <- getKVs mm pd
            :: CompilerM [(Loc ChanDeclE, ChanId)]
        let
            pChLocs = getLoc <$> procDeclChParams pd
            pChIds = snd <$> filter ((`elem` pChLocs) . fst) allPChIds
        vdSIds <- getMap mm (procDeclParams pd)
            :: CompilerM (Map (Loc VarDeclE) SortId)
        pVIds  <- uGetKVs (vdSIds :& mm) (procDeclParams pd)
            :: CompilerM [(Loc VarDeclE, VarId)]
        eSort  <- declExitSort mm (procDeclRetSort pd) <!!> pd
        return [( getLoc pd
                , ProcInfo ( ProcId (procDeclName pd)
                                    (Id pId)
                                    (toChanSort <$> pChIds)
                                    (varsort . snd <$> pVIds)
                                    eSort
                           )
                           allPChIds
                           pVIds
                )]

-- | Exit sort of an exit sort declaration.
declExitSort :: (MapsTo Text SortId mm)
             => mm -> ExitSortDecl -> CompilerM ExitSort
declExitSort _  NoExitD    = return NoExit
declExitSort _  HitD       = return Hit
declExitSort mm (ExitD xs) = Exit <$> sortIds mm xs

instance ( MapsTo Text SortId mm
         , In (Loc VarDeclE, SortId) (Contents mm) ~ 'False
         ) => DefinesAMap (Loc ProcDeclE) ProcInfo StautDecl mm where
    uGetKVs mm staut = do
        pId       <- getNextId
        pIdStd    <- getNextId
        pIdStdi   <- getNextId
        allSChIds <- uGetKVs mm staut
                     :: CompilerM [(Loc ChanDeclE, ChanId)]
        let loc = asProcDeclLoc staut
            n   = stautName staut
            sChLocs = getLoc <$> stautDeclChParams staut
            sChIds = snd <$> filter ((`elem` sChLocs) . fst) allSChIds
        vdSIds <- getMap mm (stautDeclParams staut)
                  :: CompilerM (Map (Loc VarDeclE) SortId)
        pVIds  <- uGetKVs (vdSIds :& mm) (stautDeclParams staut)
            :: CompilerM [(Loc VarDeclE, VarId)]
        eSort  <- declExitSort mm (stautDeclRetSort staut) <!!> staut
        -- NOTE: this is required to comply with the current TorXakis
        -- compiler, which generates an "std_" variant based on a
        -- 'combineParameters' function which shouldn't be exposed (according
        -- to the comments of 'StautDef').
        innerVdSIds <- getMap mm (stautDeclInnerVars staut)
                       :: CompilerM (Map (Loc VarDeclE) SortId)
        innerVdIds  <- uGetKVs (innerVdSIds :& mm) (stautDeclInnerVars staut)
                       :: CompilerM [(Loc VarDeclE, VarId)]
        sVId        <- getNextId
        let
            s = VarId "$s" (Id sVId) sortIdInt
            stdVids = combineParameters (snd <$> pVIds) s (snd <$> innerVdIds)
        return [ ( loc
                 , ProcInfo ( ProcId n
                                    (Id pId)
                                    (toChanSort <$> sChIds)
                                    (varsort . snd <$> pVIds)
                                    eSort
                            )
                           allSChIds
                           pVIds
                 )
               , ( ExtraAut "std" loc
                 , ProcInfo ( ProcId ("std_" <> n)
                                    (Id pIdStd)
                                    (toChanSort <$> sChIds)
                                    (varsort <$> stdVids)
                                    eSort
                            )
                           allSChIds
                           pVIds
                 )
               , ( ExtraAut "stdi" loc
                 , ProcInfo ( ProcId ("stdi_" <> n)
                                    (Id pIdStdi)
                                    (toChanSort <$> sChIds)
                                    (varsort . snd <$> pVIds)
                                    eSort
                            )
                           allSChIds
                           pVIds
                 )
               ]

-- | Get all the @ProcId@'s in a composite map.
allProcIds :: MapsTo (Loc ProcDeclE) ProcInfo mm => mm -> [ProcId]
allProcIds mm = getPId <$> values @(Loc ProcDeclE) @ProcInfo mm

instance DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) StautDecl () where
    uGetKVs _ staut = do
        chanDecls <- getMap () (stautDeclChParams staut) :: CompilerM (Map Text (Loc ChanDeclE))
        uGetKVs (predefChDecls <.+> chanDecls) (stautTrans staut)

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) Transition mm where
    uGetKVs mm (Transition _ offr _ _) = uGetKVs mm offr
