{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.ValExpr.CstrId
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation functions related to 'TorXakis' constructor id's.
--------------------------------------------------------------------------------
module TorXakis.Compiler.ValExpr.CstrId
    (compileToCstrId)
where

import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text)

import           CstrId                   (CstrId (CstrId))
import           Id                       (Id (Id))
import           SortId                   (SortId)

import           TorXakis.Compiler.Data   (CompilerM, getNextId)
import           TorXakis.Compiler.Maps   (findSortIdM)
import           TorXakis.Compiler.MapsTo (MapsTo)
import           TorXakis.Parser.Data     (ADTDecl, CstrDecl, CstrE, Loc,
                                           adtName, constructors, cstrFields,
                                           cstrName, fieldSort, getLoc, nodeLoc)

-- | Compile a list of ADT declarations into a map from the location of the
-- constructor declaration, to their corresponding constructor id's.
compileToCstrId :: (MapsTo Text SortId mm)
                => mm -> [ADTDecl] -> CompilerM (Map (Loc CstrE) CstrId)
compileToCstrId mm ds = Map.fromList . concat <$>
    traverse (adtToCstrId mm) ds

adtToCstrId :: (MapsTo Text SortId mm)
            => mm
            -> ADTDecl
            -> CompilerM [(Loc CstrE, CstrId)]
adtToCstrId mm a = do
    sId <- findSortIdM mm (adtName a, nodeLoc a)
    traverse (cstrToCstrId mm sId) (constructors a)

cstrToCstrId :: (MapsTo Text SortId mm)
             => mm
             -> SortId -- ^ SortId of the containing ADT.
             -> CstrDecl
             -> CompilerM (Loc CstrE, CstrId)
cstrToCstrId mm sId c = do
    i <- getNextId
    aSids <- traverse (findSortIdM mm . fieldSort) (cstrFields c)
    return (getLoc c, CstrId (cstrName c) (Id i) aSids sId)
