{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Data.VarDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compiler functions and instances on variable declarations.
--------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-orphans #-}
module TorXakis.Compiler.Data.VarDecl
    ()
where

import           Data.Text                          (Text)

import           SortId                             (SortId)
import           VarId                              (VarId)

import           TorXakis.Compiler.Maps             ((.@!!))
import           TorXakis.Compiler.Maps.DefinesAMap (DefinesAMap, uGetKVs)
import           TorXakis.Compiler.MapsTo           (MapsTo)
import           TorXakis.Compiler.ValExpr.VarId    (varIdFromVarDecl)
import           TorXakis.Parser.Data               (Loc, VarDecl, VarDeclE,
                                                     getLoc, varDeclSort)

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc VarDeclE) SortId VarDecl mm where
    uGetKVs mm vd = pure . (getLoc vd, ) <$>  mm .@!! varDeclSort vd

-- | A process declaration introduces variable id's in its parameters.
instance ( MapsTo (Loc VarDeclE) SortId mm
         ) => DefinesAMap (Loc VarDeclE) VarId VarDecl mm where
    uGetKVs mm vd = pure <$> varIdFromVarDecl mm vd
