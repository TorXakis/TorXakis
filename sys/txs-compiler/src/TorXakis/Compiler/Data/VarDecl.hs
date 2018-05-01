{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module TorXakis.Compiler.Data.VarDecl where

import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Text                          (Text)

import           SortId                             (SortId)
import           VarId                              (VarId (VarId))

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Maps.DefinesAMap
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.VarId
import           TorXakis.Parser.Data

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc VarDeclE) SortId VarDecl mm where
    getKVs mm vd = pure . (getLoc vd, ) <$>  mm .@!! varDeclSort vd

-- | A process declaration introduces variable id's in its parameters.
instance ( MapsTo (Loc VarDeclE) SortId mm
         ) => DefinesAMap (Loc VarDeclE) VarId VarDecl mm where
    getKVs mm vd = pure <$> varIdFromVarDecl mm vd

