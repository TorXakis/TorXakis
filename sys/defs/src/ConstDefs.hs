{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module ConstDefs
where

import GHC.Generics (Generic)
import Control.DeepSeq

import qualified Data.Map as Map

import CstrId

-- | Const
-- Union of Boolean, integer, string, and AlgebraicDataType constant values
data  Const         =  Cbool    { cBool :: Bool }
                     | Cint     { cInt :: Integer }
                     | Cstring  { cString :: String }
                     | Cregex   { cRegex :: String }      -- XSD input
                                           -- PvdL: performance gain: translate only once,
                                           --       storing SMT string as well
                     | Cstr     { cstrId :: CstrId, args :: [Const] }
                     | Cerror   { msg :: String }
     deriving (Eq,Ord,Read,Show, Generic, NFData)

type  WEnv v        =  Map.Map v Const

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
