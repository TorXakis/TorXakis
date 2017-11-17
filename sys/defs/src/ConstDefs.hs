{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module ConstDefs
where

import           Control.DeepSeq
import           Data.Data
import qualified Data.Map        as Map
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           CstrId
import           SortId

-- | Union of Boolean, Integer, String, and AlgebraicDataType constant values.
data Const = Cbool    { cBool :: Bool }
           | Cint     { cInt :: Integer }
           | Cstring  { cString :: Text }
           | Cregex   { cRegex :: Text } -- ^ XSD input
                                         -- PvdL: performance gain: translate only once,
                                         --       storing SMT string as well
           | Cstr     { cstrId :: CstrId, args :: [Const] }
           | Cerror   { msg :: String }
           | Cany     { sort :: SortId }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

type WEnv v = Map.Map v Const
