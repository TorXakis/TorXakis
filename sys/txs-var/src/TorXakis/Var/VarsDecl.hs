{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  VarsDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Variables Declarations
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
module TorXakis.Var.VarsDecl
( VarsDecl
, mkVarsDecl
, toList
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           GHC.Generics        (Generic)

import TorXakis.Error
import TorXakis.Name
import TorXakis.SortContext
import TorXakis.Var.VarDef

-- | Data for a variables declarations.
--   The order of the variables declarations is relevant.
newtype VarsDecl = VarsDecl { -- | toList
                                toList :: [VarDef]
                        }
         deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

mkVarsDecl :: SortContext a => a -> [VarDef] -> Either Error VarsDecl
mkVarsDecl ctx l | not $ null nuVars            = Left $ Error ("Non unique names: " ++ show nuVars)
                 | not $ null undefinedSorts    = Left $ Error ("List of variables with undefined sorts: " ++ show undefinedSorts)
                 | otherwise                    = Right $ VarsDecl l
    where
        nuVars :: [VarDef]
        nuVars = repeatedByName l

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . memberSort ctx . sort) l

