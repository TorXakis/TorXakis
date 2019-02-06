{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ChansDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Channel Declarations
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
module TorXakis.ChansDecl
( ChansDecl
, mkChansDecl
, toList
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import TorXakis.Error
import TorXakis.Name
import TorXakis.Sort (SortContext, memberSort)
import TorXakis.ChanDef

-- | Data for a channels declarations.
--   The order of the channels declarations is relevant.
newtype ChansDecl = ChansDecl { -- | toList
                                toList :: [ChanDef]
                        }
         deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

mkChansDecl :: SortContext a => a -> [ChanDef] -> Either MinError ChansDecl
mkChansDecl ctx l | not $ null nuChans            = Left $ MinError (T.pack ("Non unique names: " ++ show nuChans))
                  | not $ null undefinedSorts    = Left $ MinError (T.pack ("List of channels with undefined sorts: " ++ show undefinedSorts))
                  | otherwise                    = Right $ ChansDecl l
    where
        nuChans :: [ChanDef]
        nuChans = repeatedByName l

        undefinedSorts :: [ChanDef]
        undefinedSorts = filter (not . (all (memberSort ctx) . toSorts . chanSort ) ) l
