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
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.Chan.ChansDecl
( ChansDecl
, mkChansDecl
, toList
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           GHC.Generics        (Generic)

import           TorXakis.Chan.ChanDef
import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.SortContext

-- | Data for a channels declarations.
--   The order of the channels declarations is relevant.
newtype ChansDecl = ChansDecl { -- | toList
                                toList :: [ChanDef]
                        }
         deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | smart constructor for ChansDecl
-- Error is returned when
--
-- * Any Sort of the channels is not defined within context.
--
-- * Channel Names are not unique.
--
mkChansDecl :: SortContext c => c -> [ChanDef] -> Either Error ChansDecl
mkChansDecl ctx l | not $ null nuChans           = Left $ Error ("Non unique names: " ++ show nuChans)
                  | not $ null undefinedSorts    = Left $ Error ("List of channels with undefined sorts: " ++ show undefinedSorts)
                  | otherwise                    = Right $ ChansDecl l
    where
        nuChans :: [ChanDef]
        nuChans = repeatedByName l

        undefinedSorts :: [ChanDef]
        undefinedSorts = filter (not . (all (`memberSort` ctx) . toSorts . chanSort ) ) l

instance UsedNames ChansDecl where
    usedNames (ChansDecl l) = usedNames l

instance UsedSorts c ChansDecl where
    usedSorts ctx (ChansDecl l) = usedSorts ctx l
