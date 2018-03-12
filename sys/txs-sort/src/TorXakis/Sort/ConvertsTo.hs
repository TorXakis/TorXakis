{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ConvertsTo
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Damian Nadales <damian.nadalesagut@tno.nl>
--                Kerem Ispirli <kerem.ispirli@tno.nl>
--                Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides ConvertsTo class to enable conversion from one type to
-- another in a common way.
-----------------------------------------------------------------------------
module TorXakis.Sort.ConvertsTo where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Control.Arrow       ((+++), (|||))

import           TorXakis.Sort.Name
import           TorXakis.Sort.Ref

-- | Enables conversion from one type to another in a common way.
class ConvertsTo a a' where
    convertTo :: a -> a'

-- | Converting 'Name' to 'Name' is just id; useful when 'Name' is part of a complex
--   structure that is to be converted.
instance ConvertsTo Name Name where
    convertTo = id

-- | Converting a list of entities of a convertible type to a list of entities
--   of the target type.
instance ConvertsTo a a' => ConvertsTo [a] [a'] where
    convertTo = map convertTo

instance (ConvertsTo a a', ConvertsTo b b') =>
    ConvertsTo (Either a b) (Either a' b') where
    convertTo = convertTo +++ convertTo

instance (ConvertsTo a c, ConvertsTo b c) =>
    ConvertsTo (Either a b) c where
    convertTo = convertTo ||| convertTo    

-- | Converting 'Ref' of a certain convertible type to 'Ref' of the target type,
--   using the same data.
instance ConvertsTo a b => ConvertsTo (Ref a) (Ref b) where
    convertTo (RefByName nm) = RefByName nm

-- | Convert a list of entities of a convertible type to a 'HashMap.HashMap',
--   using entities of the target type as values and 'Ref's to target type as
--   keys. Target type has to be 'Referencable'.
instance (Referencable a', ConvertsTo a a') => ConvertsTo [a] (HashMap (Ref a') a') where
    convertTo as =
        Map.fromList
        $ map ((\a' -> (mkRef a', a')) . convertTo) as


