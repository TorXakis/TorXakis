{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Sort.ConvertsTo where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import           Name
import           Ref

class ConvertsTo a a' where
    convertTo :: a -> a'

instance ConvertsTo Name Name where
    convertTo = id

instance ConvertsTo a a' => ConvertsTo [a] [a'] where
    convertTo = map convertTo

instance ConvertsTo a b => ConvertsTo (Ref a) (Ref b) where
    convertTo (RefByName nm) = RefByName nm

instance (Referencable a', ConvertsTo a a') => ConvertsTo [a] (HashMap (Ref a') a') where
    convertTo as =
        Map.fromList
        $ map ((\a' -> (mkRef a', a')) . convertTo) as
