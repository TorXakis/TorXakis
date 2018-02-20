{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE TypeOperators         #-}
-- {-# LANGUAGE DefaultSignatures     #-}
-- {-# LANGUAGE FlexibleContexts      #-}
module Sort.ConvertsTo where

import           Control.Arrow
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Name
import           Ref
import           Sort.FieldDefs

class ConvertsTo a a' where
    convertTo :: a -> a'

instance ConvertsTo Name Name where
    convertTo = id

instance ConvertsTo a a' => ConvertsTo [a] [a'] where
    convertTo = map convertTo

instance ConvertsTo a a' => ConvertsTo (FieldDef a) (FieldDef a') where
    convertTo (FieldDef n sNm) = FieldDef n (convertTo sNm)

instance ConvertsTo a a' => ConvertsTo (FieldDefs a) (FieldDefs a') where
    convertTo (FieldDefs fs nr) = FieldDefs (convertTo fs) nr

instance ConvertsTo a b => ConvertsTo (Ref a) (Ref b) where
    convertTo (Ref txt) = Ref txt

instance (HasName a', ConvertsTo a a') => ConvertsTo [a] (Map (Ref a') a') where
    convertTo as =
        Map.fromList
        $ map ((\a -> (Ref $ Name.toText $ getName a, a)) . convertTo) as
