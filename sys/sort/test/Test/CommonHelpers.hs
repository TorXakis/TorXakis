{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.CommonHelpers
where
-- test specific Haskell imports
import Test.HUnit

-- test specific TorXakis imports

-- generic Haskell imports
-- import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import           Data.String (IsString (..))
import qualified Data.Text as T

-- generic TorXakis imports
import Name
-- import Ref
-- import Sort.ADTDefs
-- import Sort.ConstructorDefs
-- import Sort.ConvertsTo
import Sort.FieldDefs

mkFieldDefs :: [FieldDef v] -> FieldDefs v
mkFieldDefs fs = FieldDefs fs $ length fs

fieldNoMeta :: Name -> v -> FieldDef v
fieldNoMeta n s = FieldDef n s T.empty

instance IsString Name where
    fromString s = n
        where Right n = name $ T.pack s

assertUnorderedEqual :: (Ord a, Show a) => String -> [a] -> [a] -> Assertion
assertUnorderedEqual msg l1 l2 = assertEqual msg (Set.fromList l1) (Set.fromList l2)