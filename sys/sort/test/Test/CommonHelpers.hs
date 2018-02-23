{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.CommonHelpers
where
-- test specific Haskell imports
import Test.HUnit

-- test specific TorXakis imports

-- generic Haskell imports
import qualified Data.Set as Set
import           Data.String (IsString (..))
import qualified Data.Text as T

-- generic TorXakis imports
import Name
import Sort.Internal (FieldDef (..), FieldDefs (..))

instance IsString Name where
    fromString s = n
        where Right n = name $ T.pack s

assertUnorderedEqual :: (Ord a, Show a) => String -> [a] -> [a] -> Assertion
assertUnorderedEqual msg l1 l2 = assertEqual msg (Set.fromList l1) (Set.fromList l2)

mkName :: String -> Name
mkName s = n
    where Right n = name $ T.pack s

fieldNoMeta :: Name -> v -> FieldDef v
fieldNoMeta n s = FieldDef n s T.empty

mkIntField :: Int -> FieldDef Name
mkIntField i = fieldNoMeta (mkName $ "field" ++ show i) "Int"

mkFieldDefs :: [FieldDef v] -> FieldDefs v
mkFieldDefs fs = FieldDefs fs $ length fs
