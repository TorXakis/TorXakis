{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Sort.FieldDefsGen where

import           Control.Monad.State
import           Test.QuickCheck
import           QuickCheck.GenT

import           Name
import           Sort

import           NameGen

-- newtype ArbitraryFieldDef = ArbitraryFieldDef
--     { arbitraryFieldDef :: FieldDef Name }

-- instance Arbitrary ArbitraryFieldDef where
--     arbitrary = do
--         ArbitraryName fn <- arbitrary
--         ArbitraryName ss <- arbitrary
--         return $ ArbitraryFieldDef $ FieldDef fn ss

-- newtype ArbitraryFieldDefs = ArbitraryFieldDefs
--     { arbitraryFieldDefs :: FieldDefs Name }

-- instance Arbitrary ArbitraryFieldDefs 

arbitraryFieldDefs :: GenT (State ([ADTDef Name], [FieldDef Name])) (ConstructorDefs Name)
arbitraryFieldDefs = undefined
