{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module NameGen where

import           Test.QuickCheck
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T

import Name

newtype ArbitraryName = ArbitraryName { arbitraryName :: Name }

arbitraryPrintableString :: Gen String
arbitraryPrintableString = getPrintableString <$> arbitrary

instance Arbitrary ArbitraryName where
    arbitrary = do
        s:str <- listOf1 arbitraryPrintableChar
        return $ ArbitraryName $ fromNonEmpty (s :| str)
