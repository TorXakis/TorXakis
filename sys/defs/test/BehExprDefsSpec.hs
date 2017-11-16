{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS -Wno-orphans #-}
module BehExprDefsSpec where

import           Generic.Random.Generic
import           Test.Hspec
import           Test.QuickCheck

-- Local imports
import           FreeMonoidX
import           Product
import           Sum
import           TxsDefs

-- TODO: remove the need for this package by making 'Name' a 'newtype'.
import           Test.QuickCheck.Instances ()

-- TODO: put all these arbitrary instances in a separate package.
instance Arbitrary BExpr where
    arbitrary = genericArbitrary beWeigths
        where beWeigths :: Weights BExpr
              beWeigths = (50 :: W "Stop")
                        % (0 :: W "ActionPref")
                        % (0 :: W "Guard")
                        % (1 :: W "Choice")
                        % (0 :: W "Parallel")
                        % (0 :: W "Enable")
                        % (0 :: W "Disable")
                        % (0 :: W "Interrupt")
                        % (0 :: W "ProcInst")
                        % (0 :: W "Hide")
                        % (0 :: W "ValueEnv")
                        % (0 :: W "StAut")
                        % ()

instance (Ord v, Arbitrary v) => Arbitrary (ValExpr v) where
    arbitrary = genericArbitraryU

instance (Ord w, Arbitrary w) => Arbitrary (ValExprView w) where
    arbitrary = genericArbitrary veWeights
        where veWeights = (1 :: W "Vconst")
                        % (0 :: W "Vvar")
                        % (0 :: W "Vequal")
                        % (0 :: W "Vite")
                        % (0 :: W "Vnot")
                        % (0 :: W "Vand")
                        % (0 :: W "Vdivide")
                        % (0 :: W "Vmodulo")
                        % (0 :: W "Vsum")
                        % (0 :: W "Vproduct")
                        % (0 :: W "Vgez")
                        % (0 :: W "Vlength")
                        % (0 :: W "Vat")
                        % (0 :: W "Vconcat")
                        % (0 :: W "Vstrinre")
                        % (0 :: W "Vcstr")
                        % (0 :: W "Viscstr")
                        % (0 :: W "Vaccess")
                        % (0 :: W "Vfunc")
                        % (0 :: W "Vpredef")
                        % (0 :: W "Verror")
                        % ()

instance Arbitrary ActOffer where arbitrary = genericArbitraryU

instance Arbitrary VarId where arbitrary = genericArbitraryU

instance Arbitrary ProcId where arbitrary = genericArbitraryU

instance Arbitrary ChanId where arbitrary = genericArbitraryU

instance Arbitrary StatId where arbitrary = genericArbitraryU

instance Arbitrary SortId where arbitrary = genericArbitraryU

instance Arbitrary ExitSort where arbitrary = genericArbitraryU

instance Arbitrary Offer where arbitrary = genericArbitraryU

instance Arbitrary ChanOffer where arbitrary = genericArbitraryU

instance Arbitrary Trans where arbitrary = genericArbitraryU

instance (Ord a, Arbitrary a) => Arbitrary (FreeMonoidX a) where arbitrary = genericArbitraryU

instance Arbitrary a => Arbitrary (SumTerm a) where arbitrary = genericArbitraryU

instance Arbitrary a => Arbitrary (ProductTerm a) where arbitrary = genericArbitraryU

instance Arbitrary Const where arbitrary = genericArbitraryU

instance Arbitrary CstrId where arbitrary = genericArbitraryU

instance Arbitrary FuncId where arbitrary = genericArbitraryU

instance Arbitrary PredefKind where arbitrary = genericArbitraryU

propBExprEq :: BExpr -> Property
propBExprEq bexp = bexp === bexp

spec :: Spec
spec = do
    describe "Equality on behaviour expressions" $
        it "is well-defined" $ property propBExprEq
