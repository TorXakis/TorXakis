{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -Wall -Werror #-}
module ValExprSpec where

import           Sum
import           Test.Hspec
import           TxsDefs

sum012 :: ValExpr Int
sum012 = cstrSum [ SumTerm (cstrConst (Cint 0))
                 , SumTerm (cstrConst (Cint 1))
                 , SumTerm (cstrConst (Cint 2))
                 ]

sum12 :: ValExpr Int
sum12 = cstrSum [ SumTerm (cstrConst (Cint 1))
                , SumTerm (cstrConst (Cint 2))
                ]

spec :: Spec
spec =
  describe "cstrSum examples" $
    it "Transforms (0 + 1) + 2 into (0 + 1 + 2)" $
      cstrSum [SumTerm (cstrConst (Cint 0)), SumTerm sum12]
      `shouldBe` sum012