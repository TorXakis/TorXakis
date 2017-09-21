{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
{-# OPTIONS -Wall -Werror #-}
module ValExprSpec where

import           Sum
import           Test.Hspec
import           TxsDefs

sum012 :: ValExpr Int
sum012 = cstrSum $ fromMultiplierList  [ (cstrConst (Cint 0), 1)
                                       , (cstrConst (Cint 1), 1)
                                       , (cstrConst (Cint 2), 1)]

sum12 :: ValExpr Int
sum12 = cstrSum $ fromMultiplierList  [ (cstrConst (Cint 1), 1)
                                      , (cstrConst (Cint 2), 1)]

spec :: Spec
spec =
  describe "cstrSum examples" $
    it "Transforms (0 + 1) + 2 into (0 + 1 + 2)" $
      cstrSum (fromMultiplierList [(cstrConst (Cint 0), 1), (sum12, 1)])
      `shouldBe` sum012
