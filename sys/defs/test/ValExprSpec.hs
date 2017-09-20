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


sum01True :: ValExpr Int
sum01True = cstrSum $ fromMultiplierList  [ (cstrConst (Cint 0), 1)
                                       , (cstrConst (Cint 1), 1)
                                       , (cstrConst (Cbool True), 1)]

sum1True :: ValExpr Int
sum1True = cstrSum $ fromMultiplierList  [ (cstrConst (Cint 1), 1)
                                         , (cstrConst (Cbool True), 1)]

spec :: Spec
spec =
  describe "cstrSum examples" $ do
    it "Transforms (0 + 1) + 2 into (0 + 1 + 2)" $
      cstrSum (fromMultiplierList [(cstrConst (Cint 0), 1), (sum12, 1)]) `shouldBe` sum012
    it "Does not allow to sum Integers and Booleans" $
      -- TODO: this shouldn't even compile!
      cstrSum (fromMultiplierList [(cstrConst (Cint 0), 1), (sum1True, 1)]) `shouldBe` sum01True
