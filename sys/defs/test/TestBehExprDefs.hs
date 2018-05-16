{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module TestBehExprDefs
(
testBehExprDefs
)
where
-- test specific Haskell imports
import Test.QuickCheck

-- test specific TorXakis imports


-- generic Haskell imports
import qualified Data.Set as Set

-- generic TorXakis imports
import BehExprDefs
import ChanId
import ConstDefs
import SortId
import StdTDefs
import ValExpr
import VarId

-- variables
varIdx :: VarId
varIdx = VarId "x" 33 sortIdInt

varIdy :: VarId
varIdy = VarId "y" 34 sortIdInt

--valExpr: True
valExprTrue :: ValExpr VarId
valExprTrue = cstrConst (Cbool True)

--valExpr: False
valExprFalse :: ValExpr VarId
valExprFalse = cstrConst (Cbool False)

--valExpr : 0 <= x
valExprGEZx :: ValExpr VarId
valExprGEZx = cstrGEZ $ cstrVar varIdx

--valExpr : x == y
valExprEqualxy :: ValExpr VarId
valExprEqualxy = cstrEqual (cstrVar varIdx) (cstrVar varIdy)

newtype GenValExprBool = GenValExprBool (ValExpr VarId)
  deriving (Eq, Ord, Show)

instance Arbitrary (GenValExprBool) where
   arbitrary = genValExprBool

genValExprBool :: Gen GenValExprBool
genValExprBool = elements [ GenValExprBool valExprTrue
                          , GenValExprBool valExprFalse
                          , GenValExprBool valExprGEZx
                          , GenValExprBool valExprEqualxy
                          ]
    
chanIdA :: ChanId
chanIdA = ChanId { ChanId.name = "A"
                 , ChanId.unid = 2
                 , ChanId.chansorts = []
                 }   

chanIdB :: ChanId
chanIdB = ChanId { ChanId.name = "B"
                 , ChanId.unid = 3
                 , ChanId.chansorts = [sortIdInt]
                 }   
-- offer: A
offerA :: Offer
offerA = Offer { chanid = chanIdA
               , chanoffers = []
               }

-- offer: B!1
offerB1 :: Offer
offerB1 = Offer { chanid = chanIdB
                , chanoffers = [Exclam (cstrConst (Cint 1))]
                }

-- offer: B ? x
offerBx :: Offer
offerBx = Offer { chanid = chanIdB
                , chanoffers = [Quest varIdx]
                }
-- offer: EXIT
offerExit :: Offer
offerExit = Offer chanIdExit []

newtype GenOffer = GenOffer Offer
  deriving (Eq, Ord, Show)

instance Arbitrary (GenOffer) where
   arbitrary = genOffer

genOffer :: Gen GenOffer
genOffer = elements [ GenOffer $ offerExit
                    , GenOffer $ offerA   
                    , GenOffer $ offerB1  
                    , GenOffer $ offerBx  
                    ]


newtype GenActOffer = GenActOffer ActOffer
  deriving (Eq, Ord, Show)

instance Arbitrary (GenActOffer) where
   arbitrary = do
                GenValExprBool vexpr <- arbitrary
                soffers <- arbitrary :: Gen (Set.Set GenOffer)
                let offers' = Set.map (\(GenOffer o) -> o) soffers
                return (GenActOffer $ ActOffer offers' Set.empty vexpr)
 

newtype GenBExpr = GenBExpr BExpr
  deriving (Eq, Ord, Show)

instance Arbitrary (GenBExpr) where
   arbitrary = genBExpr

      
genBExpr :: Gen GenBExpr
genBExpr = sized genBExpr'

genActionPref :: Int -> Gen GenBExpr
genActionPref n = do
                    GenActOffer ao <- arbitrary
                    GenBExpr bexpr <- genBExpr' (n-1)
                    return (GenBExpr $ actionPref ao bexpr)

genGuard :: Int -> Gen GenBExpr
genGuard n = do
                GenValExprBool vexpr <- arbitrary
                GenBExpr bexpr <- genBExpr' (n-1)
                return (GenBExpr $ guard vexpr bexpr)
                    
genBExpr' :: Int -> Gen GenBExpr
genBExpr' 0 = return $ GenBExpr stop
genBExpr' n | n > 0 = oneof [ return $ GenBExpr stop
                            , genActionPref n
                            , genGuard n
                            ]
genBExpr' n = error $ "Unexpected negative size value for genBExpr' : " ++ show n
 
---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
prop_ActionPrefixConditionFalseEqualsStop :: Set.Set GenOffer -> GenBExpr -> Bool
prop_ActionPrefixConditionFalseEqualsStop soffers (GenBExpr bexpr) =
    let offers' = Set.map (\(GenOffer o) -> o) soffers in
        stop == actionPref (ActOffer offers' Set.empty valExprFalse) bexpr

prop_GuardTrue :: GenBExpr -> Bool
prop_GuardTrue (GenBExpr bexpr) =
    bexpr == guard valExprTrue bexpr

prop_GuardFalse :: GenBExpr -> Bool
prop_GuardFalse (GenBExpr bexpr) =
    stop == guard valExprFalse bexpr

prop_GuardStop :: GenValExprBool -> Bool
prop_GuardStop (GenValExprBool vexpr) =
    stop == guard vexpr stop

prop_GuardActionPrefix :: GenValExprBool -> GenValExprBool -> Set.Set GenOffer -> GenBExpr -> Bool
prop_GuardActionPrefix (GenValExprBool vexpr1) (GenValExprBool vexpr2) soffers (GenBExpr bexpr) =
    let offers' = Set.map (\(GenOffer o) -> o) soffers in
        actionPref (ActOffer offers' Set.empty (cstrAnd (Set.fromList [vexpr1,vexpr2]))) bexpr == 
            guard vexpr1 (actionPref (ActOffer offers' Set.empty vexpr2) bexpr)

return []
testBehExprDefs :: IO Bool
testBehExprDefs = --do
                -- $forAllProperties $ quickCheckWithResult stdArgs { maxSuccess = 100 }
                -- $verboseCheckAll
                --quickCheckWith stdArgs { maxSuccess = 100000 } prop_At
                --quickCheckWith stdArgs { maxSuccess = 100000 } prop_AtValues
                  $quickCheckAll