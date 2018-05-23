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

instance Arbitrary GenValExprBool where
   arbitrary = elements [ GenValExprBool valExprTrue
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

newtype GenChanId = GenChanId ChanId
  deriving (Eq, Ord, Show)

instance Arbitrary GenChanId where
   arbitrary = elements [ GenChanId chanIdExit
                        , GenChanId chanIdA
                        , GenChanId chanIdB
                        ]

newtype GenChanOffer = GenChanOffer ChanOffer
  deriving (Eq, Ord, Show)

instance Arbitrary GenChanOffer where
   arbitrary = elements [ GenChanOffer $ Exclam (cstrConst (Cint 1))
                        , GenChanOffer $ Quest varIdx
                        , GenChanOffer $ Exclam (cstrVar varIdy)
                        ]

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

instance Arbitrary GenOffer where
   arbitrary = elements [ GenOffer offerExit
                        , GenOffer offerA   
                        , GenOffer offerB1  
                        , GenOffer offerBx  
                        ]


newtype GenActOffer = GenActOffer ActOffer
  deriving (Eq, Ord, Show)

instance Arbitrary GenActOffer where
   arbitrary = do
                GenValExprBool vexpr <- arbitrary
                soffers <- arbitrary :: Gen (Set.Set GenOffer)
                let offers' = Set.map (\(GenOffer o) -> o) soffers
                return (GenActOffer $ ActOffer offers' Set.empty vexpr)
 

newtype GenBExpr = GenBExpr BExpr
  deriving (Eq, Ord, Show)

instance Arbitrary GenBExpr where
   arbitrary = genBExpr

      
genBExpr :: Gen GenBExpr
genBExpr = sized genBExpr'
    where
        genBExpr' :: Int -> Gen GenBExpr
        genBExpr' 0 = genStop
        genBExpr' n | n > 0 = oneof [ genStop
                                    , genActionPref
                                    , genGuard
                                    , genChoice
                                    , genParallel
                                    , genEnable
                                    , genDisable
                                    ]
        genBExpr' n = error $ "Unexpected negative size value for genBExpr' : " ++ show n

        genStop :: Gen GenBExpr
        genStop = return $ GenBExpr stop
        
        genActionPref :: Gen GenBExpr
        genActionPref = do
                            n <- getSize
                            GenActOffer ao <- arbitrary
                            GenBExpr bexpr <- resize (n-1) arbitrary
                            return (GenBExpr $ actionPref ao bexpr)

        genGuard :: Gen GenBExpr
        genGuard = do
                        n <- getSize
                        GenValExprBool vexpr <- arbitrary
                        GenBExpr bexpr <- resize (n-1) arbitrary
                        return (GenBExpr $ guard vexpr bexpr)

        genChoice :: Gen GenBExpr
        genChoice = GenBExpr . choice . Set.fromList <$> genListBExpr

        genParallel :: Gen GenBExpr
        genParallel = do
                        gChanIds <- arbitrary :: Gen (Set.Set GenChanId)
                        let chanIds = Set.map (\(GenChanId chanId) -> chanId) gChanIds
                        bexprs <- genListBExpr
                        return (GenBExpr $ parallel chanIds bexprs)
                        
        genEnable :: Gen GenBExpr
        genEnable = do
                        n <- getSize
                        GenBExpr b1 <- resize (n `div` 2) arbitrary
                        GenBExpr b2 <- resize (n `div` 2) arbitrary
                        coffers <- listOf arbitrary
                        let offers' = map (\(GenChanOffer o) -> o) coffers
                        return (GenBExpr $ enable b1 offers' b2)
        
        genDisable :: Gen GenBExpr
        genDisable = do
                        n <- getSize
                        GenBExpr b1 <- resize (n `div` 2) arbitrary
                        GenBExpr b2 <- resize (n `div` 2) arbitrary
                        return (GenBExpr $ disable b1 b2)

        genListBExpr :: Gen [BExpr]
        genListBExpr = do
                        n <- getSize
                        k <- choose (0, n)
                        gBexprs <- vectorOf k $ resize (n-k) arbitrary
                        return $ map (\(GenBExpr bexpr) -> bexpr) gBexprs

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
-- |  A?x [[ False ]] >-> p <==> stop
prop_ActionPrefixConditionFalseEqualsStop :: Set.Set GenOffer -> GenBExpr -> Bool
prop_ActionPrefixConditionFalseEqualsStop soffers (GenBExpr p) =
    let offers' = Set.map (\(GenOffer o) -> o) soffers in
        stop == actionPref (ActOffer offers' Set.empty valExprFalse) p

-- | [[ True ]] =>> p <==> p
prop_GuardTrue :: GenBExpr -> Bool
prop_GuardTrue (GenBExpr p) =
    p == guard valExprTrue p

-- |  [[ False ]] =>> p <==> stop
prop_GuardFalse :: GenBExpr -> Bool
prop_GuardFalse (GenBExpr p) =
    stop == guard valExprFalse p

-- |  [[ c ]] =>> stop <==> stop
prop_GuardStop :: GenValExprBool -> Bool
prop_GuardStop (GenValExprBool vexpr) =
    stop == guard vexpr stop

-- |  [[ c1 ]] =>> A?x [[ c2 ]] >-> p <==> A?x [[ c1 /\ c2 ]] >-> p
prop_GuardActionPrefix :: GenValExprBool -> GenValExprBool -> Set.Set GenOffer -> GenBExpr -> Bool
prop_GuardActionPrefix (GenValExprBool vexpr1) (GenValExprBool vexpr2) soffers (GenBExpr p) =
    let offers' = Set.map (\(GenOffer o) -> o) soffers in
        actionPref (ActOffer offers' Set.empty (cstrAnd (Set.fromList [vexpr1,vexpr2]))) p == 
            guard vexpr1 (actionPref (ActOffer offers' Set.empty vexpr2) p)

-- |  p ## stop <==> p
prop_ChoiceStop :: GenBExpr -> Bool
prop_ChoiceStop (GenBExpr p) =
    p == choice ( Set.fromList [p, stop] )
    
-- |  p ## p <==> p
prop_ChoiceIdempotent :: GenBExpr -> Bool
prop_ChoiceIdempotent (GenBExpr p) =
    p == choice ( Set.fromList [p, p] )

-- |  (p ## q) ## r == p ## (q ## r)
prop_ChoiceAssociative :: GenBExpr -> GenBExpr -> GenBExpr -> Bool
prop_ChoiceAssociative (GenBExpr p) (GenBExpr q) (GenBExpr r) =
    choice ( Set.fromList [ choice ( Set.fromList [p,q] ), r] ) == choice ( Set.fromList [ p, choice ( Set.fromList [q,r] ) ] )

-- |  (p |[G]| q) |[G]| r == p |[G]| (q |[G]| r)
prop_ParallelAssociative :: Set.Set GenChanId -> GenBExpr -> GenBExpr -> GenBExpr -> Bool
prop_ParallelAssociative gChanIds (GenBExpr p) (GenBExpr q) (GenBExpr r) =
    let chanIds = Set.map (\(GenChanId chanId) -> chanId) gChanIds in
        parallel chanIds [ parallel chanIds [p,q], r] == parallel chanIds [ p, parallel chanIds [q,r] ]

-- | stop >>> p <==> stop
prop_EnableStop :: GenBExpr -> [GenChanOffer] -> Bool
prop_EnableStop (GenBExpr p) gChanOffers =
    let chanOffers = map (\(GenChanOffer chanOffer) -> chanOffer) gChanOffers in
        stop == enable stop chanOffers p

-- | stop [>> p <==> p
prop_DisableStopP :: GenBExpr -> Bool
prop_DisableStopP (GenBExpr p) =
    p == disable stop p

-- | p [>> stop <==> p
prop_DisablePStop :: GenBExpr -> Bool
prop_DisablePStop (GenBExpr p) =
    p == disable p stop

return []
testBehExprDefs :: IO Bool
testBehExprDefs = --do
                -- $forAllProperties $ quickCheckWithResult stdArgs { maxSuccess = 100 }
                -- $verboseCheckAll
                --quickCheckWith stdArgs { maxSuccess = 100000 } prop_At
                --quickCheckWith stdArgs { maxSuccess = 100000 } prop_AtValues
                  $quickCheckAll