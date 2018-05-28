{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestGNF
(
testGNFList
)
where
import TranslatedProcDefs

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Exception
import Control.Monad
import Test.HUnit

import TxsDefs
import ProcId
import ChanId
import SortId
import qualified Data.Text         as T
import VarId
import ConstDefs
import ValExpr


import LPEfunc
import LPE

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

procIdGen :: String -> [ChanId] -> [VarId] -> ProcId
procIdGen name' chans vars' = ProcId   {  ProcId.name       = T.pack name'
                                        , ProcId.unid       = 111
                                        , ProcId.procchans  = chans
                                        , ProcId.procvars   = vars'
                                        , ProcId.procexit   = NoExit
                                    }

varIdX :: VarId
varIdX = VarId (T.pack "x") 33 intSort
vexprX :: VExpr
vexprX = cstrVar varIdX
vexpr1 :: VExpr
vexpr1 = cstrConst (Cint 1)


-- action: A?x
actOfferAx :: ActOffer
actOfferAx   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Quest varIdX]
                                        }
                        , hiddenvars = Set.empty
                        , constraint = cstrConst (Cbool True)
            }
-- action: A!1
actOfferA1 :: ActOffer
actOfferA1   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Exclam vexpr1]
                                        }
                        , hiddenvars = Set.empty
                        , constraint = cstrConst (Cbool True)
            }

-- action: B!1
actOfferB1 :: ActOffer
actOfferB1   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Exclam vexpr1]
                                        }
                        , hiddenvars = Set.empty
                        , constraint = cstrConst (Cbool True)
            }

-- action: B?x
actOfferBx :: ActOffer
actOfferBx   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Quest varIdX]
                                        }
                        , hiddenvars = Set.empty
                        , constraint = cstrConst (Cbool True)
            }

-- sorts, chanIds
intSort :: SortId
intSort = SortId {  SortId.name = T.pack "Int"
                  , SortId.unid = 1}

chanIdA :: ChanId
chanIdA = ChanId    { ChanId.name = T.pack "A"
                    , ChanId.unid = 2
                    , ChanId.chansorts = [intSort]
                    }
chanIdB :: ChanId
chanIdB = ChanId    { ChanId.name = T.pack "B"
                    , ChanId.unid = 3
                    , ChanId.chansorts = [intSort]
                    }

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

-- check that things have been translated to pregnfFunc first
-- e.g. choice lower in the hierarchy is put in new ProcDef
-- P[A]() = A?x >-> ( (A!1 >->STOP) ## (A?x >->STOP) )
-- becomes
  -- P[A]()  = A?x >-> P$pre1[A](x)
  -- P$pre1[A](x) = (A!1 >->STOP) ## (A?x >->STOP)
testPreGNFFirst :: Test
testPreGNFFirst = TestCase $
   assertBool "choice (on lower level) is substituted" $ eqProcDefs procDefs'' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP   = procIdGen "P" [chanIdA] []
      choice'   = choice $ Set.fromList [actionPref actOfferA1 stop, actionPref actOfferAx stop]
      procDefP = ProcDef [chanIdA] [] (actionPref actOfferAx choice')

      procIdPpre1x = procIdGen "P$pre1" [chanIdA] [varIdX]
      procInstPpre1 = procInst procIdPpre1x [chanIdA] [vexprX]

      procDefP' = ProcDef [chanIdA] [] (actionPref actOfferAx procInstPpre1)
      procDefPpre1x = ProcDef [chanIdA] [varIdX] choice'


      procDefs' = Map.fromList  [ (procIdP, procDefP) ]
      procDefs'' = Map.fromList [ (procIdP, procDefP')
                                , (procIdPpre1x, procDefPpre1x) 
                                ]


-- Stop remains unchanged
testStop :: Test
testStop = TestCase $
    let procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [varIdX] stop)]
        procIdP = procIdGen "P" [chanIdA] [varIdX]
    in  assertBool "STOP"  $ eqProcDefs procDefs' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')

-- A?X >-> STOP remains unchanged
testASeqStop :: Test
testASeqStop = TestCase $
    let procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [] (actionPref actOfferAx stop))]
        procIdP = procIdGen "P" [chanIdA] [varIdX]
    in  assertBool "STOP"  $ eqProcDefs procDefs' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')

-- P[]() := A?x >-> P[A](x) remains unchanged
-- also checks that there are no infinite loops of valid gnfFunc translations
-- checks for impossible gnfFunc translations, see below
testASeqProcInst :: Test
testASeqProcInst = TestCase $
    let procIdP = procIdGen "P" [chanIdA] [varIdX]
        procInstP = procInst procIdP [chanIdA] [vexprX]
        procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [varIdX] (actionPref actOfferAx procInstP))]
    in  assertBool "STOP"  $ eqProcDefs procDefs' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')



-- P[A,B]() := A?x >-> B!1 >-> STOP is split, becomes:
-- P[A,B]() := A?x >-> P$gnf1[A,B](x)
-- P$gnf1[A,B](x) := B!1 >-> STOP
testActPrefSplit :: Test
testActPrefSplit = TestCase $
   assertBool "multi action sequence is split"  $ eqProcDefs procDefs'' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procDefP = ProcDef [chanIdA, chanIdB] [] (actionPref actOfferAx (actionPref actOfferB1 stop))


      procIdPgnf1 = procIdGen "P$gnf1" [chanIdA, chanIdB] [varIdX]
      procInstPgnf1x = procInst procIdPgnf1 [chanIdA, chanIdB] [vexprX]

      procDefP' = ProcDef [chanIdA, chanIdB] [] (actionPref actOfferAx procInstPgnf1x)
      procDefPgnf1x = ProcDef [chanIdA, chanIdB] [varIdX] (actionPref actOfferB1 stop)


      procDefs' = Map.fromList  [ (procIdP, procDefP) ]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdPgnf1, procDefPgnf1x) ]


-- /// P[]() is substituted with its definition
-- P[A]() := Q[A]()
-- Q[B]() := B?x >-> STOP
-- becomes
-- P[A]() := A?x >-> STOP
-- Q[B]() := B?x >-> STOP
testProcInst1 :: Test
testProcInst1 = TestCase $
   assertBool "procInst is substituted"  $ eqProcDefs procDefs'' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (procInst procIdQ [chanIdA] [])
      procDefQ = ProcDef [chanIdB] [] (actionPref actOfferBx stop)


      procDefP' = ProcDef [chanIdA] [] (actionPref actOfferAx stop)

      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdQ, procDefQ) ]


-- P[A]() := (A!1 >-> STOP) ## Q[A]()
-- Q[A]() := A?X >-> STOP
-- becomes
-- P[A]() := (A!1 >-> STOP) ## (A?X >-> STOP)
-- Q[A]() := A?X >-> STOP
testProcInst2 :: Test
testProcInst2 = TestCase $
   assertBool "procInst is substituted 2"  $ eqProcDefs procDefs'' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (choice $ Set.fromList [actionPref actOfferA1 stop, procInst procIdQ [chanIdA] []])
      procDefQ = ProcDef [chanIdA] [] (actionPref actOfferAx stop)


      procDefP' = ProcDef [chanIdA] [] (choice $ Set.fromList [actionPref actOfferA1 stop, actionPref actOfferAx stop])

      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdQ, procDefQ) ]


-- P[A]()  := Q[A]() ## (A!1 >-> STOP)
-- Q[A]()  := R[A]()
-- R[A]()  := A?x -> STOP
-- becomes
-- P[A]()  := (A?x >-> STOP) ## (A!1 >-> STOP)
-- Q[A]()  := A?x -> STOP
-- R[A]()  := A?x -> STOP
testProcInst3 :: Test
testProcInst3 = TestCase $
   assertBool "procInst is substituted 3"  $ eqProcDefs procDefs'' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] []
      procIdR = procIdGen "R" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (choice $ Set.fromList [procInst procIdQ [chanIdA] [], actionPref actOfferA1 stop])
      procDefQ = ProcDef [chanIdA] [] (procInst procIdR [chanIdA] [])
      procDefR = ProcDef [chanIdA] [] (actionPref actOfferAx stop)

      procDefP' = ProcDef [chanIdA] [] (choice $ Set.fromList [actionPref actOfferAx stop, actionPref actOfferA1 stop])
      procDefQ' = ProcDef [chanIdA] [] (actionPref actOfferAx stop)


      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)
                                , (procIdR, procDefR)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdQ, procDefQ')
                                , (procIdR, procDefR) ]



-- choice substitution is normalised!
--  P[]() := (A!1 >-> STOP) ## Q[]()
--  Q[]() := (A?x >-> STOP) ## (A!1 >-> P[]())
--  becomes:
--  P[]() := (A!1 >-> STOP) ## (A?x >-> STOP) ## (A!1 >-> P[]())
--      NOTE the normalisation: choice is not nested after the substitution:
--      (A!1 >-> STOP) ## ((A?x >-> STOP) ## (A!1 >-> P[]())) became (A!1 >-> STOP) ## (A?x >-> STOP) ## (A!1 >-> P[]())
--      otherwise it would not be in pregnfFunc (and thus not GNF) after the substitution!
testProcInst4 :: Test
testProcInst4 = TestCase $
   assertBool "procInst is substituted and normalised"  $ eqProcDefs procDefs'' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP = procIdGen "P" [] []
      procIdQ = procIdGen "Q" [] []
      procDefP = ProcDef [] [] (choice $ Set.fromList [actionPref actOfferA1 stop, procInst procIdQ [] []])
      procDefQ = ProcDef [] [] (choice $ Set.fromList [actionPref actOfferAx stop, actionPref actOfferA1 (procInst procIdP [] [])])


      procDefP' = ProcDef [] [] (choice $ Set.fromList [actionPref actOfferA1 stop
                                        , actionPref actOfferAx stop
                                        , actionPref actOfferA1 (procInst procIdP [] [])
                                        ])

      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdQ, procDefQ) ]






-- no name clash of newly created ProcDefs by pregnfFunc and GNF:
-- P[A,B]() := A?x >-> B!1 >-> ( (A!1 >->STOP) ## (A?x >->STOP) )
-- becomes after preGNF:
-- P[A,B]() := A?x >-> B!1 >-> P$pre1[A,B](x)
-- P$pre1[A,B](x) := STOP ## STOP

-- becomes after GNF:
-- P[A,B]() := A?x >-> P$gnf1(x)
-- P$gnf1[A,B](x) := B!1 >-> P$pre1[A,B](x)
-- P$pre1[A,B](x) := (A!1 >->STOP) ## (A?x >->STOP)
testNamingClash :: Test
testNamingClash = TestCase $
   assertBool "pregnfFunc / gnfFunc naming of new ProcDefs doesn't clash"  $ eqProcDefs procDefs'' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procIdPgnf1 = procIdGen "P$gnf1" [chanIdA, chanIdB] [varIdX]
      procIdPpre1 = procIdGen "P$pre1" [chanIdA, chanIdB] [varIdX]
      choice'= choice $ Set.fromList [actionPref actOfferA1 stop, actionPref actOfferAx stop]
      
      procDefP = ProcDef [chanIdA, chanIdB] [] (actionPref actOfferAx (actionPref actOfferB1 choice'))

      procDefP' = ProcDef [chanIdA, chanIdB] [] (actionPref actOfferAx (procInst procIdPgnf1 [chanIdA, chanIdB] [vexprX]))
      procDefPgnf1 = ProcDef [chanIdA, chanIdB] [varIdX] (actionPref actOfferB1 (procInst procIdPpre1 [chanIdA, chanIdB] [vexprX]))
      procDefPpre1 = ProcDef [chanIdA, chanIdB] [varIdX] choice'

      procDefs' = Map.fromList  [ (procIdP, procDefP) ]

      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdPgnf1, procDefPgnf1)
                                , (procIdPpre1, procDefPpre1) ]


assertException :: (Exception e, Eq e) => e -> ProcDefs -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        _ <- evaluate action
        assertFailure $ "Expected exception: " ++ show ex
    where isWanted = Control.Monad.guard . (== ex)

-- assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
-- assertException ex action =
--     handleJust isWanted (const $ return ()) $ do
--         _ <- action
--         assertFailure $ "Expected exception: " ++ show ex
--     where isWanted = Control.Monad.guard . (== ex)


-- cycle detection
--  P[]() := P[]()
-- should fail
testLoop1 :: Test
testLoop1 = TestCase $
   -- result = gnfFunc procIdP emptyTranslatedProcDefs procDefs
--    let (result, err) = gnfFunc procIdP emptyTranslatedProcDefs procDefs in
--    assertBool "loop 1" err "loop (GNF) detected in P"
--    assertFailure "found a no-progress loop" (gnfFunc procIdP emptyTranslatedProcDefs procDefs)
--    assertException (ErrorCall "found a no-progress loop") (evaluate $ gnfFunc procIdP emptyTranslatedProcDefs procDefs)
--    assertRaises "desc error..." (ErrorCall "found a no-progress loop") (evaluate $ gnfFunc procIdP emptyTranslatedProcDefs procDefs)
    -- assertBool "loop 1"  $ eqProcDefs procDefs' (gnfFunc procIdP emptyTranslatedProcDefs procDefs)
    assertException (ErrorCall "found a no-progress loop") (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
    where
      procIdP = procIdGen "P" [] []
      procDefP = ProcDef [] [] (procInst procIdP [] [])

      procDefs' = Map.fromList  [  (procIdP, procDefP)]

-- cycle detection
--  P[]() :=     A >-> P[]()
--            ## P[]()
-- should fail
testLoop2 :: Test
testLoop2 = TestCase $
   assertBool "loop 2"  $ eqProcDefs procDefs'' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP = procIdGen "P" [] []
      procDefP = ProcDef [] [] (choice [
                                  (actionPref actOfferAx (procInst procIdP [] [])),
                                  (procInst procIdP [] [])
                                  ])

      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procDefs'' = procDefs'


-- cycle detection
--  P[]() :=     A >-> P[]()
--            ## Q[]()
--  Q[]() :=  P[]()
-- should fail
testLoop3 :: Test
testLoop3 = TestCase $
    assertBool "loop 3"  $ eqProcDefs procDefs'' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
    where
      procIdP = procIdGen "P" [] []
      procIdQ = procIdGen "Q" [] []
      procDefP = ProcDef [] [] (choice [
                                  (actionPref actOfferAx (procInst procIdP [] [])),
                                  (procInst procIdQ [] [])
                                  ])
      procDefQ = ProcDef [] [] (procInst procIdP [] [])
      procDefs' = Map.fromList  [  (procIdP, procDefP),
                                  (procIdQ, procDefQ)]
      procDefs'' = procDefs'






----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testGNFList :: Test
testGNFList = TestList [  
                          TestLabel "pregnfFunc translation is executed first" testPreGNFFirst
                        , TestLabel "Stop is unchanged" testStop
                        , TestLabel "A >-> STOP remains the same" testASeqStop
                        , TestLabel "A >-> P[]() remains the same" testASeqProcInst
                        , TestLabel "multi action sequence is split" testActPrefSplit
                        , TestLabel "procInst is substituted" testProcInst1
                        , TestLabel "procInst is substituted 2" testProcInst2
                        , TestLabel "procInst is substituted 3" testProcInst3
                        , TestLabel "procInst is substituted 4" testProcInst4

                        , TestLabel "pregnfFunc / gnfFunc naming of new ProcDefs doesn't clash" testNamingClash
                        , TestLabel "loop 1" testLoop1
                        , TestLabel "loop 2" testLoop2
                        , TestLabel "loop 3" testLoop3
                      ]
