{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- TODO: make sure these warnings are removed.
-- TODO: also check the hlint warnings!
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TestGNF
(
testGNFList
)
where
import LPE
import TranslatedProcDefs

import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map

import TxsDefs
import TxsShow
import ProcId
import ChanId
import SortId
import qualified Data.Text         as T
import VarId
import ConstDefs
import ValExpr

import LPEfunc

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

procIdGen :: String -> [ChanId] -> [VarId] -> ProcId
procIdGen name chans vars = ProcId   {    ProcId.name       = T.pack name
                                        , ProcId.unid       = 111
                                        , ProcId.procchans  = chans
                                        , ProcId.procvars   = vars
                                        , ProcId.procexit   = NoExit
                                    }

varIdX = VarId (T.pack "x") 33 intSort
varIdY = VarId (T.pack "y") 34 intSort
-- vexprX :: VExpr
vexprX = cstrVar varIdX
vexpr1 = cstrConst (Cint 1)


-- action: A!1
actOfferA1   = ActOffer {  offers = Set.singleton(
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Exclam vexpr1]
                                        })
                        , constraint = cstrConst (Cbool True)
            }

-- action: A?x
actOfferAx   = ActOffer {  offers = Set.singleton(
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Quest varIdX]
                                        })
                        , constraint = cstrConst (Cbool True)
            }

-- action: B!1
actOfferB1   = ActOffer {  offers = Set.singleton(
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Exclam vexpr1]
                                        })
                        , constraint = cstrConst (Cbool True)
            }

-- action: B?x
actOfferBx   = ActOffer {  offers = Set.singleton(
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Quest varIdX]
                                        })
                        , constraint = cstrConst (Cbool True)
            }

-- sorts, chanIds
intSort = SortId {  SortId.name = T.pack "Int"
                  , SortId.unid = 1}

chanIdA = ChanId    { ChanId.name = T.pack "A"
                    , ChanId.unid = 2
                    , ChanId.chansorts = [intSort]
                    }
chanIdB = ChanId    { ChanId.name = T.pack "B"
                    , ChanId.unid = 3
                    , ChanId.chansorts = [intSort]
                    }

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

-- check that things have been translated to pregnfFunc first
-- e.g. choice lower in the hierarchy is put in new ProcDef
-- P[A]() = A?x >-> (STOP ## STOP)
-- becomes
  -- P[A]()  = A?x >-> P$pre1[A](x)
  -- P$pre1[A](x) = STOP ## STOP
testPreGNFFirst :: Test
testPreGNFFirst = TestCase $
   assertBool "choice (on lower level) is substituted" $ eq_procDefs procDefs' (gnfFunc procIdP emptyTranslatedProcDefs procDefs)
   where
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (ActionPref actOfferAx (Choice [Stop, Stop]))


      procIdPpre1x = procIdGen "P$pre1" [chanIdA] [varIdX]
      procInstPpre1 = ProcInst procIdPpre1x [chanIdA] [vexprX]

      procDefP' = ProcDef [chanIdA] [] (ActionPref actOfferAx procInstPpre1)
      procDefPpre1x = ProcDef [chanIdA] [varIdX] (Choice [Stop, Stop])


      procDefs = Map.fromList  [ (procIdP, procDefP) ]
      procDefs' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdPpre1x, procDefPpre1x) ]


-- Stop remains unchanged
testStop :: Test
testStop = TestCase $
    let procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [varIdX] Stop)]
        procIdP = procIdGen "P" [chanIdA] [varIdX]
    in  assertBool "STOP"  $ eq_procDefs procDefs (gnfFunc procIdP emptyTranslatedProcDefs procDefs)

-- A?X >-> STOP remains unchanged
testASeqStop :: Test
testASeqStop = TestCase $
    let procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [] (ActionPref actOfferAx Stop))]
        procIdP = procIdGen "P" [chanIdA] [varIdX]
    in  assertBool "STOP"  $ eq_procDefs procDefs (gnfFunc procIdP emptyTranslatedProcDefs procDefs)

-- P[]() := A?x >-> P[A](x) remains unchanged
-- also checks that there are no infinite loops of valid gnfFunc translations
-- checks for impossible gnfFunc translations, see below
testASeqProcInst :: Test
testASeqProcInst = TestCase $
    let procIdP = procIdGen "P" [chanIdA] [varIdX]
        procInstP = ProcInst procIdP [chanIdA] [vexprX]
        procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [varIdX] (ActionPref actOfferAx procInstP))]
    in  assertBool "STOP"  $ eq_procDefs procDefs (gnfFunc procIdP emptyTranslatedProcDefs procDefs)



-- P[A,B]() := A?x >-> B!1 >-> STOP is split, becomes:
-- P[A,B]() := A?x >-> P$gnf1[A,B](x)
-- P$gnf1[A,B](x) := B!1 >-> STOP
testActPrefSplit :: Test
testActPrefSplit = TestCase $
   assertBool "multi action sequence is split"  $ eq_procDefs procDefs' (gnfFunc procIdP emptyTranslatedProcDefs procDefs)
   where
      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procDefP = ProcDef [chanIdA, chanIdB] [] (ActionPref actOfferAx (ActionPref actOfferB1 Stop))


      procIdPgnf1 = procIdGen "P$gnf1" [chanIdA, chanIdB] [varIdX]
      procInstPgnf1x = ProcInst procIdPgnf1 [chanIdA, chanIdB] [vexprX]

      procDefP' = ProcDef [chanIdA, chanIdB] [] (ActionPref actOfferAx procInstPgnf1x)
      procDefPgnf1x = ProcDef [chanIdA, chanIdB] [varIdX] (ActionPref actOfferB1 Stop)


      procDefs = Map.fromList  [ (procIdP, procDefP) ]
      procDefs' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdPgnf1, procDefPgnf1x) ]


-- /// P[]() is substituted with its definition
-- P[A]() := Q[A]()
-- Q[B]() := B?x >-> STOP
-- becomes
-- P[A]() := A?x >-> STOP
-- Q[B]() := B?x >-> STOP
testProcInst1 :: Test
testProcInst1 = TestCase $
   assertBool "procInst is substituted"  $ eq_procDefs procDefs' (gnfFunc procIdP emptyTranslatedProcDefs procDefs)
   where
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (ProcInst procIdQ [chanIdA] [])
      procDefQ = ProcDef [chanIdB] [] (ActionPref actOfferBx Stop)


      procDefP' = ProcDef [chanIdA] [] (ActionPref actOfferAx Stop)

      procDefs = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procDefs' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdQ, procDefQ) ]


-- P[A]() := STOP ## Q[A]()
-- Q[A]() := A?X >-> STOP
-- becomes
-- P[A]() := STOP ## (A?X >-> STOP)
-- Q[A]() := A?X >-> STOP
testProcInst2 :: Test
testProcInst2 = TestCase $
   assertBool "procInst is substituted 2"  $ eq_procDefs procDefs' (gnfFunc procIdP emptyTranslatedProcDefs procDefs)
   where
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (Choice [Stop, ProcInst procIdQ [chanIdA] []])
      procDefQ = ProcDef [chanIdA] [] (ActionPref actOfferAx Stop)


      procDefP' = ProcDef [chanIdA] [] (Choice [Stop, (ActionPref actOfferAx Stop)])

      procDefs = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procDefs' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdQ, procDefQ) ]


-- P[A]()  := Q[A]() ## STOP
-- Q[A]()  := R[A]()
-- R[A]()  := A?x -> STOP
-- becomes
-- P[A]()  := (A?x >-> STOP) ## STOP
-- Q[A]()  := A?x -> STOP
-- R[A]()  := A?x -> STOP
testProcInst3 :: Test
testProcInst3 = TestCase $
   assertBool "procInst is substituted 3"  $ eq_procDefs procDefs' (gnfFunc procIdP emptyTranslatedProcDefs procDefs)
   where
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] []
      procIdR = procIdGen "R" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (Choice [ProcInst procIdQ [chanIdA] [], Stop])
      procDefQ = ProcDef [chanIdA] [] (ProcInst procIdR [chanIdA] [])
      procDefR = ProcDef [chanIdA] [] (ActionPref actOfferAx Stop)

      procDefP' = ProcDef [chanIdA] [] (Choice [(ActionPref actOfferAx Stop), Stop])
      procDefQ' = ProcDef [chanIdA] [] (ActionPref actOfferAx Stop)


      procDefs = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)
                                , (procIdR, procDefR)]
      procDefs' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdQ, procDefQ')
                                , (procIdR, procDefR) ]



-- choice substitution is normalised!
--  P[]() := STOP ## Q[]()
--  Q[]() := STOP ## STOP
--  becomes:
--  P[]() := STOP ## STOP ## STOP
--      NOTE the normalisation: choice is not nested after the substitution:
--      STOP ## (STOP ## STOP) became  STOP ## STOP ## STOP
--      otherwise it would not be in pregnfFunc (and thus not GNF) after the substitution!
testProcInst4 :: Test
testProcInst4 = TestCase $
   assertBool "procInst is substituted and normalised"  $ eq_procDefs procDefs' (gnfFunc procIdP emptyTranslatedProcDefs procDefs)
   where
      procIdP = procIdGen "P" [] []
      procIdQ = procIdGen "Q" [] []
      procDefP = ProcDef [] [] (Choice [Stop, ProcInst procIdQ [] []])
      procDefQ = ProcDef [] [] (Choice [Stop, Stop])


      procDefP' = ProcDef [] [] (Choice [Stop, Stop, Stop])

      procDefs = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procDefs' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdQ, procDefQ) ]






-- no name clash of newly created ProcDefs by pregnfFunc and GNF:
-- P[A,B]() := A?x >-> B!1 >-> (STOP ## STOP)
-- becomes after preGNF:
-- P[A,B]() := A?x >-> B!1 >-> P$pre1[A,B](x)
-- P$pre1[A,B](x) := STOP ## STOP

-- becomes after GNF:
-- P[A,B]() := A?x >-> P$gnf1(x)
-- P$gnf1[A,B](x) := B!1 >-> P$pre1[A,B](x)
-- P$pre1[A,B](x) := STOP ## STOP
testNamingClash :: Test
testNamingClash = TestCase $
   assertBool "pregnfFunc / gnfFunc naming of new ProcDefs doesn't clash"  $ eq_procDefs procDefs' (gnfFunc procIdP emptyTranslatedProcDefs procDefs)
   where
      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procIdPgnf1 = procIdGen "P$gnf1" [chanIdA, chanIdB] [varIdX]
      procIdPpre1 = procIdGen "P$pre1" [chanIdA, chanIdB] [varIdX]

      procDefP = ProcDef [chanIdA, chanIdB] [] (ActionPref actOfferAx (ActionPref actOfferB1 (Choice [Stop, Stop])))

      procDefP' = ProcDef [chanIdA, chanIdB] [] (ActionPref actOfferAx (ProcInst procIdPgnf1 [chanIdA, chanIdB] [vexprX]))
      procDefPgnf1 = ProcDef [chanIdA, chanIdB] [varIdX] (ActionPref actOfferB1 (ProcInst procIdPpre1 [chanIdA, chanIdB] [vexprX]))
      procDefPpre1 = ProcDef [chanIdA, chanIdB] [varIdX] (Choice [Stop, Stop])

      procDefs = Map.fromList  [ (procIdP, procDefP) ]

      procDefs' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdPgnf1, procDefPgnf1)
                                , (procIdPpre1, procDefPpre1) ]


--
-- -- create monad to run GNF
-- -- return the resulting procDefs and the possible error
-- gnfTestWrapper :: ProcId -> TranslatedProcDefs -> ProcDefs -> (ProcDefs, String)
-- gnfTestWrapper procId translatedProcDefs procDefs =
--   let a = a
--   in (Map.fromList [], "loop (GNF) detected in P")


--
-- -- cycle detection
-- --  P[]() := P[]()
-- -- should fail
-- testLoop1 :: Test
-- testLoop1 = TestCase $
--    -- result = gnfFunc procIdP emptyTranslatedProcDefs procDefs
--    let (result, err) = gnfTestWrapper procIdP emptyTranslatedProcDefs procDefs in
--    assertBool "loop 1" err "loop (GNF) detected in P"
--    where
--       procIdP = procIdGen "P" [] []
--       procDefP = ProcDef [] [] (ProcInst procIdP [] [])
--
--       procDefs = Map.fromList  [  (procIdP, procDefP)]
--
-- -- cycle detection
-- --  P[]() :=     A >-> P[]()
-- --            ## P[]()
-- -- should fail
-- testLoop2 :: Test
-- testLoop2 = TestCase $
--    let (result, err) = gnfTestWrapper procIdP emptyTranslatedProcDefs procDefs in
--    assertBool "loop 2" err "loop (GNF) detected in P"
--    where
--       procIdP = procIdGen "P" [] []
--       procDefP = ProcDef [] [] (Choice [
--                                   (ActionPref actOfferAx (ProcInst procIdP [] [])),
--                                   (ProcInst procIdP [] [])
--                                   ])
--
--       procDefs = Map.fromList  [  (procIdP, procDefP)]
--
--
-- -- cycle detection
-- --  P[]() :=     A >-> P[]()
-- --            ## Q[]()
-- --  Q[]() :=  P[]()
-- -- should fail
-- testLoop3 :: Test
-- testLoop3 = TestCase $
--    let (result, err) = gnfTestWrapper procIdP emptyTranslatedProcDefs procDefs in
--    assertBool "loop 3" err "loop (GNF) detected in P"
--    where
--       procIdP = procIdGen "P" [] []
--       procIdQ = procIdGen "Q" [] []
--       procDefP = ProcDef [] [] (Choice [
--                                   (ActionPref actOfferAx (ProcInst procIdP [] [])),
--                                   (ProcInst procIdQ [] [])
--                                   ])
--       procDefQ = ProcDef [] [] (ProcInst procIdQ [] [])
--       procDefs = Map.fromList  [  (procIdP, procDefP),
--                                   (procIdQ, procDefQ)]
--
--



----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testGNFList :: Test
testGNFList = TestList [  TestLabel "pregnfFunc translation is executed first" testPreGNFFirst
                        , TestLabel "Stop is unchanged" testStop
                        , TestLabel "A >-> STOP remains the same" testASeqStop
                        , TestLabel "A >-> P[]() remains the same" testASeqProcInst
                        , TestLabel "multi action sequence is split" testActPrefSplit
                        , TestLabel "procInst is substituted" testProcInst1
                        , TestLabel "procInst is substituted 2" testProcInst2
                        , TestLabel "procInst is substituted 3" testProcInst3
                        , TestLabel "procInst is substituted 4" testProcInst4

                        , TestLabel "pregnfFunc / gnfFunc naming of new ProcDefs doesn't clash" testNamingClash
                        -- , TestLabel "loop 1" testLoop1
                        -- , TestLabel "loop 2" testLoop2
                        -- , TestLabel "loop 3" testLoop3
                      ]
