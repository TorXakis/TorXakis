{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestPreGNF
(
testPreGNFList
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
import VarId
import qualified Data.Text         as T
import ConstDefs
import ValExpr

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
vexprY = cstrVar varIdY
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

-- action: B?y
actOfferBy   = ActOffer {  offers = Set.singleton(
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Quest varIdY]
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

-- Stop remains unchanged
testStop :: Test
testStop = TestCase $
    let procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [varIdX] Stop)]
        procIdP = procIdGen "P" [chanIdA] [varIdX]
    in  assertEqual "STOP" procDefs (preGNF procIdP emptyTranslatedProcDefs procDefs)

-- action prefix remains unchanged
testActPref :: Test
testActPref = TestCase $
    let procIdP = procIdGen "P" [chanIdA] [varIdX]
        procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [varIdX] (ActionPref actOfferAx Stop))]
    in  assertEqual "A?x >-> STOP" procDefs (preGNF procIdP emptyTranslatedProcDefs procDefs)

testActPref2 :: Test
testActPref2 = TestCase $
   let procIdP = procIdGen "P" [chanIdA] [varIdX]
       procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [varIdX] (ActionPref actOfferAx (ActionPref actOfferB1 Stop)))]
   in  assertEqual "A?x >-> B!1 >-> STOP" procDefs (preGNF procIdP emptyTranslatedProcDefs procDefs)

-- action prefix is translated recursively
-- P[A]() = A?x >-> Q[A]()
-- Q[A]() = A?x >-> (STOP ## STOP)
-- becomes
-- P[A]() = A?x >-> Q[A]()
-- Q[A]() = A?x >-> Q$pre1[A](x)
-- Q$pre1[A](x) = STOP ## STOP
testActPref3 :: Test
testActPref3 = TestCase $
   assertEqual "ActionPref is translated recursively" procDefs' (preGNF procIdP emptyTranslatedProcDefs procDefs)
   where
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] []

      procDefP = ProcDef [chanIdA] [] (ActionPref actOfferAx (ProcInst procIdQ [chanIdA] []))
      procDefQ = ProcDef [chanIdA] [] (ActionPref actOfferAx (Choice [Stop, Stop]))


      procIdQpre1 = procIdGen "Q$pre1" [chanIdA] [varIdX]
      procDefQ' = ProcDef [chanIdA] [] (ActionPref actOfferAx (ProcInst procIdQpre1 [chanIdA] [vexprX]))
      procDefQpre1 = ProcDef [chanIdA] [varIdX] (Choice [Stop, Stop])

      procDefs = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procDefs' = Map.fromList  [ (procIdP, procDefP)
                                , (procIdQ, procDefQ')
                                , (procIdQpre1, procDefQpre1) ]

-- process instance remains unchanged in preGNF
testProcInst :: Test
testProcInst = TestCase $
   let procIdP = procIdGen "P" [chanIdA] []
       procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [] (ProcInst procIdP [chanIdA] []))]
   in  assertEqual "P[]()" procDefs (preGNF procIdP emptyTranslatedProcDefs procDefs)

-- choices at top-level remain unchanged
testChoice1 :: Test
testChoice1 = TestCase $
   let procIdP = procIdGen "P" [chanIdA] []
       procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [] (Choice [Stop, Stop]))]
   in  assertEqual "Stop ## Stop" procDefs (preGNF procIdP emptyTranslatedProcDefs procDefs)

-- choices at top-level remain unchanged
testChoice2 :: Test
testChoice2 = TestCase $
   let procIdP = procIdGen "P" [chanIdA] []
       procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [] (Choice [Stop, Stop, Stop]))]
   in  assertEqual "Stop ## Stop ## Stop" procDefs (preGNF procIdP emptyTranslatedProcDefs procDefs)


-- choices at a lower level are substituted with a process instance to a
-- process definition that is created for exactly the substituted term
-- P[A]() = A?x >-> (STOP ## STOP)
-- becomes
  -- P[A]()  = A?x >-> P$pre1[A](x)
  -- P$pre1[A](x) = STOP ## STOP
testChoice3 :: Test
testChoice3 = TestCase $
   assertEqual "choice (on lower level) is substituted" procDefs' (preGNF procIdP emptyTranslatedProcDefs procDefs)
   where
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (ActionPref actOfferAx (Choice [Stop, Stop]))

      procIdPpre1 = procIdGen "P$pre1" [chanIdA] [varIdX]
      procDefP' = ProcDef [chanIdA] [] (ActionPref actOfferAx (ProcInst procIdPpre1 [chanIdA] [vexprX]))
      procDefPpre1 = ProcDef [chanIdA] [varIdX] (Choice [Stop, Stop])

      procDefs = Map.fromList  [  (procIdP, procDefP)]
      procDefs' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdPpre1, procDefPpre1) ]


-- choices at a lower level are substituted with a process instance to a
-- process definition that is created for exactly the substituted term
-- SAME AS ABOVE, only nested choice is second expression of a choice
-- P[A]() =     STOP
--      ## A?x >-> (STOP ## STOP)
-- becomes
  -- P[A]() =            STOP
  --                  ## A?x >-> P$pre2[A](x)
  -- P$pre2[A](x) =   STOP ## STOP
testChoice4 :: Test
testChoice4 = TestCase $
   assertEqual "choice (on lower level) is substituted 2" procDefs' (preGNF procIdP emptyTranslatedProcDefs procDefs)
   where
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (Choice [Stop, ActionPref actOfferAx (Choice [Stop, Stop])])

      procIdPpre2 = procIdGen "P$pre2" [chanIdA] [varIdX]
      procInstPpre2 = ProcInst procIdPpre2 [chanIdA] [vexprX]
      procDefP' = ProcDef [chanIdA] [] (Choice [Stop, ActionPref actOfferAx procInstPpre2])
      procDefPpre2 = ProcDef [chanIdA] [varIdX] (Choice [Stop, Stop])

      procDefs = Map.fromList  [  (procIdP, procDefP)]
      procDefs' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdPpre2, procDefPpre2) ]



-- choices nested two levels deep
-- P[A]() = (A?x >-> ((B?y >-> STOP ## STOP) ## STOP)) ## STOP
-- or maybe more readable:
--   P =
--                A?x
--          >->
--                          B?y
--                     >->
--                              STOP
--                          ##
--                              STOP
--                ##
--                     STOP
--    ##
--          STOP

-- becomes:
-- P[A]()               = ( A?x >-> P$pre1[A](x)  ) ## STOP
-- P$pre1[A](x)         = (B?y >-> P$pre1$pre1[A](x,y)) ## STOP
-- P$pre1$pre1[A](x,y)  = STOP ## STOP
testChoice5 :: Test
testChoice5 = TestCase $
   assertEqual "choice (on lower level) is substituted 2" procDefs' (preGNF procIdP emptyTranslatedProcDefs procDefs)
   where
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] bexprP
      bexprP = Choice [ActionPref actOfferAx (
                          Choice [ ActionPref actOfferBy (Choice [Stop,Stop]) ,
                                    Stop]),
                      Stop]

      procIdPpre1 = procIdGen "P$pre1" [chanIdA] [varIdX]
      procIdPpre1pre1 = procIdGen "P$pre1$pre1" [chanIdA] [varIdX, varIdY]
      procInstPpre1 = ProcInst procIdPpre1 [chanIdA] [vexprX]

      procDefP' = ProcDef [chanIdA] [] (Choice [ActionPref actOfferAx procInstPpre1,
                                                Stop])

      procInstPpre1pre1 = ProcInst procIdPpre1pre1 [chanIdA] [vexprX, vexprY]
      procDefPpre1 = ProcDef [chanIdA] [varIdX] (Choice [ActionPref actOfferBy procInstPpre1pre1,
                                                         Stop])
      procDefPpre1pre1 = ProcDef [chanIdA] [varIdX, varIdY] (Choice [Stop, Stop])

      procDefs = Map.fromList  [ (procIdP,procDefP) ]
      procDefs' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdPpre1, procDefPpre1)
                                , (procIdPpre1pre1, procDefPpre1pre1)]

----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testPreGNFList :: Test
testPreGNFList = TestList [ TestLabel "Stop is unchanged" testStop
                          , TestLabel "Single ActionPref is unchanged " testActPref
                          , TestLabel "Multi ActionPref is unchanged" testActPref2
                          , TestLabel "ActionPref is translated recursively" testActPref3
                          , TestLabel "ProcInst is unchanged" testProcInst
                          , TestLabel "Choice2 at top-level is unchanged" testChoice1
                          , TestLabel "Choice3 at top-level is unchanged 2" testChoice2
                          , TestLabel "choice at lower level is substituted" testChoice3
                          , TestLabel "choice at lower level is substituted 2" testChoice4
                          , TestLabel "choice at lower level is substituted (recursively)" testChoice5
                         ]
