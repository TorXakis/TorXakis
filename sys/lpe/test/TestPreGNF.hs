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

import TranslatedProcDefs

import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map

import TxsDefs
import ProcId
import ChanId
import SortId
import VarId
import qualified Data.Text         as T
import ConstDefs
import ValExpr

import LPEfunc

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
varIdY :: VarId
varIdY = VarId (T.pack "y") 34 intSort
vexprX :: VExpr
vexprX = cstrVar varIdX
vexprY :: VExpr
vexprY = cstrVar varIdY
vexpr1 :: VExpr
vexpr1 = cstrConst (Cint 1)


-- action: A?x
actOfferAx :: ActOffer
actOfferAx   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Quest varIdX]
                                              }
                        , constraint = cstrConst (Cbool True)
            }


-- action: B!1
actOfferB1 :: ActOffer
actOfferB1   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Exclam vexpr1]
                                              }
                        , constraint = cstrConst (Cbool True)
            }

-- action: B?y
actOfferBy :: ActOffer
actOfferBy   = ActOffer {  offers = Set.singleton 
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Quest varIdY]
                                              }
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

-- Stop remains unchanged
testStop :: Test
testStop = TestCase $
    let procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [varIdX] stop)]
        procIdP = procIdGen "P" [chanIdA] [varIdX]
    in  assertBool "STOP" $ eqProcDefs  procDefs' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')

-- action prefix remains unchanged
testActPref :: Test
testActPref = TestCase $
    let procIdP = procIdGen "P" [chanIdA] [varIdX]
        procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [varIdX] (actionPref actOfferAx stop))]
    in  assertBool "A?x >-> STOP" $ eqProcDefs  procDefs' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')

testActPref2 :: Test
testActPref2 = TestCase $
   let procIdP = procIdGen "P" [chanIdA] [varIdX]
       procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [varIdX] (actionPref actOfferAx (actionPref actOfferB1 stop)))]
   in  assertBool "A?x >-> B!1 >-> STOP" $ eqProcDefs  procDefs' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')

-- action prefix is translated recursively
-- P[A]() = A?x >-> Q[A]()
-- Q[A]() = A?x >-> (STOP ## STOP)
-- becomes
-- P[A]() = A?x >-> Q[A]()
-- Q[A]() = A?x >-> Q$pre1[A](x)
-- Q$pre1[A](x) = STOP ## STOP
testActPref3 :: Test
testActPref3 = TestCase $
   assertBool "ActionPref is translated recursively" $ eqProcDefs  procDefs'' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] []

      procDefP = ProcDef [chanIdA] [] (actionPref actOfferAx (procInst procIdQ [chanIdA] []))
      procDefQ = ProcDef [chanIdA] [] (actionPref actOfferAx (choice [stop, stop]))


      procIdQpre1 = procIdGen "Q$pre1" [chanIdA] [varIdX]
      procDefQ' = ProcDef [chanIdA] [] (actionPref actOfferAx (procInst procIdQpre1 [chanIdA] [vexprX]))
      procDefQpre1 = ProcDef [chanIdA] [varIdX] (choice [stop, stop])

      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP)
                                , (procIdQ, procDefQ')
                                , (procIdQpre1, procDefQpre1) ]

-- process instance remains unchanged in preGNF
testProcInst :: Test
testProcInst = TestCase $
   let procIdP = procIdGen "P" [chanIdA] []
       procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [] (procInst procIdP [chanIdA] []))]
   in  assertBool "P[]()"  $ eqProcDefs procDefs' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')

-- choices at top-level remain unchanged
testChoice1 :: Test
testChoice1 = TestCase $
   let procIdP = procIdGen "P" [chanIdA] []
       procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [] (choice [stop, stop]))]
   in  assertBool "Stop ## Stop" $ eqProcDefs  procDefs' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')

-- choices at top-level remain unchanged
testChoice2 :: Test
testChoice2 = TestCase $
   let procIdP = procIdGen "P" [chanIdA] []
       procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [] (choice [stop, stop, stop]))]
   in  assertBool "Stop ## Stop ## Stop"  $ eqProcDefs procDefs' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')


-- choices at a lower level are substituted with a process instance to a
-- process definition that is created for exactly the substituted term
-- P[A]() = A?x >-> (STOP ## STOP)
-- becomes
  -- P[A]()  = A?x >-> P$pre1[A](x)
  -- P$pre1[A](x) = STOP ## STOP
testChoice3 :: Test
testChoice3 = TestCase $
   assertBool "choice (on lower level) is substituted" $ eqProcDefs  procDefs'' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (actionPref actOfferAx (choice [stop, stop]))

      procIdPpre1 = procIdGen "P$pre1" [chanIdA] [varIdX]
      procDefP' = ProcDef [chanIdA] [] (actionPref actOfferAx (procInst procIdPpre1 [chanIdA] [vexprX]))
      procDefPpre1 = ProcDef [chanIdA] [varIdX] (choice [stop, stop])

      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
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
   assertBool "choice (on lower level) is substituted 2"  $ eqProcDefs procDefs'' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (choice [stop, actionPref actOfferAx (choice [stop, stop])])

      procIdPpre2 = procIdGen "P$pre2" [chanIdA] [varIdX]
      procInstPpre2 = procInst procIdPpre2 [chanIdA] [vexprX]
      procDefP' = ProcDef [chanIdA] [] (choice [stop, actionPref actOfferAx procInstPpre2])
      procDefPpre2 = ProcDef [chanIdA] [varIdX] (choice [stop, stop])

      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
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
   assertBool "choice (on lower level) is substituted 2"  $ eqProcDefs procDefs'' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] bexprP
      bexprP = choice [actionPref actOfferAx (
                          choice [ actionPref actOfferBy (choice [stop,stop]) ,
                                   stop]),
                      stop]

      procIdPpre1 = procIdGen "P$pre1" [chanIdA] [varIdX]
      procIdPpre1pre1 = procIdGen "P$pre1$pre1" [chanIdA] [varIdX, varIdY]
      procInstPpre1 = procInst procIdPpre1 [chanIdA] [vexprX]

      procDefP' = ProcDef [chanIdA] [] (choice [actionPref actOfferAx procInstPpre1,
                                                stop])

      procInstPpre1pre1 = procInst procIdPpre1pre1 [chanIdA] [vexprX, vexprY]
      procDefPpre1 = ProcDef [chanIdA] [varIdX] (choice [actionPref actOfferBy procInstPpre1pre1,
                                                         stop])
      procDefPpre1pre1 = ProcDef [chanIdA] [varIdX, varIdY] (choice [stop, stop])

      procDefs' = Map.fromList  [ (procIdP,procDefP) ]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
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
