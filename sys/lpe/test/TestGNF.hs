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

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text         as T
import VarId
import Test.HUnit

import ValExpr

import TranslatedProcDefs
import TxsDefs
import LPEfunc
import TestDefinitions

-- import Debug.Trace

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

-- check that things have been translated to pregnfFunc first
-- e.g. choice lower in the hierarchy is put in new ProcDef
-- P[A]() = A?x >-> ( (A!1 >->STOP) ## (A?x >->STOP) )
-- becomes
  -- P[A]()  = A?x >-> P$pre1[A](x)
  -- P$pre1[A](P$pre1$x) = (A!1 >->STOP) ## (A?x >->STOP)
testPreGNFFirst :: Test
testPreGNFFirst = TestCase $
   assertBool "choice (on lower level) is substituted" $ eqProcDefs procDefs'' procDefsResult
   where
      (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'

      procIdP   = procIdGen "P" [chanIdA] []
      choice'   = choice $ Set.fromList [actionPref actOfferA1 stop, actionPref actOfferAx stop]
      procDefP = ProcDef [chanIdA] [] (actionPref actOfferAx choice')

      procIdPpre1x = procIdGen "P$pre1" [chanIdA] [varIdPpre1X]
      procInstPpre1 = procInst procIdPpre1x [chanIdA] [vexprX]

      procDefP' = ProcDef [chanIdA] [] (actionPref actOfferAx procInstPpre1)
      procDefPpre1x = ProcDef [chanIdA] [varIdPpre1X] choice'


      procDefs' = Map.fromList  [ (procIdP, procDefP) ]
      procDefs'' = Map.fromList [ (procIdP, procDefP')
                                , (procIdPpre1x, procDefPpre1x) 
                                ]


-- Stop remains unchanged
testStop :: Test
testStop = TestCase $
    let procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [varIdX] stop)]
        procIdP = procIdGen "P" [chanIdA] [varIdX]
        (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'
    in  assertBool "STOP"  $ eqProcDefs procDefs' procDefsResult

-- A?X >-> STOP remains unchanged
testASeqStop :: Test
testASeqStop = TestCase $
    let procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [] (actionPref actOfferAx stop))]
        procIdP = procIdGen "P" [chanIdA] [varIdX]
        (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'
    in  assertBool "STOP"  $ eqProcDefs procDefs' procDefsResult

-- P[]() := A?x >-> P[A](x) remains unchanged
-- also checks that there are no infinite loops of valid gnfFunc translations
-- checks for impossible gnfFunc translations, see below
testASeqProcInst :: Test
testASeqProcInst = TestCase $
    let procIdP = procIdGen "P" [chanIdA] [varIdX]
        procInstP = procInst procIdP [chanIdA] [vexprX]
        procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [varIdX] (actionPref actOfferAx procInstP))]
        (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'
    in  assertBool "STOP"  $ eqProcDefs procDefs' procDefsResult



-- P[A,B]() := A?x >-> B!1 >-> STOP is split, becomes:
-- P[A,B]() := A?x >-> P$gnf1[A,B](x)
-- P$gnf1[A,B](P$gnf1$x) := B!1 >-> STOP
testActPrefSplit :: Test
testActPrefSplit = TestCase $
   assertBool "multi action sequence is split"  $ eqProcDefs procDefs'' procDefsResult
   where
      (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'

      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procDefP = ProcDef [chanIdA, chanIdB] [] (actionPref actOfferAx (actionPref actOfferB1 stop))


      procIdPgnf1 = procIdGen "P$gnf1" [chanIdA, chanIdB] [varIdPgnf1X]
      procInstPgnf1x = procInst procIdPgnf1 [chanIdA, chanIdB] [vexprX]

      procDefP' = ProcDef [chanIdA, chanIdB] [] (actionPref actOfferAx procInstPgnf1x)
      procDefPgnf1x = ProcDef [chanIdA, chanIdB] [varIdPgnf1X] (actionPref actOfferB1 stop)


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
   assertBool "procInst is substituted"  $ eqProcDefs procDefs'' procDefsResult
   where
      (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'

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
   assertBool "procInst is substituted 2"  $ eqProcDefs procDefs'' procDefsResult
   where
      (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'

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
   assertBool "procInst is substituted 3"  $ eqProcDefs procDefs'' procDefsResult
   where
      (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'

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
   assertBool "procInst is substituted and normalised"  $ eqProcDefs procDefs'' procDefsResult
   where
      (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'
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



-- P[A]() := [[x == 1]] =>> Q[A]()
-- Q[A]() := A?x >-> STOP
-- with procInst = P[A]()
-- 
-- becomes
-- P[A]() :=  [[x == 1]] =>> A?x >-> STOP     
-- with procInst = P[A]()
testGuardProcInst :: Test
testGuardProcInst = TestCase $
   assertBool "guard procInst"  $ eqProcDefs procDefs'' procDefsResult
   where
      (procDefsResult, _gnfTodo) =  gnfFunc procIdP emptyTranslatedProcDefs procDefs'
      procIdP = procIdGen "P" [] []
      procIdQ = procIdGen "Q" [] []
      procDefP = ProcDef [] [] (guard   (cstrEqual vexprX int1 )
                                        (procInst procIdQ [chanIdA] []))
      procDefQ = ProcDef [] []  (actionPref actOfferAx stop)


      procDefP' = ProcDef [] [] (guard  (cstrEqual vexprX int1 )
                                        (actionPref actOfferAx stop))

      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdQ, procDefQ) ]

     
-- P[A]() := A?x >-> [[x == 1]] =>> P[A]()
-- with procInst = P[A]()
-- 
-- explanation: the formal parameters of the ProcDef that is being created need to be made unique
-- to avoid name clashes due substitution
--
-- becomes
-- P[A]() :=  A?x >-> P$gnf1[A](x)
-- P$gnf1[A](P$gnf1$x) :=  A?x [[P$gnf1$x == 1]] >-> P$gnf1[A](x)
-- with procInst = P[A]()
testActionPrefGuard :: Test
testActionPrefGuard = TestCase $
    --    trace ("\n\n expected: " ++ show procDefs'' ++ "\n\n got: " ++ show procDefsResult) $ 
   assertBool "testActionPrefGuard"  $ eqProcDefs procDefs'' procDefsResult
   where
      (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'

      procIdP = procIdGen "P" [] []
      procIdPgnf1 = procIdGen "P$gnf1" [] [varIdPgnf1X]
      procInstP = procInst procIdP [] []

      procDefP = ProcDef [] [] (actionPref actOfferAx (guard  (cstrEqual vexprX int1) procInstP))

      procDefP' = ProcDef [] [] (actionPref actOfferAx (procInst procIdPgnf1 [] [vexprX]))
      procDefPgnf1 = ProcDef [] [varIdPgnf1X] (actionPref actOfferAx {constraint = cstrEqual vexprPgnf1X int1} 
                                                    (procInst procIdPgnf1 [] [vexprX]))            


      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                 , (procIdPgnf1, procDefPgnf1) ]

-- example with multiple choices, where the guard needs to be distributed to each option
--       
-- P[A]() :=    A?x >-> [[x == 1]] =>> P[A]()
--          ##  A?x >-> Stop
-- with procInst = P[A]()
--
-- becomes
-- P[A]() :=    A?x >-> P$gnf1[A]()
--          ##  A?x >-> Stop 
-- P$gnf1[A](P$gnf1$x) :=       A?x [[P$gnf1$x == 1]] >-> P$gnf1[A](x)
--                          ##  A?x [[P$gnf1$x == 1]] >-> Stop 
-- with procInst = P[A]()
testActionPrefGuard2 :: Test
testActionPrefGuard2 = TestCase $
    --    trace ("\n\n expected: " ++ show procDefs'' ++ "\n\n got: " ++ show procDefsResult) $ 
   assertBool "testActionPrefGuard"  $ eqProcDefs procDefs'' procDefsResult
   where
      (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'

      procIdP = procIdGen "P" [] []
      procIdPgnf1 = procIdGen "P$gnf1" [] [varIdPgnf1X]
      procInstP = procInst procIdP [] []

      procDefP = ProcDef [] [] (choice $ Set.fromList [
                                            actionPref actOfferAx (guard  (cstrEqual vexprX int1) procInstP)
                                        ,   actionPref actOfferAx stop
                                ])

      procDefP' = ProcDef [] [] (choice $ Set.fromList [
                                            actionPref actOfferAx (procInst procIdPgnf1 [] [vexprX])
                                        ,   actionPref actOfferAx stop
                                        ])
      procDefPgnf1 = ProcDef [] [varIdPgnf1X] (choice $ Set.fromList [
                                                        actionPref actOfferAx {constraint = cstrEqual vexprPgnf1X int1} 
                                                            (procInst procIdPgnf1 [] [vexprX])
                                                    ,   actionPref actOfferAx {constraint = cstrEqual vexprPgnf1X int1} 
                                                            stop
                                        ])


      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                 , (procIdPgnf1, procDefPgnf1) ]


       
-- P[A]() := A?x >-> Q[A]()
-- Q[A]() := A?x >-> [[x == 1]] P[A]()
-- with procInst = P[A]()
--
-- becomes
-- P[A]() := A?x >-> Q[A]()
-- Q[A]() := A?x >-> Q$gnf1[A](x)
-- Q$gnf1[A](P$gnf1$x) :=  A?x [[P$gnf1$x == 1]] >-> Q[A]()         -- watch out! the x checked in the constraints refers back to the previous x, hence P$gnf1$x
-- with procInst = P[A]()
testGuardLoop :: Test
testGuardLoop = TestCase $
   --    trace ("\n\n expected: " ++ show procDefs'' ++ "\n\n got: " ++ show procDefsResult) $ 
   assertBool "testActionPrefGuard"  $ eqProcDefs procDefs'' procDefsResult
   where
      (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'

      procIdP = procIdGen "P" [] []
      procIdQ = procIdGen "Q" [] []
      procIdQgnf1 = procIdGen "Q$gnf1" [] [varIdQgnf1X]
      procInstP = procInst procIdP [] []
      procInstQ = procInst procIdQ [] []

      procDefP = ProcDef [] [] (actionPref actOfferAx procInstQ)
      procDefQ = ProcDef [] [] (actionPref actOfferAx (guard  (cstrEqual vexprX int1) procInstP))


      varIdQgnf1X :: VarId
      varIdQgnf1X = VarId (T.pack "Q$gnf1$x") 233 intSort
      vexprQgnf1X :: VExpr
      vexprQgnf1X = cstrVar varIdQgnf1X

      procDefQ' = ProcDef [] [] (actionPref actOfferAx (procInst procIdQgnf1 [] [vexprX]))
      procDefQgnf1 = ProcDef [] [varIdQgnf1X] (actionPref actOfferAx {constraint = cstrEqual vexprQgnf1X int1} 
                                                procInstQ)            

      procDefs' = Map.fromList  [  (procIdP, procDefP),
                                   (procIdQ, procDefQ)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP)
                                 , (procIdQ, procDefQ')
                                 , (procIdQgnf1, procDefQgnf1) ]


-- P[A](y) := A?x >-> [[y == 1]] P[A](x)
-- with procInst = P[A](0)
--
-- becomes after GNF:
-- P[A](y) :=                           A?x >-> P$gnf1[A](y,x)
-- P$gnf1[A](P$gnf1$y, P$gnf1$x) := A?x [[P$gnf1$y == 1]] >-> P$gnf1[A](P$gnf1$x, x)
-- with procInst = P[A](0)
testGuardLoop2 :: Test
testGuardLoop2 = TestCase $
   -- trace ("\n\n expected: " ++ show procDefs'' ++ "\n\n got: " ++ show procDefsResult) $ 
   assertBool "testActionPrefGuard"  $ eqProcDefs procDefs'' procDefsResult
   where
      (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'

      procIdP = procIdGen "P" [chanIdA] [varIdY]
      procIdPgnf1 = procIdGen "P$gnf1" [chanIdA] [varIdPgnf1Y, varIdPgnf1X]

      procInstP = procInst procIdP [chanIdA] [vexprX]
      
      procDefP = ProcDef [chanIdA] [varIdY] (actionPref actOfferAx (guard  (cstrEqual vexprY int1) procInstP))
      procDefP' = ProcDef [chanIdA] [varIdY] (actionPref actOfferAx (procInst procIdPgnf1 [chanIdA] [vexprY, vexprX]))
      procDefPgnf1 = ProcDef [chanIdA] [varIdPgnf1Y, varIdPgnf1X] (actionPref actOfferAx {constraint = cstrEqual vexprPgnf1Y int1} 
                                                            (procInst procIdPgnf1 [chanIdA] [vexprPgnf1X, vexprX]))            

      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                 , (procIdPgnf1, procDefPgnf1) ]



-- P[A](z) :=       A?x >-> A?y >-> (P[A](0) ## P[A](1))
--              ##  Q[A]()
-- Q[A]() := A?x >-> STOP 
-- with procInst = P[A](0)
--
-- becomes after preGNF:
-- P[A](z) :=       A?x >-> A?y >-> P$pre1(z,x,y)
--              ##  Q[A]()
-- Q[A]() := A?x >-> STOP 
-- P$pre1(P$pre1$z, P$pre1$x, P$pre1$y) := P[A](0) ## P[A](1)
-- 
-- remarks:
-- the GNF translation of P will create a new ProcDef for part of the first summand: A?y >-> P$pre1(z,x,y)
--      then recursively that new ProcDef will be translated to GNF
--         thus P$pre1 needs to be translated to GNF
--      However, the translation of P$pre1 involves instantiating P[A](0) and P[A](1)
--          But P is still being translated to GNF 
--          thus the GNF translation of P$pre needs to be postponed until after the GNF translation of P
-- 
-- becomes after GNF:
-- P[A](z) :=       A?x >-> P$gnf1[A](z,x)
--              ##  A?x >-> STOP                    -- Q[A]() was instantiated
-- Q[A]() := A?x >-> STOP 
-- P$gnf1[A](P$gnf1$z,P$gnf1$x) := A?y >-> P$pre1(P$gnf1$z,P$gnf1$x,y)
-- P$pre1(P$pre1$z, P$pre1$x, P$pre1$y) := 
--                  A?x >-> P$gnf1[A](0,x)          -- P[A](0) was instantiated
--              ##  A?x >-> STOP                    -- cont'd...
--              ##  A?x >-> P$gnf1[A](1,x)          -- P[A](1) was instantiated
--              ##  A?x >-> STOP                    -- cont'd...
-- 
-- with procInst = P[A](0)
testNoPrematureSubstitution :: Test
testNoPrematureSubstitution = TestCase $
   -- trace ("\n\n expected: " ++ show procDefs'' ++ "\n\n got: " ++ show procDefsResult) $ 
   assertBool "testActionPrefGuard"  $ eqProcDefs procDefs'' procDefsResult
   where
    (procDefsResult, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'

    procIdP = procIdGen "P" [chanIdA] [varIdZ]
    procIdQ = procIdGen "Q" [chanIdA] []

    
    procInstP0 = procInst procIdP [chanIdA] [int0]
    procInstP1 = procInst procIdP [chanIdA] [int1]
    procInstQ = procInst procIdQ [chanIdA] []
    
    procDefP = ProcDef [chanIdA] [varIdZ] (choice $ Set.fromList [ actionPref actOfferAx 
                                                                     (actionPref actOfferAy 
                                                                        (choice $ Set.fromList [procInstP0, procInstP1]))
                                                                 , procInstQ])
    procDefQ = ProcDef [chanIdA] [] (actionPref actOfferAx stop)
    procDefs' = Map.fromList  [  (procIdP, procDefP),
                                (procIdQ, procDefQ)]


    -- translated:



    procIdPgnf1 = procIdGen "P$gnf1" [chanIdA] [varIdPgnf1Z, varIdPgnf1X]
    procIdPpre1 = procIdGen "P$pre1" [chanIdA] [varIdPpre1Z, varIdPpre1X, varIdPpre1Y]

    procDefP' = ProcDef [chanIdA] [varIdZ] (choice $ Set.fromList [actionPref actOfferAx ( procInst procIdPgnf1 [chanIdA] [vexprZ, vexprX])
                                                                  ,actionPref actOfferAx stop])
    procDefPgnf1 = ProcDef [chanIdA] [varIdPgnf1Z, varIdPgnf1X] 
                            (actionPref actOfferAy (procInst procIdPpre1 [chanIdA] [vexprPgnf1Z, vexprPgnf1X, vexprY]))
    
    procDefPpre1 = ProcDef [chanIdA] [varIdPpre1Z, varIdPpre1X, varIdPpre1Y] 
                        (choice $ Set.fromList [
                                actionPref actOfferAx (procInst procIdPgnf1 [chanIdA] [int0, vexprX]),
                                actionPref actOfferAx stop,
                                actionPref actOfferAx (procInst procIdPgnf1 [chanIdA] [int1, vexprX]),
                                actionPref actOfferAx stop
                        ])
                        
    
    procDefs'' = Map.fromList  [ (procIdP, procDefP'),
                                 (procIdPgnf1, procDefPgnf1),
                                 (procIdQ, procDefQ),
                                 (procIdPpre1, procDefPpre1) ]

                 
-- no name clash of newly created ProcDefs by pregnfFunc and GNF:
-- P[A,B]() := A?x >-> B!1 >-> ( (A!1 >->STOP) ## (A?x >->STOP) )
-- becomes after preGNF:
-- P[A,B]() := A?x >-> B!1 >-> P$pre1[A,B](x)
-- P$pre1[A,B](x) := STOP ## STOP
--
-- becomes after GNF:
-- P[A,B]() := A?x >-> P$gnf1(x)
-- P$gnf1[A,B](P$gnf1$x) := B!1 >-> P$pre1[A,B](P$gnf1$x)
-- P$pre1[A,B](P$pre1$x) := (A!1 >->STOP) ## (A?x >->STOP)
testNamingClash :: Test
testNamingClash = TestCase $
   assertBool "pregnfFunc / gnfFunc naming of new ProcDefs doesn't clash"  $ eqProcDefs procDefs'' procDefsRes
   where
      (procDefsRes, _gnfTodo) = gnfFunc procIdP emptyTranslatedProcDefs procDefs'

      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procIdPgnf1 = procIdGen "P$gnf1" [chanIdA, chanIdB] [varIdPgnf1X]
      procIdPpre1 = procIdGen "P$pre1" [chanIdA, chanIdB] [varIdPpre1X]
      choice'= choice $ Set.fromList [actionPref actOfferA1 stop, actionPref actOfferAx stop]
      
      procDefP = ProcDef [chanIdA, chanIdB] [] (actionPref actOfferAx (actionPref actOfferB1 choice'))

      procDefP' = ProcDef [chanIdA, chanIdB] [] (actionPref actOfferAx (procInst procIdPgnf1 [chanIdA, chanIdB] [vexprX]))
      procDefPgnf1 = ProcDef [chanIdA, chanIdB] [varIdPgnf1X] (actionPref actOfferB1 (procInst procIdPpre1 [chanIdA, chanIdB] [vexprPgnf1X]))
      procDefPpre1 = ProcDef [chanIdA, chanIdB] [varIdPpre1X] choice'

      procDefs' = Map.fromList  [ (procIdP, procDefP) ]

      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdPgnf1, procDefPgnf1)
                                , (procIdPpre1, procDefPpre1) ]


-- assertException :: (Exception e, Eq e) => e -> ProcDefs -> IO ()
-- assertException ex action =
--     handleJust isWanted (const $ return ()) $ do
--         _ <- evaluate action
--         assertFailure $ "Expected exception: " ++ show ex
--     where isWanted = Control.Monad.guard . (== ex)

-- assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
-- assertException ex action =
--     handleJust isWanted (const $ return ()) $ do
--         _ <- action
--         assertFailure $ "Expected exception: " ++ show ex
--     where isWanted = Control.Monad.guard . (== ex)


-- -- cycle detection
-- --  P[]() := P[]()
-- -- should fail
-- testLoop1 :: Test
-- testLoop1 = TestCase $
--    -- result = gnfFunc procIdP emptyTranslatedProcDefs procDefs
-- --    let (result, err) = gnfFunc procIdP emptyTranslatedProcDefs procDefs in
-- --    assertBool "loop 1" err "loop (GNF) detected in P"
-- --    assertFailure "found a no-progress loop" (gnfFunc procIdP emptyTranslatedProcDefs procDefs)
-- --    assertException (ErrorCall "found a no-progress loop") (evaluate $ gnfFunc procIdP emptyTranslatedProcDefs procDefs)
-- --    assertRaises "desc error..." (ErrorCall "found a no-progress loop") (evaluate $ gnfFunc procIdP emptyTranslatedProcDefs procDefs)
--     -- assertBool "loop 1"  $ eqProcDefs procDefs' (gnfFunc procIdP emptyTranslatedProcDefs procDefs)
--     assertException (ErrorCall "found a no-progress loop") (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
--     where
--       procIdP = procIdGen "P" [] []
--       procDefP = ProcDef [] [] (procInst procIdP [] [])

--       procDefs' = Map.fromList  [  (procIdP, procDefP)]

-- -- cycle detection
-- --  P[]() :=     A >-> P[]()
-- --            ## P[]()
-- -- should fail
-- testLoop2 :: Test
-- testLoop2 = TestCase $
--    assertBool "loop 2"  $ eqProcDefs procDefs'' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
--    where
--       procIdP = procIdGen "P" [] []
--       procDefP = ProcDef [] [] (choice (Set.fromList [
--                                   (actionPref actOfferAx (procInst procIdP [] [])),
--                                   (procInst procIdP [] [])
--                                   ]))

--       procDefs' = Map.fromList  [  (procIdP, procDefP)]
--       procDefs'' = procDefs'


-- -- cycle detection
-- --  P[]() :=     A >-> P[]()
-- --            ## Q[]()
-- --  Q[]() :=  P[]()
-- -- should fail
-- testLoop3 :: Test
-- testLoop3 = TestCase $
--     assertBool "loop 3"  $ eqProcDefs procDefs'' (gnfFunc procIdP emptyTranslatedProcDefs procDefs')
--     where
--       procIdP = procIdGen "P" [] []
--       procIdQ = procIdGen "Q" [] []
--       procDefP = ProcDef [] [] (choice (Set.fromList [
--                                   (actionPref actOfferAx (procInst procIdP [] [])),
--                                   (procInst procIdQ [] [])
--                                   ]))
--       procDefQ = ProcDef [] [] (procInst procIdP [] [])
--       procDefs' = Map.fromList  [  (procIdP, procDefP),
--                                   (procIdQ, procDefQ)]
--       procDefs'' = procDefs'






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

                        , TestLabel "guard procInst" testGuardProcInst
                        , TestLabel "actionpref guard procInst" testActionPrefGuard
                        , TestLabel "actionpref guard procInst 2" testActionPrefGuard2
                        , TestLabel "guard loop" testGuardLoop                        
                        , TestLabel "guard loop2" testGuardLoop2      
                        
                        , TestLabel "no premature substitution" testNoPrematureSubstitution

                        , TestLabel "pregnfFunc / gnfFunc naming of new ProcDefs doesn't clash" testNamingClash

                        -- , TestLabel "loop 1" testLoop1
                        -- , TestLabel "loop 2" testLoop2
                        -- , TestLabel "loop 3" testLoop3
                      ]
