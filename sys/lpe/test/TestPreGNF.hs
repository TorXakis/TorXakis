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
import qualified Data.Text         as T
import VarId

import TxsDefs
import ValExpr

import LPEfunc
import TestDefinitions

-- import Debug.Trace

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
-- Q[A]() = A?x >-> (P[A]() ## Q[A]())
-- becomes
-- P[A]() = A?x >-> Q[A]()
-- Q[A]() = A?x >-> Q$pre1[A](x)
-- Q$pre1[A](P$pre1$x) = P[A]() ## Q[A]()
testActPref3 :: Test
testActPref3 = TestCase $
   assertBool "ActionPref is translated recursively" $ eqProcDefs  procDefs'' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] []

      procInstP = procInst procIdP [chanIdA] []
      procInstQ = procInst procIdQ [chanIdA] []
      
      procDefP = ProcDef [chanIdA] [] (actionPref actOfferAx procInstQ)
      procDefQ = ProcDef [chanIdA] [] (actionPref actOfferAx (choice $ Set.fromList [procInstP, procInstQ]))


      varIdQpre1X :: VarId
      varIdQpre1X = VarId (T.pack "Q$pre1$x") 33 intSort

      procIdQpre1 = procIdGen "Q$pre1" [chanIdA] [varIdQpre1X]
      procDefQ' = ProcDef [chanIdA] [] (actionPref actOfferAx (procInst procIdQpre1 [chanIdA] [vexprX]))
      procDefQpre1 = ProcDef [chanIdA] [varIdQpre1X] (choice $ Set.fromList [procInstP, procInstQ])

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
       procInstP = procInst procIdP [chanIdA] []
       procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [] (choice $ Set.fromList [procInstP, actionPref actOfferAx procInstP]))]
   in  assertBool "P[A]() ## (A?x -> P[A]())" $ eqProcDefs  procDefs' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')

-- choices at top-level remain unchanged
testChoice2 :: Test
testChoice2 = TestCase $
   let procIdP = procIdGen "P" [chanIdA, chanIdB] []
       procInstP = procInst procIdP [chanIdA, chanIdB] []
       procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA, chanIdB] [] (choice $ Set.fromList [procInstP, actionPref actOfferAx procInstP, actionPref actOfferB1 procInstP]))]
   in  assertBool "P[A,B]() ## (A?x -> P[A,B]()) ## (B?x -> P[A,B]())"  $ eqProcDefs procDefs' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')


-- choices at a lower level are substituted with a process instance to a
-- process definition that is created for exactly the substituted term
-- P[A]() = A?x >-> (P[A]() ## (A?x >-> STOP))
-- becomes
  -- P[A]()  = A?x >-> P$pre1[A](x)
  -- P$pre1[A](P$pre1$x) = P[A]() ## (A?x >-> STOP)
testChoice3 :: Test
testChoice3 = TestCase $
   assertBool "choice (on lower level) is substituted" $ eqProcDefs  procDefs'' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP   = procIdGen "P" [chanIdA] []
      procInstP = procInst procIdP [chanIdA] []
      choice'   = choice $ Set.fromList [procInstP, actionPref actOfferAx stop]
      procDefP  = ProcDef [chanIdA] [] (actionPref actOfferAx choice')


      procIdPpre1 = procIdGen "P$pre1" [chanIdA] [varIdPpre1X]
      procDefP' = ProcDef [chanIdA] [] (actionPref actOfferAx (procInst procIdPpre1 [chanIdA] [vexprX]))
      procDefPpre1 = ProcDef [chanIdA] [varIdPpre1X] choice'

      procDefs'  = Map.fromList  [  (procIdP, procDefP)]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                 , (procIdPpre1, procDefPpre1) 
                                 ]


-- choices at a lower level are substituted with a process instance to a
-- process definition that is created for exactly the substituted term
-- SAME AS ABOVE, only nested choice is second expression of a choice
-- P[A]() =     A?x >-> STOP
--           ## A?x >-> (p[A]() ## (A?x >-> STOP))
-- becomes
  -- P[A]() =   A?x >-> STOP
  --         ## A?x >-> P$pre2[A](x)
  -- P$pre2[A](P$pre2$x) = P[A]() ## (A?x >-> STOP)
testChoice4 :: Test
testChoice4 = TestCase $
   assertBool "choice (on lower level) is substituted 2"  $ eqProcDefs procDefs'' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')
   where
      procIdP  = procIdGen "P" [chanIdA] []
      procInstP = procInst procIdP [chanIdA] []
      axstop   = actionPref actOfferAx stop
      choice'  = choice $ Set.fromList [procInstP, axstop]
      procDefP = ProcDef [chanIdA] [] (choice $ Set.fromList [axstop, actionPref actOfferAx choice'])


      varIdPpre2X :: VarId
      varIdPpre2X = VarId (T.pack "P$pre2$x") 33 intSort
      
      procIdPpre2 = procIdGen "P$pre2" [chanIdA] [varIdPpre2X]
      procInstPpre2 = procInst procIdPpre2 [chanIdA] [vexprX]
      procDefP' = ProcDef [chanIdA] [] (choice $ Set.fromList [axstop, actionPref actOfferAx procInstPpre2])
      procDefPpre2 = ProcDef [chanIdA] [varIdPpre2X] choice'

      procDefs'  = Map.fromList [ (procIdP, procDefP) ]
      procDefs'' = Map.fromList [ (procIdP, procDefP')
                                , (procIdPpre2, procDefPpre2) 
                                ]



-- choices nested two levels deep
-- P[A,B]() = (A?x >-> ((B?y >-> (P[A,B]() ## A?x >-> STOP) ## P[A,B]())) ## P[A,B]()
-- or maybe more readable:
--   P =
--                A?x
--          >->
--                          B?y
--                     >->
--                              P[A,B]()
--                          ##
--                              A?x >-> STOP
--                ##
--                     P[A,B]()
--    ##
--          P[A,B]()

-- becomes:
-- P[A,B]()                                         = ( A?x >-> P$pre1[A](x)  ) ## P[A,B]()
-- P$pre1[A,B](P$pre1$x)                            = (B?y >-> P$pre1$pre1[A](P$pre1$x,y)) ## P[A,B]()
-- P$pre1$pre1[A,B](P$pre1$pre1$x,P$pre1$pre1$y)    = P[A,B]() ## A?x >-> STOP
testChoice5 :: Test
testChoice5 = TestCase $
   assertBool "choice (on lower level) is substituted 2" 
   -- $ trace ("\n\n expected: " ++ show procDefs'' ++ "\n\n got: " ++ show res ) 
   $ eqProcDefs procDefs'' res 
   where
      res = preGNFFunc procIdP emptyTranslatedProcDefs procDefs'
      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procDefP = ProcDef [chanIdA, chanIdB] [] bexprP
      procInstP = procInst procIdP [chanIdA, chanIdB] []
      axstop    = actionPref actOfferAx stop
      choice'   = choice $ Set.fromList [procInstP, axstop]
      bexprP = choice $ Set.fromList [actionPref actOfferAx (
                          choice $ Set.fromList [ actionPref actOfferBy choice' 
                                 , procInstP]),
                      procInstP]


      varIdPpre1pre1X :: VarId
      varIdPpre1pre1X = VarId (T.pack "P$pre1$pre1$P$pre1$x") 33 intSort
      varIdPpre1pre1Y :: VarId
      varIdPpre1pre1Y = VarId (T.pack "P$pre1$pre1$y") 33 intSort
      
      procIdPpre1 = procIdGen "P$pre1" [chanIdA, chanIdB] [varIdPpre1X]
      procIdPpre1pre1 = procIdGen "P$pre1$pre1" [chanIdA, chanIdB] [varIdPpre1pre1X, varIdPpre1pre1Y]
      procInstPpre1 = procInst procIdPpre1 [chanIdA, chanIdB] [vexprX]

      procDefP' = ProcDef [chanIdA, chanIdB] [] (choice $ Set.fromList [ actionPref actOfferAx procInstPpre1
                                                        , procInstP])

      procInstPpre1pre1 = procInst procIdPpre1pre1 [chanIdA, chanIdB] [vexprPpre1X, vexprY]
      procDefPpre1 = ProcDef [chanIdA, chanIdB] [varIdPpre1X] (choice $ Set.fromList [ actionPref actOfferBy procInstPpre1pre1
                                                                 , procInstP])
      procDefPpre1pre1 = ProcDef [chanIdA, chanIdB] [varIdPpre1pre1X, varIdPpre1pre1Y] choice'

      procDefs' = Map.fromList  [ (procIdP,procDefP) ]
      procDefs'' = Map.fromList  [ (procIdP, procDefP')
                                , (procIdPpre1, procDefPpre1)
                                , (procIdPpre1pre1, procDefPpre1pre1)]


-- Guard Stop remains unchanged
testGuardStop :: Test
testGuardStop = TestCase $
   let procIdP = procIdGen "P" [] []
       procDefs' = Map.fromList [(procIdP, ProcDef [chanIdA] [] 
                                        (guard (cstrEqual vexprX int1 )
                                                stop))]
   in  assertBool "testGuardStop" $ eqProcDefs  procDefs' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')

-- Guard ProcInst remains unchanged
testGuardProcInst :: Test
testGuardProcInst = TestCase $
      let procIdP = procIdGen "P" [] []
          procInstP = procInst procIdP [] []
          procDefs' = Map.fromList [(procIdP, ProcDef [] [] 
                                           (guard (cstrEqual vexprX int1 )
                                                   procInstP))]
      in  assertBool "testGuardProcInst" $ eqProcDefs  procDefs' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')

-- Guard ActionPref remains unchanged
testGuardActionPref :: Test
testGuardActionPref = TestCase $
      let procIdP = procIdGen "P" [] []
          procDefs' = Map.fromList [(procIdP, ProcDef [] [] 
                                           (guard   (cstrEqual vexprX int1 )
                                                    (actionPref actOfferAx stop) ))]
      in  assertBool "testGuardActionPref" $ eqProcDefs  procDefs' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')
   

-- ActionPref (Guard ProcInst) remains unchanged
-- i.e. A?x >-> ([[x==1]] =>> P[]())
testActionPrefGuardProcInst :: Test
testActionPrefGuardProcInst = TestCase $
      let procIdP = procIdGen "P" [] []
          procInstP = procInst procIdP [] []
          procDefs' = Map.fromList [(procIdP, ProcDef [] [] 
                                                (actionPref actOfferAx 
                                                    (guard   (cstrEqual vexprX int1) procInstP)
                                                )
                                    )]
                                           
                                            
      in  assertBool "testActionPrefGuardProcInst" $ eqProcDefs  procDefs' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')
   

-- Guard Choice 
-- [[x = 1]] =>> (A?x >-> STOP ## A?x >-> STOP)
-- becomes
--      [[x = 1]] =>> A?x >-> STOP 
-- ##   [[x = 1]] =>> A?x >-> STOP 
testGuardChoice :: Test
testGuardChoice = TestCase $
      let procIdP = procIdGen "P" [] []
          procDefs' = Map.fromList [(procIdP, ProcDef [] [] 
                                           (guard   (cstrEqual vexprX int1 )
                                                    (choice $ Set.fromList [actionPref actOfferAx stop, stop]) ))]
          procDefs'' = Map.fromList [(procIdP, ProcDef [] [] 
                                        (choice $ Set.fromList [
                                            guard   (cstrEqual vexprX int1 )
                                                     (actionPref actOfferAx stop),
                                            guard   (cstrEqual vexprX int1 )
                                                     stop
                                            ]))]
      in assertBool "testGuardChoice" $ eqProcDefs  procDefs'' (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')

      
-- leaving out test for Guard with Parallel
--      because testing against full return of lpePar is incredibly tedious (return multiple intermediarey results)
--      solution: going to cover this in tests for LPE of Guard: there we only need to test against relevant return of lpePar - the rest is filtered out

-- testGuardPar :: Test
-- testGuardPar = TestCase $
--       let   procIdP = procIdGen "P" [] []
--             procDefs' = Map.fromList [(procIdP, ProcDef [] [] 
--                                            (guard   (cstrEqual vexprX int1 )
--                                                     ( parallel Set.empty [stop, stop])))]


--             varIdOp1pcPpre1op1 = VarId (T.pack "op1$pc$P$pre1$op1") 0 intSort
--             varIdOp1pcPpre1op2 = VarId (T.pack "op1$pc$P$pre1$op2") 0 intSort

--             -- vexprOp1pcPpre1op1 = cstrVar varIdOp1pcPpre1op1

--             procIdPpre1 = procIdGen "P$pre1" [] [varIdOp1pcPpre1op1, varIdOp1pcPpre1op2]
--             procInstPpre1 = procInst procIdPpre1 [] [int0, int0]
--             procDefs'' = Map.fromList [(procIdP, ProcDef [] [] 
--                                                 (guard (cstrEqual vexprX int1 )
--                                                         procInstPpre1)),
                                      
--                                         (ProcId {ProcId.name = T.pack "P$pre1", ProcId.unid = 1, procchans = [], procvars = [],  procexit = NoExit},
--                                                 ProcDef [] [] (parallel Set.empty [choice Set.empty, choice Set.empty])),
                                                
                                                
--                                         (ProcId {ProcId.name = T.pack "P$pre1", ProcId.unid = 10, procchans = [], 
--                                             procvars = [
--                                                 VarId {VarId.name = T.pack "op1$pc$P$pre1$op1", VarId.unid = 5, varsort = SortId {SortId.name = T.pack "Int", SortId.unid = 102}},
--                                                 VarId {VarId.name = T.pack "op2$pc$P$pre1$op2", SortId.unid = 9, varsort = SortId {SortId.name = T.pack "Int", SortId.unid = 102}}], procexit = NoExit},
--                                             ProcDef [] [VarId {VarId.name = T.pack "op1$pc$P$pre1$op1", VarId.unid = 5, varsort = SortId {SortId.name = T.pack "Int", SortId.unid = 102}},VarId {VarId.name = T.pack "op2$pc$P$pre1$op2", VarId.unid = 9, varsort = SortId {SortId.name = T.pack "Int", SortId.unid = 102}}] (choice Set.empty))
                                                
                                                
--                                                 -- (ProcId {name = "P$pre1$op1", unid = 2, procchans = [], procvars = [], procexit = NoExit},ProcDef [] [] (BExpr {view = Choice (fromList [])})),
                                                
                                                
--                                                 -- (ProcId {name = "P$pre1$op1", unid = 4, procchans = [], procvars = [VarId {name = "pc$P$pre1$op1", unid = 3, varsort = SortId {name = "Int", unid = 102}}], procexit = NoExit},ProcDef [] [VarId {name = "pc$P$pre1$op1", unid = 3, varsort = SortId {name = "Int", unid = 102}}] (BExpr {view = Choice (fromList [])})),   
                                                
--                                                 -- (ProcId {name = "P$pre1$op2", unid = 6, procchans = [], procvars = [], procexit = NoExit},ProcDef [] [] (BExpr {view = Choice (fromList [])})),
                                                
--                                                 -- (ProcId {name = "P$pre1$op2", unid = 8, procchans = [], procvars = [VarId {name = "pc$P$pre1$op2", unid = 7, varsort = SortId {name = "Int", unid = 102}}], procexit = NoExit},ProcDef [] [VarId {name = "pc$P$pre1$op2", unid = 7, varsort = SortId {name = "Int", unid = 102}}] (BExpr {view = Choice (fromList [])}))
--                                                         ]
--             res = (preGNFFunc procIdP emptyTranslatedProcDefs procDefs')
--       in  trace ("\n expected': " ++ show procDefs' ++ "\ngot: " ++ show res) $ assertBool "testGuardPar" $ eqProcDefs  procDefs'' res
   

----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testPreGNFList :: Test
testPreGNFList = TestList [  TestLabel "Stop is unchanged" testStop
                          , TestLabel "Single ActionPref is unchanged " testActPref
                          , TestLabel "Multi ActionPref is unchanged" testActPref2
                          , TestLabel "ActionPref is translated recursively" testActPref3
                          , TestLabel "ProcInst is unchanged" testProcInst
                          , TestLabel "Choice2 at top-level is unchanged" testChoice1
                          , TestLabel "Choice3 at top-level is unchanged 2" testChoice2
                          , TestLabel "choice at lower level is substituted" testChoice3
                          , TestLabel "choice at lower level is substituted 2" testChoice4
                          , TestLabel "choice at lower level is substituted (recursively)" testChoice5
                          , TestLabel "guard with stop" testGuardStop
                          , TestLabel "guard with procInst" testGuardProcInst
                          , TestLabel "guard with action prefix" testGuardActionPref
                          , TestLabel "action prefix with guard and procInst" testActionPrefGuardProcInst
                          , TestLabel "guard with choice" testGuardChoice

                         ]
