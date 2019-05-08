{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE ViewPatterns        #-}
module TestLPEInterrupt
(
testLPEInterruptList
)
where

import LPE
import TranslatedProcDefs

import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text         as T

import SortId
import TxsDefs
import VarId
import Constant
import ValExpr

import LPEfunc
import TestDefinitions

-- import Debug.Trace
-- import TxsShow

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

-- runs lpeHide, but returns only the relevant translated ProcDef
lpeInterruptTestWrapper :: BExpr -> TranslatedProcDefs -> ProcDefs -> Maybe (BExpr, ProcDef)
lpeInterruptTestWrapper procInst'' translatedProcDefs procDefs' =
  let (procInst'@(TxsDefs.view -> ProcInst procId' _ _), procDefs'') = lpeInterruptFunc procInst'' chanOffers translatedProcDefs procDefs'
      procDef' = fromMaybe
                    (error "lpeInterruptTestWrapper: could not find the procId")
                    (Map.lookup procId' procDefs'') in
  --trace ("\nresult procInst: " ++ show procInst' ++ "\nprocDef': " ++ show procDef') $  
  Just (procInst', procDef')

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------


-- P[]() ::= STOP [>< EXIT
--    with procInst = P[]()
-- becomes 
-- P[](pc$P$interrupt$lhs, pc$P$interrupt$rhs) ::=
--             {} [pc$P$interrupt$rhs == 0] >-> P$interrupt[](0,0)     
-- with procInst = P(0, 0)
        
testStopExit :: Test
testStopExit = TestCase $
--    trace ("\ntestStopExit:\n expected:" ++  pshow (procInst', DefProc procDefPlpe)  ++ 
--             "\ngot: " ++ pshow (res_procInst, DefProc res_procDef)) $
--             -- "\n res_procDefs': " ++ pshow_procDefs res_procDefs') $
      assertBool "testStopExit" (eqProcDef (Just (procInst', procDefPlpe)) (Just (res_procInst, res_procDef) ))
   where
      (res_procInst, res_procDef) = fromMaybe (error "could not find the given procId") (lpeInterruptTestWrapper procInst'' emptyTranslatedProcDefs procDefs')

      procInst'' = procInst procIdP [] []
      procIdP = procIdGen "P" [] []
      procDefP = ProcDef [] [] (interrupt stop (actionPref actOfferExit stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [] [varIdPpcInterruptLHS, varIdPpcInterruptRHS]
      procDefPlpe = ProcDef [] [varIdPpcInterruptLHS, varIdPpcInterruptRHS] (
                                    actionPref  ActOffer {  offers = Set.empty
                                                            , hiddenvars = Set.fromList []
                                                            , constraint = cstrEqual vexprPpcInterruptRHS int0
                                                            } 
                                                (procInst procIdPlpe [] [int0, int0]))
      procInst' = procInst procIdPlpe [] [int0, int0]




-- P[]() ::= EXIT [>< EXIT
--    with procInst = P[]()
-- becomes 
-- P[](pc$P$interrupt$lhs, pc$P$interrupt$rhs) ::= 
--       EXIT [pc$P$interrupt$rhs == 0, pc$P$interrupt$lhs == 0] >-> P$pre1[A](-1, -1)
--   ##  {} [pc$P$interrupt$rhs == 0] >-> P$pre1[](0,0)      
-- P(0,0)
 
testActionPrefExit :: Test
testActionPrefExit = TestCase $
--    trace ("\testActionPrefExit:\n expected:" ++  pshow (procInst', DefProc procDefPlpe)  ++ 
--             "\ngot: " ++ pshow (res_procInst, DefProc res_procDef)) $
--             -- "\n res_procDefs': " ++ pshow_procDefs res_procDefs') $
      assertBool "testActionPrefExit" (eqProcDef (Just (procInst', procDefPlpe)) (Just (res_procInst, res_procDef) ))
   where
      (res_procInst, res_procDef) = fromMaybe (error "could not find the given procId") (lpeInterruptTestWrapper procInst'' emptyTranslatedProcDefs procDefs')

      procInst'' = procInst procIdP [] []
      procIdP = procIdGen "P" [] []
      procDefP = ProcDef [] [] (interrupt (actionPref actOfferExit stop) (actionPref actOfferExit stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [] [varIdPpcInterruptLHS, varIdPpcInterruptRHS]
      procDefPlpe = ProcDef [] [varIdPpcInterruptLHS, varIdPpcInterruptRHS] (
                                    choice $ Set.fromList [

                                          actionPref  actOfferExit {  hiddenvars = Set.fromList []
                                                                  ,   constraint =  cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprPpcInterruptRHS int0)
                                                                                                                        (cstrEqual vexprPpcInterruptLHS int0)
                                                                                                                        (cstrConst (Cbool False))
                                                                                    ])
                                                } 
                                                (procInst procIdPlpe [] [intMin1, intMin1]),

                                          actionPref  ActOffer {  offers = Set.empty
                                                                  , hiddenvars = Set.fromList []
                                                                  , constraint = cstrEqual vexprPpcInterruptRHS int0
                                                                  } 
                                                (procInst procIdPlpe [] [int0, int0]) 
                                          ])
      procInst' = procInst procIdPlpe [] [int0, int0]


-- P[]() ::= A >-> EXIT [>< EXIT
--    with procInst = P[]()
-- becomes 
-- P[](pc$P$interrupt$lhs, pc$P$interrupt$rhs) ::= 
--       EXIT [pc$P$interrupt$rhs == 0, pc$P$interrupt$lhs == 1] >-> P[](-1, -1)
--   ##  A [pc$P$interrupt$rhs == 0, pc$P$interrupt$lhs == 0] >-> P[](1, pc$P$interrupt$rhs)
--   ##  {} [pc$P$interrupt$rhs == 0] >-> P[](0,0)      
-- P(0,0)
 
testActionPrefExitExit :: Test
testActionPrefExitExit = TestCase $
--    trace ("\testActionPrefExit:\n expected:" ++  pshow (procInst', DefProc procDefPlpe)  ++ 
--             "\ngot: " ++ pshow (res_procInst, DefProc res_procDef)) $
--             -- "\n res_procDefs': " ++ pshow_procDefs res_procDefs') $
      assertBool "testActionPrefExit" (eqProcDef (Just (procInst', procDefPlpe)) (Just (res_procInst, res_procDef) ))
   where
      (res_procInst, res_procDef) = fromMaybe (error "could not find the given procId") (lpeInterruptTestWrapper procInst'' emptyTranslatedProcDefs procDefs')
      procInst'' = procInst procIdP [] []
      procIdP = procIdGen "P" [] []
      procDefP = ProcDef [] [] (interrupt (actionPref actOfferA (actionPref actOfferExit stop)) (actionPref actOfferExit stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [] [varIdPpcInterruptLHS, varIdPpcInterruptRHS]
      procDefPlpe = ProcDef [] [varIdPpcInterruptLHS, varIdPpcInterruptRHS] (
                                    choice $ Set.fromList [
                                          actionPref  actOfferExit {  hiddenvars = Set.fromList []
                                                                  ,   constraint = cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprPpcInterruptRHS int0)
                                                                                                                        (cstrEqual vexprPpcInterruptLHS int1)
                                                                                                                        (cstrConst (Cbool False))
                                                                                    ])
                                                } 
                                                (procInst procIdPlpe [] [intMin1, intMin1]),
                                          
                                          actionPref  actOfferA {  hiddenvars = Set.fromList []
                                                                  ,   constraint =  cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprPpcInterruptRHS int0)
                                                                                                                        (cstrEqual vexprPpcInterruptLHS int0)
                                                                                                                        (cstrConst (Cbool False))
                                                                                    ])
                                                } 
                                                (procInst procIdPlpe [] [int1, vexprPpcInterruptRHS]),

                                          actionPref  ActOffer {  offers = Set.empty
                                                                  , hiddenvars = Set.fromList []
                                                                  , constraint = cstrEqual vexprPpcInterruptRHS int0
                                                                  } 
                                                (procInst procIdPlpe [] [int0, int0]) 
                                          ])
      procInst' = procInst procIdPlpe [] [int0, int0]

-- P[A]() ::= EXIT [>< A >-> EXIT 
--    with procInst = P[]()
-- becomes 
-- P[A](P$lhs$pc$P$interrupt$lhs, P$rhs$pc$P$interrupt$rhs, P$rhs$P$interrupt$rhs$pre1$pc$P$interrupt$rhs$pre1$lhs ) ::= 
--       {} [P$rhs$pc$P$interrupt$rhs == 1, P$rhs$P$interrupt$rhs$pre1$pc$P$interrupt$rhs$pre1$lhs == 1] >-> P[A](0, 0, ANY) 
--   ##  A [P$rhs$pc$P$interrupt$rhs == 0] >-> P[A](-1, 1, 1) 
--   ##  A [P$rhs$pc$P$interrupt$rhs == 1, P$rhs$P$interrupt$rhs$pre1$pc$P$interrupt$rhs$pre1$lhs == 0] >-> P[A](-1, 1, 1)      // dead branch due to an extra unfolding of a ProcDef during translation
--   ##  EXIT [P$rhs$pc$P$interrupt$rhs == 0,  P$lhs$pc$P$interrupt$lhs == 0] >-> P[A](-1, -1, P$rhs$P$interrupt$rhs$pre1$pc$P$interrupt$rhs$pre1$lhs)
-- P[A](0,0, ANY)
 
 
testExitActionPref :: Test
testExitActionPref = TestCase $
--    trace ("\testExitActionPref:\n expected:" ++  pshow (procInst', DefProc procDefPlpe)  ++ 
--             "\ngot: " ++ pshow (res_procInst, DefProc res_procDef)) $
--             -- "\n res_procDefs': " ++ pshow_procDefs res_procDefs') $
      assertBool "testActionPrefExit" (eqProcDef (Just (procInst', procDefPlpe)) (Just (res_procInst, res_procDef) ))
   where
      (res_procInst, res_procDef) = fromMaybe (error "could not find the given procId") (lpeInterruptTestWrapper procInst'' emptyTranslatedProcDefs procDefs')

      procInst'' = procInst procIdP [] []
      procIdP = procIdGen "P" [] []
      procDefP = ProcDef [] [] (interrupt (actionPref actOfferExit stop) (actionPref actOfferA (actionPref actOfferExit stop)) )
      procDefs' = Map.fromList  [  (procIdP, procDefP)]


      varIdPpcLHS' :: VarId
      varIdPpcLHS' = VarId (T.pack "P$lhs$pc$P$interrupt$lhs") 133 sortIdInt
      varIdPpcRHS' :: VarId
      varIdPpcRHS' = VarId (T.pack "P$rhs$pc$P$interrupt$rhs") 134 sortIdInt
      varIdPpcRHS2' :: VarId
      varIdPpcRHS2' = VarId (T.pack "P$rhs$P$interrupt$rhs$pre1$pc$P$interrupt$rhs$pre1$lhs") 135 sortIdInt
      
      vexprPpcLHS' :: VExpr
      vexprPpcLHS' = cstrVar varIdPpcLHS'
      vexprPpcRHS' :: VExpr
      vexprPpcRHS' = cstrVar varIdPpcRHS'
      vexprPpcRHS2' :: VExpr
      vexprPpcRHS2' = cstrVar varIdPpcRHS2'

      procIdPlpe = procIdGen "P" [] [varIdPpcLHS', varIdPpcRHS', varIdPpcRHS2']
      procDefPlpe = ProcDef [] [varIdPpcLHS', varIdPpcRHS', varIdPpcRHS2'] (
                                    choice $ Set.fromList [
                                          -- {} [P$rhs$pc$P$interrupt$rhs == 1, P$rhs$P$interrupt$rhs$pre1$pc$P$interrupt$rhs$pre1$lhs == 1] >-> P[A](0, 0, ANY) 
                                          actionPref  ActOffer {  offers = Set.empty
                                                                  , hiddenvars = Set.fromList []
                                                                  , constraint =  cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprPpcRHS' int1)
                                                                                                                  (cstrEqual vexprPpcRHS2' int1)
                                                                                                                  (cstrConst (Cbool False))
                                                                              ])
                                                                  } 
                                                (procInst procIdPlpe [] [int0, int0, anyInt]), 

                                          --   ##  A [P$rhs$pc$P$interrupt$rhs == 0] >-> P[A](-1, 1, 1) 
                                          actionPref actOfferA { hiddenvars = Set.fromList []
                                                               , constraint = cstrEqual vexprPpcRHS' int0
                                                               }
                                                     (procInst procIdPlpe [] [intMin1, int1, int1]),

                                          
                                          --   ##  A [P$rhs$pc$P$interrupt$rhs == 1, P$rhs$P$interrupt$rhs$pre1$pc$P$interrupt$rhs$pre1$lhs == 0] >-> P[A](-1, 1, 1)      // dead branch due to an extra unfolding of a ProcDef during translation
                                          actionPref  actOfferA {  hiddenvars = Set.fromList []
                                                                  ,   constraint =  cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprPpcRHS' int1)
                                                                                                                        (cstrEqual vexprPpcRHS2' int0)
                                                                                                                        (cstrConst (Cbool False))
                                                                                    ])
                                                } 
                                                (procInst procIdPlpe [] [intMin1, int1, int1]),

                                          --   ##  EXIT [P$rhs$pc$P$interrupt$rhs == 0,  P$lhs$pc$P$interrupt$lhs == 0] >-> P[A](-1, -1, P$rhs$P$interrupt$rhs$pre1$pc$P$interrupt$rhs$pre1$lhs)
                                          actionPref  actOfferExit {  hiddenvars = Set.fromList []
                                                                  ,   constraint = cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprPpcRHS' int0)
                                                                                                                        (cstrEqual vexprPpcLHS' int0)
                                                                                                                        (cstrConst (Cbool False))
                                                                                    ])
                                                } 
                                                (procInst procIdPlpe [] [intMin1, intMin1, vexprPpcRHS2'])
                                    ])
      procInst' = procInst procIdPlpe [] [int0, int0, anyInt]



        
----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testLPEInterruptList :: Test
testLPEInterruptList = TestList [  
                              TestLabel "testStopExit" testStopExit
                        ,     TestLabel "testActionPrefExit" testActionPrefExit
                        ,     TestLabel "testActionPrefExitExit" testActionPrefExitExit
                        ,     TestLabel "testExitActionPref" testExitActionPref

                        ]
