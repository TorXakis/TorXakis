{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE ViewPatterns        #-}
module TestLPEHide
(
testLPEHideList
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
import ValExpr

import LPEfunc
import TestDefinitions
---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

-- runs lpeHide, but returns only the relevant translated ProcDef
lpeHideTestWrapper :: BExpr -> TranslatedProcDefs -> ProcDefs -> Maybe (BExpr, ProcDef)
lpeHideTestWrapper procInst'' translatedProcDefs procDefs' =
  let (procInst'@(TxsDefs.view -> ProcInst procId' _ _), procDefs'') = lpeHideFunc procInst'' chanOffers translatedProcDefs procDefs'
      procDef' = fromMaybe
                    (error "lpeHideTestWrapper: could not find the procId")
                    (Map.lookup procId' procDefs'') in
  --trace ("\nresult procInst: " ++ show procInst' ++ "\nprocDef': " ++ show procDef') $  
  Just (procInst', procDef')

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------


-- P[A]() := HIDE [] IN STOP NI 
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) :=                    // technically: choice Set.empty
-- with procInst = P[A](0)

testStop1 :: Test
testStop1 = TestCase $
   assertBool "simple STOP" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst'' emptyTranslatedProcDefs procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (hide Set.empty stop)
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (choice Set.empty)
      procInst' = procInst procIdPlpe [chanIdA] [int0]



-- P[A]() := HIDE [A] IN STOP NI 
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) :=                    // technically: choice Set.empty
-- with procInst = P[A](0)

testStop2 :: Test
testStop2 = TestCase $
   assertBool "simple STOP 2" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst'' emptyTranslatedProcDefs procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (hide (Set.singleton chanIdA) stop)
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (choice Set.empty)
      procInst' = procInst procIdPlpe [chanIdA] [int0]



-- P[A]() := HIDE [] IN A -> STOP NI 
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) := A -> P[A](-1)
-- with procInst = P[A](0)

testActionPref1 :: Test
testActionPref1 = TestCase $
   assertBool "actionPref 1" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst'' emptyTranslatedProcDefs procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA0] []
      procIdP = procIdGen "P" [chanIdA0] []
      procDefP = ProcDef [chanIdA0] [] (hide Set.empty (actionPref actOfferA stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [chanIdA0] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA0] [varIdPcP] (actionPref                         
                                                      -- action: A [pc$P == 0]
                                                      ActOffer {  offers = Set.singleton
                                                                              Offer { chanid = chanIdA0
                                                                                    , chanoffers = []
                                                                              }
                                                                  , hiddenvars = Set.empty
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (procInst procIdPlpe [chanIdA0] [intMin1]))
      procInst' = procInst procIdPlpe [chanIdA0] [int0]


-- P[A]() := HIDE [A] IN A -> STOP NI 
-- with procInst = P[A]()
-- becomes
-- LPE_P[A](pc$P) := {} -> STOP
-- with procInst = P[A](0)

testActionPref2 :: Test
testActionPref2 = TestCase $
   assertBool "actionPref 2" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst'' emptyTranslatedProcDefs procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA0] []
      procIdP = procIdGen "P" [chanIdA0] []
      procDefP = ProcDef [chanIdA0] [] (hide (Set.singleton chanIdA0) (actionPref actOfferA stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [chanIdA0] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA0] [varIdPcP] (actionPref                         
                                                      -- action: {} [pc$P == 0]
                                                      ActOffer {  offers = Set.empty
                                                                  , hiddenvars = Set.empty
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (procInst procIdPlpe [chanIdA0] [intMin1]))
      procInst' = procInst procIdPlpe [chanIdA0] [int0]




-- P[A]() := HIDE [] IN A?x -> STOP NI 
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) := A?x -> P[A](-1)
-- with procInst = P[A](0)

testActionPref3 :: Test
testActionPref3 = TestCase $
   assertBool "actionPref 3" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst'' emptyTranslatedProcDefs procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (hide Set.empty (actionPref actOfferAx stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (actionPref                         
                                                      -- action: A?x [pc$P == 0]
                                                      ActOffer {  offers = Set.singleton
                                                                              Offer { chanid = chanIdA
                                                                                    , chanoffers = [Quest varIdA1]
                                                                              }
                                                                  , hiddenvars = Set.empty
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (procInst procIdPlpe [chanIdA] [intMin1]))
      procInst' = procInst procIdPlpe [chanIdA] [int0]


-- P[A]() := HIDE [A] IN A?x -> STOP NI 
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) := {} [pc$P == 0] {hiddenvars: A1_1} -> P[A](-1)
-- with procInst = P[A](0)

testActionPref4 :: Test
testActionPref4 = TestCase $
   assertBool "actionPref 3" (eqProcDef (Just (procInst', procDefPlpe)) res)
   where
      res =  lpeHideTestWrapper procInst'' emptyTranslatedProcDefs procDefs'
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (hide (Set.singleton chanIdA) (actionPref actOfferAx stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      varIdA1' = VarId (T.pack "A$1_3") 4034 sortIdInt
                                                                                          
      procIdPlpe = procIdGen "P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (actionPref                         
                                                      -- action: {} {A1_1} [pc$P == 0]
                                                      ActOffer {  offers = Set.empty
                                                                  , hiddenvars = Set.fromList [varIdA1']
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (procInst procIdPlpe [chanIdA] [intMin1]))
      procInst' = procInst procIdPlpe [chanIdA] [int0]


        
----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testLPEHideList :: Test
testLPEHideList = TestList [  
                              TestLabel "simple STOP" testStop1
                            , TestLabel "simple STOP 2" testStop2

                            , TestLabel "actionPref 1" testActionPref1
                            , TestLabel "actionPref 2" testActionPref2
                            , TestLabel "actionPref 3" testActionPref3
                            , TestLabel "actionPref 4" testActionPref4
                        ]
