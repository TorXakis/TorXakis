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

{-# LANGUAGE ViewPatterns        #-}

module TestLPEHide
(
testLPEHideList
)
where

import Id
import LPE
import TranslatedProcDefs

import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text         as T

import TxsDefs
import TxsShow
import ProcId
import ChanId
import SortId
import VarId
import ConstDefs
import ValExpr

import LPEfunc

import StdTDefs (chanIdIstep)
import Debug.Trace

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

-- runs lpeHide, but returns only the relevant translated ProcDef
lpeHideTestWrapper :: BExpr -> TranslatedProcDefs -> ProcDefs -> Maybe (BExpr, ProcDef)
lpeHideTestWrapper procInst'' translatedProcDefs procDefs =
  let (procInst'@(TxsDefs.view -> ProcInst procId' _ _), procDefs') = lpeHideFunc procInst'' chanOffers translatedProcDefs procDefs
      procDef' = fromMaybe
                    (error "lpeHideTestWrapper: could not find the procId")
                    (Map.lookup procId' procDefs') in
  --trace ("\nresult procInst: " ++ show procInst' ++ "\nprocDef': " ++ show procDef') $  
  Just (procInst', procDef')



procIdGen :: String -> [ChanId] -> [VarId] -> ProcId
procIdGen name chans vars = ProcId   {    ProcId.name       = T.pack name
                                        , ProcId.unid       = 111
                                        , ProcId.procchans  = chans
                                        , ProcId.procvars   = vars
                                        , ProcId.procexit   = NoExit
                                    }

varIdX = VarId (T.pack "x") 33 intSort
varIdY = VarId (T.pack "y") 34 intSort
varIdZ = VarId (T.pack "z") 34 intSort
varIdA1 = VarId (T.pack "A$1") 34 intSort
varIdB1 = VarId (T.pack "B$1") 34 intSort

vexprX = cstrVar varIdX
vexprA1 = cstrVar varIdA1
vexprB1 = cstrVar varIdB1

vexpr0 = cstrConst (Cint 0)
vexpr1 = cstrConst (Cint 1)
vexpr2 = cstrConst (Cint 2)
vexprMin1 = cstrConst (Cint (-1))

int0 = cstrConst (Cint 0)
int1 = cstrConst (Cint 1)
int2 = cstrConst (Cint 2)
varIdPcP = VarId (T.pack "pc$P") 0 intSort
vexprPcP = cstrVar varIdPcP


-- action: A
actOfferA :: ActOffer
actOfferA   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdA0
                                              , chanoffers = []
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

-- action: A?x
actOfferAx :: ActOffer
actOfferAx   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Quest varIdX]
                                        }
                        , hiddenvars = Set.empty
                        , constraint = cstrConst (Cbool True)
            }
-- action: A!x
actOfferAExclamX :: ActOffer
actOfferAExclamX   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Exclam vexprX]
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

-- action: C?x
actOfferCx :: ActOffer
actOfferCx   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdC
                                              , chanoffers = [Quest varIdX]
                                        }
                        , hiddenvars = Set.empty
                        , constraint = cstrConst (Cbool True)
            }



chanOffers = Map.fromList [   ((T.pack "A", 1), VarId (T.pack "A$1") 34 intSort)
                            , ((T.pack "B", 1), VarId (T.pack "B$1") 34 intSort)
                        ]


-- sorts, chanIds
intSort = SortId {  SortId.name = T.pack "Int"
                  , SortId.unid = 1}

chanIdA0 = ChanId    { ChanId.name = T.pack "A"
                        , ChanId.unid = 2
                        , ChanId.chansorts = []
                        }                  
chanIdA = ChanId    { ChanId.name = T.pack "A"
                    , ChanId.unid = 2
                    , ChanId.chansorts = [intSort]
                    }
chanIdB = ChanId    { ChanId.name = T.pack "B"
                    , ChanId.unid = 3
                    , ChanId.chansorts = [intSort]
                    }
chanIdC = ChanId    { ChanId.name = T.pack "C"
                    , ChanId.unid = 4
                    , ChanId.chansorts = [intSort]
                    }
anyInt = cstrConst $ Cany intSort

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
   assertBool "simple STOP" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst'' emptyTranslatedProcDefs procDefs))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (hide Set.empty stop)
      procDefs = Map.fromList  [  (procIdP, procDefP)]

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
   assertBool "simple STOP 2" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst'' emptyTranslatedProcDefs procDefs))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (hide (Set.singleton chanIdA) stop)
      procDefs = Map.fromList  [  (procIdP, procDefP)]

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
   assertBool "actionPref 1" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst'' emptyTranslatedProcDefs procDefs))
   where
      procInst'' = procInst procIdP [chanIdA0] []
      procIdP = procIdGen "P" [chanIdA0] []
      procDefP = ProcDef [chanIdA0] [] (hide Set.empty (actionPref actOfferA stop))
      procDefs = Map.fromList  [  (procIdP, procDefP)]

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
                                                      (procInst procIdPlpe [chanIdA0] [vexprMin1]))
      procInst' = procInst procIdPlpe [chanIdA0] [int0]


-- P[A]() := HIDE [A] IN A -> STOP NI 
-- with procInst = P[A]()
-- becomes
-- LPE_P[A](pc$P) := {} -> STOP
-- with procInst = P[A](0)

testActionPref2 :: Test
testActionPref2 = TestCase $
   assertBool "actionPref 2" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst'' emptyTranslatedProcDefs procDefs))
   where
      procInst'' = procInst procIdP [chanIdA0] []
      procIdP = procIdGen "P" [chanIdA0] []
      procDefP = ProcDef [chanIdA0] [] (hide (Set.singleton chanIdA0) (actionPref actOfferA stop))
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [chanIdA0] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA0] [varIdPcP] (actionPref                         
                                                      -- action: {} [pc$P == 0]
                                                      ActOffer {  offers = Set.empty
                                                                  , hiddenvars = Set.empty
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (procInst procIdPlpe [chanIdA0] [vexprMin1]))
      procInst' = procInst procIdPlpe [chanIdA0] [int0]




-- P[A]() := HIDE [] IN A?x -> STOP NI 
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) := A?x -> P[A](-1)
-- with procInst = P[A](0)

testActionPref3 :: Test
testActionPref3 = TestCase $
   assertBool "actionPref 3" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst'' emptyTranslatedProcDefs procDefs))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (hide Set.empty (actionPref actOfferAx stop))
      procDefs = Map.fromList  [  (procIdP, procDefP)]

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
                                                      (procInst procIdPlpe [chanIdA] [vexprMin1]))
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
      res =  lpeHideTestWrapper procInst'' emptyTranslatedProcDefs procDefs
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (hide (Set.singleton chanIdA) (actionPref actOfferAx stop))
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdA1' = VarId (T.pack "A$1_3") 34 intSort
                                                                                          
      procIdPlpe = procIdGen "P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (actionPref                         
                                                      -- action: {} {A1_1} [pc$P == 0]
                                                      ActOffer {  offers = Set.empty
                                                                  , hiddenvars = Set.fromList [varIdA1']
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (procInst procIdPlpe [chanIdA] [vexprMin1]))
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
