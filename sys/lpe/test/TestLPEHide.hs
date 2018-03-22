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

import StdTDefs (chanIdIstep)
import Debug.Trace

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

-- runs lpeHide, but returns only the relevant translated ProcDef
lpeHideTestWrapper :: BExpr -> TranslatedProcDefs -> ProcDefs -> Maybe (BExpr, ProcDef)
lpeHideTestWrapper procInst translatedProcDefs procDefs =
  let (procInst'@(ProcInst procId' _ _), procDefs') = lpeHideFunc procInst chanOffers translatedProcDefs procDefs
      procDef' = case Map.lookup procId' procDefs' of
                    Just procDef   -> procDef
                    Nothing        -> error "lpeHideTestWrapper: could not find the procId" in
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
actOfferA   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdA0
                                              , chanoffers = []
                                        }
                        , constraint = cstrConst (Cbool True)
            }


-- action: A!1
actOfferA1   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Exclam vexpr1]
                                        }
                        , constraint = cstrConst (Cbool True)
            }

-- action: A?x
actOfferAx   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Quest varIdX]
                                        }
                        , constraint = cstrConst (Cbool True)
            }
-- action: A!x
actOfferAExclamX   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Exclam vexprX]
                                        }
                        , constraint = cstrConst (Cbool True)
            }

-- action: B!1
actOfferB1   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Exclam vexpr1]
                                        }
                        , constraint = cstrConst (Cbool True)
            }

-- action: B?x
actOfferBx   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Quest varIdX]
                                        }
                        , constraint = cstrConst (Cbool True)
            }

-- action: C?x
actOfferCx   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdC
                                              , chanoffers = [Quest varIdX]
                                        }
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


-- P[A]() := HIDE [] IN EXIT NI 
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) := ISTEP [pc$P == 0] >-> EXIT
-- with procInst = P[A](0)




-- P[A]() := HIDE [] IN STOP NI 
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) :=                    // technically: Choice []
-- with procInst = P[A](0)

testStop1 :: Test
testStop1 = TestCase $
   assertBool "simple STOP" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst emptyTranslatedProcDefs procDefs))
   where
      procInst = ProcInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (Hide [] Stop)
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (Choice [])
      procInst' = ProcInst procIdPlpe [chanIdA] [int0]



-- P[A]() := HIDE [A] IN STOP NI 
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) :=                    // technically: Choice []
-- with procInst = P[A](0)

testStop2 :: Test
testStop2 = TestCase $
   assertBool "simple STOP 2" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst emptyTranslatedProcDefs procDefs))
   where
      procInst = ProcInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (Hide [chanIdA] Stop)
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (Choice [])
      procInst' = ProcInst procIdPlpe [chanIdA] [int0]



-- P[A]() := HIDE [] IN A -> STOP NI 
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) := A -> P[A](-1)
-- with procInst = P[A](0)

testActionPref1 :: Test
testActionPref1 = TestCase $
   assertBool "actionPref 1" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst emptyTranslatedProcDefs procDefs))
   where
      procInst = ProcInst procIdP [chanIdA0] []
      procIdP = procIdGen "P" [chanIdA0] []
      procDefP = ProcDef [chanIdA0] [] (Hide [] (ActionPref actOfferA Stop))
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [chanIdA0] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA0] [varIdPcP] (ActionPref                         
                                                      -- action: A [pc$P == 0]
                                                      ActOffer {  offers = Set.singleton
                                                                              Offer { chanid = chanIdA0
                                                                                    , chanoffers = []
                                                                              }
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (ProcInst procIdPlpe [chanIdA0] [vexprMin1]))
      procInst' = ProcInst procIdPlpe [chanIdA0] [int0]


-- P[A]() := HIDE [A] IN A -> STOP NI 
-- with procInst = P[A]()
-- becomes
-- LPE_P[A](pc$P) := ISTEP -> STOP
-- with procInst = P[A](0)

testActionPref2 :: Test
testActionPref2 = TestCase $
   assertBool "actionPref 2" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst emptyTranslatedProcDefs procDefs))
   where
      procInst = ProcInst procIdP [chanIdA0] []
      procIdP = procIdGen "P" [chanIdA0] []
      procDefP = ProcDef [chanIdA0] [] (Hide [chanIdA0] (ActionPref actOfferA Stop))
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [chanIdA0] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA0] [varIdPcP] (ActionPref                         
                                                      -- action: ISTEP [pc$P == 0]
                                                      ActOffer {  offers = Set.singleton
                                                                              Offer { chanid = chanIdIstep
                                                                                    , chanoffers = []
                                                                              }
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (ProcInst procIdPlpe [chanIdA0] [vexprMin1]))
      procInst' = ProcInst procIdPlpe [chanIdA0] [int0]




-- P[A]() := HIDE [] IN A?x -> STOP NI 
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) := A?x -> P[A](-1)
-- with procInst = P[A](0)

testActionPref3 :: Test
testActionPref3 = TestCase $
   assertBool "actionPref 3" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst emptyTranslatedProcDefs procDefs))
   where
      procInst = ProcInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (Hide [] (ActionPref actOfferAx Stop))
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (ActionPref                         
                                                      -- action: A?x [pc$P == 0]
                                                      ActOffer {  offers = Set.singleton
                                                                              Offer { chanid = chanIdA
                                                                                    , chanoffers = [Quest varIdA1]
                                                                              }
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (ProcInst procIdPlpe [chanIdA] [vexprMin1]))
      procInst' = ProcInst procIdPlpe [chanIdA] [int0]


-- P[A]() := HIDE [A] IN A?x -> STOP NI 
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) := ISTEP ?x -> P[A](-1)
-- with procInst = P[A](0)

testActionPref4 :: Test
testActionPref4 = TestCase $
   assertBool "actionPref 3" (eqProcDef (Just (procInst', procDefPlpe)) (lpeHideTestWrapper procInst emptyTranslatedProcDefs procDefs))
   where
      procInst = ProcInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (Hide [chanIdA] (ActionPref actOfferAx Stop))
      procDefs = Map.fromList  [  (procIdP, procDefP)]


      -- chanIdIstep :: ChanId
      -- chanIdIstep = ChanId "ISTEP" 902 []
      chanIdIstepX :: ChanId
      chanIdIstepX = ChanId (T.pack "ISTEP") 902 []
                                                                                                      
      procIdPlpe = procIdGen "P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (ActionPref                         
                                                      -- action: ISTEP ?x [pc$P == 0]
                                                      ActOffer {  offers = Set.singleton
                                                                              Offer { chanid = chanIdIstepX
                                                                                    , chanoffers = [Quest varIdA1]
                                                                              }
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (ProcInst procIdPlpe [chanIdA] [vexprMin1]))
      procInst' = ProcInst procIdPlpe [chanIdA] [int0]



        
----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testLPEHideList :: Test
testLPEHideList = TestList [  TestLabel "simple STOP" testStop1
                            , TestLabel "simple STOP 2" testStop2

                            , TestLabel "actionPref 1" testActionPref1
                            , TestLabel "actionPref 2" testActionPref2
                            , TestLabel "actionPref 3" testActionPref3
                            , TestLabel "actionPref 4" testActionPref4
                        ]
