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
module TestLPEPar
(
testLPEParList
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

-- type ProcDefs = Map.Map TxsDefs.ProcId TxsDefs.ProcDef

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

-- runs lpePar, but returns only the relevant translated ProcDef
lpeParTestWrapper :: BExpr -> TranslatedProcDefs -> ProcDefs -> Maybe (BExpr, ProcDef)
lpeParTestWrapper procInst'' translatedProcDefs procDefs =
  let (procInst'@(TxsDefs.view -> ProcInst procId' _ _), procDefs') = lpeParFunc procInst'' translatedProcDefs procDefs
      procDef' = case Map.lookup procId' procDefs' of
                    Just procDef   -> procDef
                    Nothing        -> error "lpeParTestWrapper: could not find the procId" in
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
varIdS = VarId (T.pack "s") 35 intSort
varIdA1 = VarId (T.pack "A$1") 34 intSort
varIdB1 = VarId (T.pack "B$1") 34 intSort

vexprX = cstrVar varIdX
vexprS = cstrVar varIdS
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


-- action: A    // no chanoffers!
actOfferA   = ActOffer {  offers = Set.singleton(
                                        Offer { chanid = chanIdA
                                              , chanoffers = []
                                        })
                        , constraint = cstrConst (Cbool True)
            }

-- action: B    // no chanoffers!
actOfferB   = ActOffer {  offers = Set.singleton(
                                        Offer { chanid = chanIdB
                                              , chanoffers = []
                                        })
                        , constraint = cstrConst (Cbool True)
            }



-- action: A|B    // no chanoffers!
actOfferAB   = ActOffer {  offers = Set.fromList [
                                      Offer { chanid = chanIdA
                                            , chanoffers = []
                                      },
                                      Offer { chanid = chanIdB
                                            , chanoffers = []
                                      }
                                    ]
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

anyInt = cstrConst $ Cany intSort


---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------


-- -------------------------------------------------
-- testing parallel semantics for single-actions
--
-- A >-> STOP |G| A >-> STOP
-- we then test this for G = [], [A], [B], [A,B]
-- -------------------------------------------------

-- the general case for all G
-- P[A,B]() := A >-> STOP |G| A >-> STOP
-- procInst: P[A,B]()
-- becomes
-- P[A,B]() := P$op1[A,B]() |G| P$op2[A,B]()
-- P$op1[A,B]() :=  A >-> STOP
-- P$op2[A,B]() :=  A >-> STOP
-- becomes after op-LPE translation
-- P[A,B]() := P$op1[A,B](0) |G| P$op2[A,B](0)
-- P$op1[A,B](pc$P$op1) :=  A [pc$P$op1 == 0] >->  P$op1[A,B](-1)
-- P$op2[A,B](pc$P$op2) :=  A [pc$P$op2 == 0] >->  P$op2[A,B](-1)
-- becomes in the general case:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // only left side
      --    A [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
      -- // only right side
      -- ## A [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
      -- // both sides
      -- ## A [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
-- with procInst := P[A,B](0,0)
-- testSingleActionGEN :: Test
-- testSingleActionGEN = TestCase $
--    assertBool "test single actions" (procInst', procDefP') (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
--    where
--       procInst'' = procInst procIdP [chanIdA, chanIdB] []
--       procIdP = procIdGen "P" [chanIdA, chanIdB] []
--
--       procDefP = ProcDef [chanIdA, chanIdB] [] (
--             parallel [chanIdA, chanIdB] [
--                 actionPref actOfferA stop,
--                 actionPref actOfferA stop
--               ]
--             )
--       procDefs = Map.fromList  [  (procIdP, procDefP)]
--
--       varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
--       varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
--       vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
--       vexprOp2pcPop2 = cstrVar varIdOp2pcPop2
--
--       -- with procInst := P[A,B](0,0)
--       procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
--       procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
--                       (choice [
--                           -- // only left side
--                           -- A [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
--                           (actionPref
--                             ActOffer {  offers = Set.singleton(
--                                                       Offer { chanid = chanIdA
--                                                             , chanoffers = []
--                                                       })
--                                                   , constraint = cstrEqual vexprOp1pcPop1 int0
--                                       }
--                             (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcPop2]))
--                       , -- // only right side
--                         -- ## A [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
--                         (actionPref
--                           ActOffer {  offers = Set.singleton(
--                                                     Offer { chanid = chanIdA
--                                                           , chanoffers = []
--                                                     })
--                                                 , constraint = cstrEqual vexprOp2pcPop2 int0
--                                     }
--                           (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcPop1, vexprMin1]))
--                       , -- // both sides
--                         -- ## A [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
--                         (actionPref
--                           ActOffer {  offers = Set.singleton(
--                                                     Offer { chanid = chanIdA
--                                                           , chanoffers = []
--                                                     })
--                                                 , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcPop1 int0
--                                                                                       , cstrEqual vexprOp2pcPop2 int0
--                                                                 ])
--
--
--                                     }
--                           (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1]))
--                       ])
--
--       procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]


-- CASE: A >-> STOP |[]| A >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // only left side
      --    A [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
      -- // only right side
      -- ## A [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
      -- // both sides: NO
-- with procInst := P[A,B](0,0)
testSingleAction1 :: Test
testSingleAction1 = TestCase $
   assertBool "test single actions" $ eqProcDef (Just (procInst', procDefP')) (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [] [
                actionPref actOfferA stop,
                actionPref actOfferA stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
                      (choice [
                          -- // only left side
                          -- A [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = []
                                                      })
                                                  , constraint = cstrEqual vexprOp1pcPop1 int0
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcPop2]))
                      , -- // only right side
                        -- ## A [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
                        (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    })
                                                , constraint = cstrEqual vexprOp2pcPop2 int0
                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcPop1, vexprMin1]))
                      ])

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]

-- CASE: A >-> STOP |[A]| A >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // both sides
      -- ## A [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
-- with procInst := P[A,B](0,0)
testSingleAction2 :: Test
testSingleAction2 = TestCase $
   assertBool "test single actions" $ eqProcDef (Just (procInst', procDefP')) (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdA] [
                actionPref actOfferA stop,
                actionPref actOfferA stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]

                        -- // both sides
                        -- ## A [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
                        (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    })
                                                , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcPop1 int0
                                                                                      , cstrEqual vexprOp2pcPop2 int0
                                                                ])


                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1]))


      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]


-- CASE: A >-> STOP |[B]| A >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // only left side
      --    A [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
      -- // only right side
      -- ## A [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
-- with procInst := P[A,B](0,0)
testSingleAction3 :: Test
testSingleAction3 = TestCase $
   assertBool "test single actions" $ eqProcDef (Just (procInst', procDefP')) (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdB] [
                actionPref actOfferA stop,
                actionPref actOfferA stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
                      (choice [
                          -- // only left side
                          -- A [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = []
                                                      })
                                                  , constraint = cstrEqual vexprOp1pcPop1 int0
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcPop2]))
                      , -- // only right side
                        -- ## A [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
                        (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    })
                                                , constraint = cstrEqual vexprOp2pcPop2 int0
                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcPop1, vexprMin1]))

                      ])

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]




-- CASE: A >-> STOP |[A,B]| A >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // both sides
      -- ## A [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
-- with procInst := P[A,B](0,0)
testSingleAction4 :: Test
testSingleAction4 = TestCase $
   assertBool "test single actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdA, chanIdB] [
                actionPref actOfferA stop,
                actionPref actOfferA stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]

                       -- // both sides
                        -- ## A [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
                        (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    })
                                                , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcPop1 int0
                                                                                      , cstrEqual vexprOp2pcPop2 int0
                                                                ])


                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1]))


      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]










-- -------------------------------------------------
-- testing parallel semantics for single-actions: different actions
--
-- A >-> STOP |G| B >-> STOP
-- we then test this for G = [], [A], [B], [A,B]
-- -------------------------------------------------


-- the general case for all G
-- P[A,B]() := A >-> STOP |G| B >-> STOP
-- procInst: P[A,B]()
-- becomes
-- P[A,B]() := P$op1[A,B]() |G| P$op2[A,B]()
-- P$op1[A,B]() :=  A >-> STOP
-- P$op2[A,B]() :=  B >-> STOP
-- becomes after op-LPE translation
-- P[A,B]() := P$op1[A,B](0) |G| P$op2[A,B](0)
-- P$op1[A,B](pc$P$op1) :=  A [pc$P$op1 == 0] >->  P$op1[A,B](-1)
-- P$op2[A,B](pc$P$op2) :=  B [pc$P$op2 == 0] >->  P$op2[A,B](-1)
-- -- becomes in the general case:
-- -- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
--       -- // only left side
--       --    A [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
--       -- // only right side
--       -- ## B [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
--       -- // both sides: ONLY IF G = []
--       -- ## A|B [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
-- -- with procInst := P[A,B](0,0)
-- testSingleActionDifferentActionsGEN :: Test
-- testSingleActionDifferentActionsGEN = TestCase $
--    assertBool "test single actions, different actions" (procInst', procDefP') (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
--    where
--       procInst'' = procInst procIdP [chanIdA, chanIdB] []
--       procIdP = procIdGen "P" [chanIdA, chanIdB] []
--
--       procDefP = ProcDef [chanIdA, chanIdB] [] (
--             parallel [chanIdA, chanIdB] [
--                 actionPref actOfferA stop,
--                 actionPref actOfferB stop
--               ]
--             )
--       procDefs = Map.fromList  [  (procIdP, procDefP)]
--
--       varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
--       varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
--       vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
--       vexprOp2pcPop2 = cstrVar varIdOp2pcPop2
--
--       -- with procInst := P[A,B](0,0)
--       procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
--       procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
--                       (choice [
--                           -- // only left side
--                           -- A [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
--                           (actionPref
--                             ActOffer {  offers = Set.singleton(
--                                                       Offer { chanid = chanIdA
--                                                             , chanoffers = []
--                                                       })
--                                                   , constraint = cstrEqual vexprOp1pcPop1 int0
--                                       }
--                             (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcPop2]))
--                       , -- // only right side
--                         -- ## B [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
--                         (actionPref
--                           ActOffer {  offers = Set.singleton(
--                                                     Offer { chanid = chanIdB
--                                                           , chanoffers = []
--                                                     })
--                                                 , constraint = cstrEqual vexprOp2pcPop2 int0
--                                     }
--                           (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcPop1, vexprMin1]))
--                       , -- // both sides: ONLY IF G = []
--                         -- ## A|B [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
--                         (actionPref
--                           ActOffer {  offers = Set.fromList [
--                                                     Offer { chanid = chanIdA
--                                                           , chanoffers = []
--                                                     },
--                                                     Offer { chanid = chanIdB
--                                                           , chanoffers = []
--                                                     }
--                                                     ]
--                                                 , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcPop1 int0
--                                                                                       , cstrEqual vexprOp2pcPop2 int0
--                                                                 ])
--
--
--                                     }
--                           (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1]))
--                       ])
--
--       procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]
--
--


-- CASE: A >-> STOP |[]| B >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // only left side
      --    A [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
      -- // only right side
      -- ## B [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
      -- // both sides: ONLY IF G = []
      -- ## A|B [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
-- with procInst := P[A,B](0,0)
testSingleActionDifferentActions1 :: Test
testSingleActionDifferentActions1 = TestCase $
   assertBool "test single actions, different actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [] [
                actionPref actOfferA stop,
                actionPref actOfferB stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
                      (choice [
                          -- // only left side
                          -- A [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = []
                                                      })
                                                  , constraint = cstrEqual vexprOp1pcPop1 int0
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcPop2]))
                      , -- // only right side
                        -- ## B [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
                        (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdB
                                                          , chanoffers = []
                                                    })
                                                , constraint = cstrEqual vexprOp2pcPop2 int0
                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcPop1, vexprMin1]))
                      , -- // both sides: ONLY IF G = []
                        -- ## A|B [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
                        (actionPref
                          ActOffer {  offers = Set.fromList [
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    },
                                                    Offer { chanid = chanIdB
                                                          , chanoffers = []
                                                    }
                                                    ]
                                                , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcPop1 int0
                                                                                      , cstrEqual vexprOp2pcPop2 int0
                                                                ])


                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1]))
                      ])

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]



-- CASE: A >-> STOP |[A]| B >-> STOP
-- becomes in the general case:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // only right side
      -- ## B [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
-- with procInst := P[A,B](0,0)
testSingleActionDifferentActions2 :: Test
testSingleActionDifferentActions2 = TestCase $
   assertBool "test single actions, different actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdA] [
                actionPref actOfferA stop,
                actionPref actOfferB stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]

                       -- // only right side
                        -- ## B [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
                        (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdB
                                                          , chanoffers = []
                                                    })
                                                , constraint = cstrEqual vexprOp2pcPop2 int0
                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcPop1, vexprMin1]))

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]



-- CASE: A >-> STOP |[B]| B >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // only left side
      --    A [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
-- with procInst := P[A,B](0,0)
testSingleActionDifferentActions3 :: Test
testSingleActionDifferentActions3 = TestCase $
   assertBool "test single actions, different actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdB] [
                actionPref actOfferA stop,
                actionPref actOfferB stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]

                          -- // only left side
                          -- A [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = []
                                                      })
                                                  , constraint = cstrEqual vexprOp1pcPop1 int0
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcPop2]))


      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]




-- CASE: A >-> STOP |[A,B]| B >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=      // STOP/NOTHING!
-- with procInst := P[A,B](0,0)
testSingleActionDifferentActions4 :: Test
testSingleActionDifferentActions4 = TestCase $
   assertBool "test single actions, different actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdA, chanIdB] [
                actionPref actOfferA stop,
                actionPref actOfferB stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
                      (choice [])

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]








-- -------------------------------------------------
-- testing parallel semantics for multi-actions
--
-- A | B >-> STOP |G| A >-> STOP
-- we then test this for G = [], [A], [B], [A,B]
-- -------------------------------------------------

-- the general case for all G
-- P[A,B]() := A | B >-> STOP |G| A >-> STOP
-- procInst: P[A,B]()
-- becomes
-- P[A,B]() := P$op1[A,B]() |G| P$op2[A,B]()
-- P$op1[A,B]() :=  A | B >-> STOP
-- P$op2[A,B]() :=  B >-> STOP
-- becomes after op-LPE translation
-- P[A,B]() := P$op1[A,B](0) |G| P$op2[A,B](0)
-- P$op1[A,B](pc$P$op1) :=  A | B [pc$P$op1 == 0] >->  P$op1[A,B](-1)
-- P$op2[A,B](pc$P$op2) :=  B [pc$P$op2 == 0] >->  P$op2[A,B](-1)
-- becomes in the general case:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // only left side: ONLY IF G = []
      --    A | B [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
      -- // only right side: ONLY IF G = [], [B]
      -- ## A [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
      -- // both sides: ONLY IF G = []
      -- ## A | B [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
-- with procInst := P[A,B](0,0)
-- testMultiActionsGEN :: Test
-- testMultiActionsGEN = TestCase $
--    assertBool "test multi actions" (procInst', procDefP') (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
--    where
--       procInst'' = procInst procIdP [chanIdA, chanIdB] []
--       procIdP = procIdGen "P" [chanIdA, chanIdB] []
--
--       procDefP = ProcDef [chanIdA, chanIdB] [] (
--             parallel [chanIdA, chanIdB] [
--                 actionPref actOfferAB stop,
--                 actionPref actOfferA stop
--               ]
--             )
--       procDefs = Map.fromList  [  (procIdP, procDefP)]
--
--       varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
--       varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
--       vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
--       vexprOp2pcPop2 = cstrVar varIdOp2pcPop2
--
--       -- with procInst := P[A,B](0,0)
--       procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
--       procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
--                       (choice [
--                           -- // only left side: ONLY IF G = []
--                           --    A | B [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
--                           (actionPref
--                             ActOffer {  offers = Set.fromList [
--                                                     Offer { chanid = chanIdA
--                                                           , chanoffers = []
--                                                     },
--                                                     Offer { chanid = chanIdB
--                                                           , chanoffers = []
--                                                     }
--                                                   ]
--                                                   , constraint = cstrEqual vexprOp1pcPop1 int0
--                                       }
--                             (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcPop2]))
--                       , -- // only right side: ONLY IF G = [], [B]
--                         -- ## A [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
--                         (actionPref
--                           ActOffer {  offers = Set.singleton(
--                                                     Offer { chanid = chanIdA
--                                                           , chanoffers = []
--                                                     })
--                                                 , constraint = cstrEqual vexprOp2pcPop2 int0
--                                     }
--                           (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcPop1, vexprMin1]))
--                       , -- // both sides: ONLY IF G = []
--                         -- ## A | B [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
--                         (actionPref
--                           ActOffer {  offers = Set.fromList [
--                                                     Offer { chanid = chanIdA
--                                                           , chanoffers = []
--                                                     },
--                                                     Offer { chanid = chanIdB
--                                                           , chanoffers = []
--                                                     }
--                                                     ]
--                                                 , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcPop1 int0
--                                                                                       , cstrEqual vexprOp2pcPop2 int0
--                                                                 ])
--
--
--                                     }
--                           (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1]))
--                       ])
--
--       procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]
--







-- A | B >-> STOP  |[]| A >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // only left side: ONLY IF G = []
      --    A | B [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
      -- // only right side: ONLY IF G = [], [B]
      -- ## A [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
-- with procInst := P[A,B](0,0)
testMultiActions1 :: Test
testMultiActions1 = TestCase $
   assertBool "test multi actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [] [
                actionPref actOfferAB stop,
                actionPref actOfferA stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
                      (choice [
                          -- // only left side: ONLY IF G = []
                          --    A | B [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
                          (actionPref
                            ActOffer {  offers = Set.fromList [
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    },
                                                    Offer { chanid = chanIdB
                                                          , chanoffers = []
                                                    }
                                                  ]
                                                  , constraint = cstrEqual vexprOp1pcPop1 int0
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcPop2]))
                      , -- // only right side: ONLY IF G = [], [B]
                        -- ## A [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
                        (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    })
                                                , constraint = cstrEqual vexprOp2pcPop2 int0
                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcPop1, vexprMin1]))

                      ])

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]


-- A | B >-> STOP  |[A]| A >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // both sides: ONLY IF G = []
      -- ## A | B [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
-- with procInst := P[A,B](0,0)
testMultiActions2 :: Test
testMultiActions2 = TestCase $
   assertBool "test multi actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdA] [
                actionPref actOfferAB stop,
                actionPref actOfferA stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]

                       -- // both sides: ONLY IF G = []
                        -- ## A | B [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
                        (actionPref
                          ActOffer {  offers = Set.fromList [
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    },
                                                    Offer { chanid = chanIdB
                                                          , chanoffers = []
                                                    }
                                                    ]
                                                , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcPop1 int0
                                                                                      , cstrEqual vexprOp2pcPop2 int0
                                                                ])


                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1]))

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]




-- A | B >-> STOP  |[B]| A >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // only right side: ONLY IF G = [], [B]
      -- ## A [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
-- with procInst := P[A,B](0,0)
testMultiActions3 :: Test
testMultiActions3 = TestCase $
   assertBool "test multi actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdB] [
                actionPref actOfferAB stop,
                actionPref actOfferA stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]

                        -- // only right side: ONLY IF G = [], [B]
                        -- ## A [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
                        (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    })
                                                , constraint = cstrEqual vexprOp2pcPop2 int0
                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcPop1, vexprMin1]))

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]


-- A | B >-> STOP  |[A,B]| A >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=              // STOP / NOTHING!
-- with procInst := P[A,B](0,0)
testMultiActions4 :: Test
testMultiActions4 = TestCase $
   assertBool "test multi actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdA, chanIdB] [
                actionPref actOfferAB stop,
                actionPref actOfferA stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
                      (choice [])

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]





-- A | B >-> STOP  |[]| A | B >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // only left side: ONLY IF G = []
      --    A | B [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
      -- // only right side: ONLY IF G = []
      -- ## A | B [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
-- with procInst := P[A,B](0,0)
testMultiActions5 :: Test
testMultiActions5 = TestCase $
   assertBool "test multi actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [] [
                actionPref actOfferAB stop,
                actionPref actOfferAB stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
                      (choice [
                          -- // only left side: ONLY IF G = []
                          --    A | B [op1$pc$P$op1 == 0] >->  P[A,B](-1, op2$pc$P$op2)
                          (actionPref
                            ActOffer {  offers = Set.fromList [
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    },
                                                    Offer { chanid = chanIdB
                                                          , chanoffers = []
                                                    }
                                                  ]
                                                  , constraint = cstrEqual vexprOp1pcPop1 int0
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcPop2]))
                      , -- // only right side: ONLY IF G = []
                        -- ## A | B [op2$pc$P$op2 == 0] >->  P[A,B](op1$pc$P$op1, -1)
                        (actionPref
                          ActOffer {  offers =Set.fromList [
                                                  Offer { chanid = chanIdA
                                                        , chanoffers = []
                                                  },
                                                  Offer { chanid = chanIdB
                                                        , chanoffers = []
                                                  }
                                                ]
                                                , constraint = cstrEqual vexprOp2pcPop2 int0
                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcPop1, vexprMin1]))
                      ])

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]



-- A | B >-> STOP  |[A]| A | B >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=      // STOP / NOTHING!
-- with procInst := P[A,B](0,0)
testMultiActions6 :: Test
testMultiActions6 = TestCase $
   assertBool "test multi actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdA] [
                actionPref actOfferAB stop,
                actionPref actOfferAB stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
                      (choice [])

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]



-- A | B >-> STOP  |[B]| A | B >-> STOP
-- becomes:
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=      // STOP / NOTHING!
-- with procInst := P[A,B](0,0)
testMultiActions7 :: Test
testMultiActions7 = TestCase $
   assertBool "test multi actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdB] [
                actionPref actOfferAB stop,
                actionPref actOfferAB stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
                      (choice [])

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]




-- A | B >-> STOP  |[A,B]| A | B >-> STOP
-- P[A,B](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // both sides: ONLY IF G = [A,B]
      -- ## A | B [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
-- with procInst := P[A,B](0,0)
testMultiActions8 :: Test
testMultiActions8 = TestCase $
   assertBool "test multi actions"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdA, chanIdB] [
                actionPref actOfferAB stop,
                actionPref actOfferAB stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcPop1, varIdOp2pcPop2]
                        -- // both sides: ONLY IF G = []
                        -- ## A | B [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0] >->  P[A,B](-1, -1)
                        (actionPref
                          ActOffer {  offers = Set.fromList [
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    },
                                                    Offer { chanid = chanIdB
                                                          , chanoffers = []
                                                    }
                                                    ]
                                                , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcPop1 int0
                                                                                      , cstrEqual vexprOp2pcPop2 int0
                                                                ])


                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1]))

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]








-- -------------------------------------------------
-- testing parameter occurence
-- -------------------------------------------------


-- params: by P and Q / R
-- P[A](s) := Q[A](s,1) |[]| R[A](s)
-- Q[A](s,x) := A!s >-> STOP
-- R[A](s) := A!s >-> STOP
-- procInst: P[A](1)
-- becomes
-- P[A](op1$pc$Q, op1$Q$A$s, op1$Q$A$x, op2$pc$R, op2$R$A$s) :=
      -- // only left side:
      --    A?A1 [op1$pc$Q == 0, A1 == op1$Q$A$s] >->  P[A,B](-1, ANY?, ANY?, op2$pc$R, op2$R$A$s)
      -- // only right side:
      -- ## A?A1 [op2$pc$R == 0, A1 == op2$R$A$s] >->  P[A,B](op1$pc$Q, op1$Q$A$s, op1$Q$A$x, -1, ANY?)
      -- // both sides:
      -- ## A?A1 [op1$pc$P$op1 == 0, op2$pc$P$op2 == 0, A1 == op1$Q$A$s, A1 == op2$R$A$s] >->  P[A,B](-1, ANY?, ANY?, -1, ANY?)
-- procInst := P[A](0, s, ANY, 0, s)
testParams :: Test
testParams = TestCase $
   assertBool "test params"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   -- assertEqual "test params"  (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)

   where
      procInst'' = procInst procIdP [chanIdA] [int1]

      procIdP = procIdGen "P" [chanIdA] [varIdS]
      procIdQ = procIdGen "Q" [chanIdA] [varIdS, varIdX]
      procIdR = procIdGen "R" [chanIdA] [varIdS]


      procDefP = ProcDef [chanIdA] [] (
            parallel [] [
                procInst procIdQ [chanIdA] [vexprS, int1],
                procInst procIdR [chanIdA] [vexprS]
              ]
            )

      procDefQ = ProcDef [chanIdA] [varIdS, varIdX] (
                    (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = [Exclam vexprS]
                                                    })
                                                , constraint = cstrConst (Cbool True)
                                    }
                          stop))

      procDefR = ProcDef [chanIdA] [varIdS] (
                    (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = [Exclam vexprS]
                                                    })
                                                , constraint = cstrConst (Cbool True)
                                    }
                          stop))

      procDefs = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)
                                , (procIdR, procDefR)]

      varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
      varIdOp1s = VarId (T.pack "op1$Q$A$s") 35 intSort
      varIdOp1x = VarId (T.pack "op1$Q$A$x") 33 intSort
      varIdOp2pcR = VarId (T.pack "op2$pc$R") 0 intSort
      varIdOp2s = VarId (T.pack "op2$R$A$s") 35 intSort

      vexprOp1pcQ = cstrVar varIdOp1pcQ
      vexprOp1s = cstrVar varIdOp1s
      vexprOp1x = cstrVar varIdOp1x
      vexprOp2pcR = cstrVar varIdOp2pcR
      vexprOp2s = cstrVar varIdOp2s


      -- with procInst := P[A,B](0,0)
      procIdP' = procIdGen "P" [chanIdA] [varIdOp1pcQ, varIdOp1s, varIdOp1x, varIdOp2pcR, varIdOp2s]
      procDefP' = ProcDef [chanIdA] [varIdOp1pcQ, varIdOp1s, varIdOp1x, varIdOp2pcR, varIdOp2s]
                      (choice [
                          -- // only left side:
                          --    A?A1 [op1$pc$Q == 0, A1 == op1$Q$A$s] >->  P[A](-1, ANY?, ANY?, op2$pc$R, op2$R$A$s)
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      })
                                                  , constraint =  cstrAnd (Set.fromList [cstrITE (cstrEqual vexprOp1pcQ int0)
                                                                                             (cstrEqual vexprA1 vexprOp1s)
                                                                                             (cstrConst (Cbool False))
                                                                                        ])
                                      }
                            (procInst procIdP' [chanIdA] [vexprMin1, anyInt, anyInt, vexprOp2pcR, vexprOp2s]))
                        , -- // only right side:
                          -- ## A?A1 [op2$pc$R == 0, A1 == op2$R$A$s] >->  P[A](op1$pc$Q, op1$Q$A$s, op1$Q$A$x, -1, ANY?)
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      })
                                                  , constraint =  cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprOp2pcR int0)
                                                                                             (cstrEqual vexprA1 vexprOp2s)
                                                                                             (cstrConst (Cbool False))
                                                                                        ])
                                      }
                            (procInst procIdP' [chanIdA] [vexprOp1pcQ, vexprOp1s, vexprOp1x, vexprMin1, anyInt]))

                      ])

      -- procInst := P[A](0, s, ANY, 0, s)
      procInst' = procInst procIdP' [chanIdA] [int0, vexprS, int1, int0, vexprS]




-- -------------------------------------------------
-- testing multi-sequences of action prefixes
-- -------------------------------------------------

-- P[A,B]() := Q[A,B]() |G| Q[A,B]()
-- Q[A,B]() := A?x >-> B!x >-> STOP
-- with procInst := P[A,B]()
-- We try this in the following for G = [], [A], [B], [A,B]

-- the general case: generate all possible step combinations (allowed or not)
-- This becomes after operand LPE translation for the operands:
-- P[A,B]() := Q[A,B](0, ANY) |G| Q[A,B](0, ANY)
-- Q[A,B](pc$Q, Q$gnf1$A$B$x) :=    A?A1 [pc$Q == 0] >-> Q[A,B](1, A1)
--                          ## B!B1 [pc$Q == 1, B1 == Q$gnf1$A$B$x] >-> Q[A,B](-1, ANY)
-- becomes after step combination
-- P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, op2$pc$Q, op2$Q$gnf1$A$B$x) :=
--        // only op1
--        A?A1 [op1$pc$Q == 0]                    >-> P[A,B](1, A1, op2$pc$Q, op2$Q$gnf1$A$B$x)
--        B?B1 [op1$pc$Q == 1, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, op2$pc$Q, op2$Q$gnf1$A$B$x)
--                                // note: the right side is still allowed to continue! that's intended behaviour.
--        // only op2
--        A?A1 [op2$pc$Q == 0]                    >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, 1, A1)
--        B?B1 [op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, -1, ANY)
--        // both op1 and op2
--        // 1,1 : only if A \in G, but G could be more
--        A?A1 [op1$pc$Q == 0, op2$pc$Q == 0]                           >-> P[A,B](1, A1, 1, A1)
--        // 1,2 : only if G is empty: |[]|
--        A?A1 | B?B1 [op1$pc$Q == 0, op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](1, A1, -1, ANY)
--        // 2,1 : only if G is empty: |[]|
--        B?B1 | A?A1 [op1$pc$Q == 1, op2$pc$Q == 0, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, 1, A1)
--        // 2,2 : only if B \in G, but G could be more...
--        B?B1 [op1$pc$Q == 1, op2$pc$Q == 1, B1 ==op1$Q$gnf1$A$B$x, B1 ==op2$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, -1, ANY)
--  with procInst = P[A,B](0, ANY, 0, ANY)
testMultiSeqGEN :: Test
testMultiSeqGEN = TestCase $
   assertBool "test multi-sequences"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where

      -- P[A,B]() := Q[A,B]() |G| Q[A,B]()
      -- Q[A,B]() := A?x >-> B!x >-> STOP
      -- with procInst := P[A,B]()
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procIdQ = procIdGen "Q" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [] [
                procInst procIdQ [chanIdA, chanIdB] [],
                procInst procIdQ [chanIdA, chanIdB] []
              ]
            )

      procDefQ = ProcDef [chanIdA, chanIdB] [] (
                    (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = [Quest varIdX]
                                                    })
                                                , constraint = cstrConst (Cbool True)
                                    }
                          (actionPref
                              ActOffer {  offers = Set.singleton(
                                                        Offer { chanid = chanIdB
                                                              , chanoffers = [Exclam vexprX]
                                                        })
                                                    , constraint = cstrConst (Cbool True)
                                        }
                              stop)))
      procDefs = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]

      -- becomes after step combination
      -- P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, op2$pc$Q, op2$Q$gnf1$A$B$x) :=
      varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
      varIdOp1QABx = VarId (T.pack "op1$Q$gnf1$A$B$x") 33 intSort
      varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
      varIdOp2QABx = VarId (T.pack "op2$Q$gnf1$A$B$x") 33 intSort

      vexprOp1pcQ = cstrVar varIdOp1pcQ
      vexprOp1QABx = cstrVar varIdOp1QABx
      vexprOp2pcQ = cstrVar varIdOp2pcQ
      vexprOp2QABx = cstrVar varIdOp2QABx

      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp1QABx, varIdOp2pcQ, varIdOp2QABx]

      --  with procInst = P[A,B](0, ANY, 0, ANY)
      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, anyInt, int0, anyInt]


      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp1QABx, varIdOp2pcQ, varIdOp2QABx]
                      (choice [
                          --        // only op1
                          --        A?A1 [op1$pc$Q == 0]                    >-> P[A,B](1, A1, op2$pc$Q, op2$Q$gnf1$A$B$x)
                          --        B?B1 [op1$pc$Q == 1, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, op2$pc$Q, op2$Q$gnf1$A$B$x)
                          --                                // note: the right side is still allowed to continue! that's intended behaviour.
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      })
                                                  , constraint = cstrEqual vexprOp1pcQ int0
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, vexprOp2pcQ, vexprOp2QABx]))
                          ,(actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdB
                                                            , chanoffers = [Quest varIdB1]
                                                      })
                                                  , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int1
                                                                                        , cstrEqual vexprB1 vexprOp1QABx
                                                                                        ])
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, vexprOp2pcQ, vexprOp2QABx]))


                            --        // only op2
                            --        A?A1 [op2$pc$Q == 0]                    >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, 1, A1)
                            --        B?B1 [op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, -1, ANY)
                            ,(actionPref
                              ActOffer {  offers = Set.singleton(
                                                        Offer { chanid = chanIdA
                                                              , chanoffers = [Quest varIdA1]
                                                        })
                                                    , constraint = cstrEqual vexprOp2pcQ int0
                                        }
                              (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp1QABx, int1, vexprA1]))
                            ,(actionPref
                              ActOffer {  offers = Set.singleton(
                                                        Offer { chanid = chanIdB
                                                              , chanoffers = [Quest varIdB1]
                                                        })
                                                    , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp2pcQ int1
                                                                                          , cstrEqual vexprB1 vexprOp2QABx
                                                                                          ])
                                        }
                              (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp1QABx, vexprMin1, anyInt]))



                              --        // both op1 and op2

                              --        // 1,1 : only if A \in G, but G could be more
                              --        A?A1 [op1$pc$Q == 0, op2$pc$Q == 0]                           >-> P[A,B](1, A1, 1, A1)
                              ,(actionPref
                                ActOffer {  offers = Set.singleton(
                                                          Offer { chanid = chanIdA
                                                                , chanoffers = [Quest varIdA1]
                                                          })
                                                      , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0
                                                                                            , cstrEqual vexprOp2pcQ int0
                                                                                            ])
                                          }
                                (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, int1, vexprA1]))


                              --        // 1,2 : only if G is empty: |[]|
                              --        A?A1 | B?B1 [op1$pc$Q == 0, op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](1, A1, -1, ANY)
                              ,(actionPref
                                ActOffer {  offers = Set.fromList [
                                                            Offer { chanid = chanIdA
                                                                  , chanoffers = [Quest varIdA1]
                                                            },
                                                            Offer { chanid = chanIdB
                                                                  , chanoffers = [Quest varIdB1]
                                                            }
                                                        ]
                                                      , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0
                                                                                            , cstrEqual vexprOp2pcQ int1
                                                                                            , cstrEqual vexprB1 vexprOp2QABx
                                                                                            ])
                                          }
                                (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, vexprMin1, anyInt]))

                              --        // 2,1 : only if G is empty: |[]|
                              --        B?B1 | A?A1 [op1$pc$Q == 1, op2$pc$Q == 0, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, 1, A1)
                              ,(actionPref
                                ActOffer {  offers = Set.fromList [
                                                            Offer { chanid = chanIdA
                                                                  , chanoffers = [Quest varIdA1]
                                                            },
                                                            Offer { chanid = chanIdB
                                                                  , chanoffers = [Quest varIdB1]
                                                            }
                                                        ]
                                                      , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int1
                                                                                            , cstrEqual vexprOp2pcQ int0
                                                                                            , cstrEqual vexprB1 vexprOp1QABx
                                                                                            ])
                                          }
                                (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, int1, vexprA1]))

                              --        // 2,2 : only if B \in G, but G could be more...
                              --        B?B1 [op1$pc$Q == 1, op2$pc$Q == 1, B1 ==op1$Q$gnf1$A$B$x, B1 ==op2$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, -1, ANY)
                              ,(actionPref
                                ActOffer {  offers = Set.singleton(
                                                          Offer { chanid = chanIdB
                                                                , chanoffers = [Quest varIdB1]
                                                          })
                                                      , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int1
                                                                                            , cstrEqual vexprOp2pcQ int1
                                                                                            , cstrEqual vexprB1 vexprOp1QABx
                                                                                            , cstrEqual vexprB1 vexprOp2QABx
                                                                                            ])
                                          }
                                (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, vexprMin1, anyInt]))


                      ])



-- case: G = []
-- P[A,B]() := Q[A,B]() |[]| Q[A,B]()
-- Q[A,B]() := A?x >-> B!x >-> STOP
-- with procInst := P[A,B]()
-- becomes after step combination
-- P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, op2$pc$Q, op2$Q$gnf1$A$B$x) :=
--        // only op1
--        A?A1 [op1$pc$Q == 0]                    >-> P[A,B](1, A1, op2$pc$Q, op2$Q$gnf1$A$B$x)
--        B?B1 [op1$pc$Q == 1, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, op2$pc$Q, op2$Q$gnf1$A$B$x)
--                                // note: the right side is still allowed to continue! that's intended behaviour.
--        // only op2
--        A?A1 [op2$pc$Q == 0]                    >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, 1, A1)
--        B?B1 [op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, -1, ANY)
--        // both op1 and op2
--        // 1,2 : only if G is empty: |[]|
--        A?A1 | B?B1 [op1$pc$Q == 0, op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](1, A1, -1, ANY)
--        // 2,1 : only if G is empty: |[]|
--        B?B1 | A?A1 [op1$pc$Q == 1, op2$pc$Q == 0, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, 1, A1)
--  with procInst = P[A,B](0, ANY, 0, ANY)
testMultiSeq1 :: Test
testMultiSeq1 = TestCase $
   assertBool "test multi-sequences 1"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where

      -- P[A,B]() := Q[A,B]() |G| Q[A,B]()
      -- Q[A,B]() := A?x >-> B!x >-> STOP
      -- with procInst := P[A,B]()
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procIdQ = procIdGen "Q" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [] [
                procInst procIdQ [chanIdA, chanIdB] [],
                procInst procIdQ [chanIdA, chanIdB] []
              ]
            )

      procDefQ = ProcDef [chanIdA, chanIdB] [] (
                    (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = [Quest varIdX]
                                                    })
                                                , constraint = cstrConst (Cbool True)
                                    }
                          (actionPref
                              ActOffer {  offers = Set.singleton(
                                                        Offer { chanid = chanIdB
                                                              , chanoffers = [Exclam vexprX]
                                                        })
                                                    , constraint = cstrConst (Cbool True)
                                        }
                              stop)))
      procDefs = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]

      -- becomes after step combination
      -- P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, op2$pc$Q, op2$Q$gnf1$A$B$x) :=
      varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
      varIdOp1QABx = VarId (T.pack "op1$Q$gnf1$A$B$x") 33 intSort
      varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
      varIdOp2QABx = VarId (T.pack "op2$Q$gnf1$A$B$x") 33 intSort

      vexprOp1pcQ = cstrVar varIdOp1pcQ
      vexprOp1QABx = cstrVar varIdOp1QABx
      vexprOp2pcQ = cstrVar varIdOp2pcQ
      vexprOp2QABx = cstrVar varIdOp2QABx

      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp1QABx, varIdOp2pcQ, varIdOp2QABx]

      --  with procInst = P[A,B](0, ANY, 0, ANY)
      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, anyInt, int0, anyInt]


      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp1QABx, varIdOp2pcQ, varIdOp2QABx]
                      (choice [
                          --        // only op1
                          --        A?A1 [op1$pc$Q == 0]                    >-> P[A,B](1, A1, op2$pc$Q, op2$Q$gnf1$A$B$x)
                          --        B?B1 [op1$pc$Q == 1, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, op2$pc$Q, op2$Q$gnf1$A$B$x)
                          --                                // note: the right side is still allowed to continue! that's intended behaviour.
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      })
                                                  , constraint = cstrEqual vexprOp1pcQ int0
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, vexprOp2pcQ, vexprOp2QABx]))
                          ,(actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdB
                                                            , chanoffers = [Quest varIdB1]
                                                      })
                                                  , constraint =  cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprOp1pcQ int1)
                                                                                             (cstrEqual vexprB1 vexprOp1QABx)
                                                                                             (cstrConst (Cbool False))
                                                                                        ])
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, vexprOp2pcQ, vexprOp2QABx]))


                            --        // only op2
                            --        A?A1 [op2$pc$Q == 0]                    >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, 1, A1)
                            --        B?B1 [op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, -1, ANY)
                            ,(actionPref
                              ActOffer {  offers = Set.singleton(
                                                        Offer { chanid = chanIdA
                                                              , chanoffers = [Quest varIdA1]
                                                        })
                                                    , constraint = cstrEqual vexprOp2pcQ int0
                                        }
                              (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp1QABx, int1, vexprA1]))
                            ,(actionPref
                              ActOffer {  offers = Set.singleton(
                                                        Offer { chanid = chanIdB
                                                              , chanoffers = [Quest varIdB1]
                                                        })
                                                    , constraint =  cstrAnd (Set.fromList [cstrITE (cstrEqual vexprOp2pcQ int1)
                                                                                               (cstrEqual vexprB1 vexprOp2QABx)
                                                                                               (cstrConst (Cbool False))
                                                                                          ])
                                        }
                              (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp1QABx, vexprMin1, anyInt]))



                              --        // both op1 and op2
                              --
                              -- --        // 1,1 : only if A \in G, but G could be more
                              -- --        A?A1 [op1$pc$Q == 0, op2$pc$Q == 0]                           >-> P[A,B](1, A1, 1, A1)
                              -- ,(actionPref
                              --   ActOffer {  offers = Set.singleton(
                              --                             Offer { chanid = chanIdA
                              --                                   , chanoffers = [Quest varIdA1]
                              --                             })
                              --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0
                              --                                                               , cstrEqual vexprOp2pcQ int0
                              --                                                               ])
                              --             }
                              --   (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, int1, vexprA1]))
                              --

                              --        // 1,2 : only if G is empty: |[]|
                              --        A?A1 | B?B1 [op1$pc$Q == 0, op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](1, A1, -1, ANY)
                              ,(actionPref
                                ActOffer {  offers = Set.fromList [
                                                            Offer { chanid = chanIdA
                                                                  , chanoffers = [Quest varIdA1]
                                                            },
                                                            Offer { chanid = chanIdB
                                                                  , chanoffers = [Quest varIdB1]
                                                            }
                                                        ]
                                                      , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0
                                                                                            , cstrITE (cstrEqual vexprOp2pcQ int1)
                                                                                                 (cstrEqual vexprB1 vexprOp2QABx)
                                                                                                 (cstrConst (Cbool False))
                                                                                            ])
                                          }
                                (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, vexprMin1, anyInt]))

                              --        // 2,1 : only if G is empty: |[]|
                              --        B?B1 | A?A1 [op1$pc$Q == 1, op2$pc$Q == 0, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, 1, A1)
                              ,(actionPref
                                ActOffer {  offers = Set.fromList [
                                                            Offer { chanid = chanIdA
                                                                  , chanoffers = [Quest varIdA1]
                                                            },
                                                            Offer { chanid = chanIdB
                                                                  , chanoffers = [Quest varIdB1]
                                                            }
                                                        ]
                                                      , constraint =  cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprOp1pcQ int1)
                                                                                                 (cstrEqual vexprB1 vexprOp1QABx)
                                                                                                 (cstrConst (Cbool False))
                                                                                            , cstrEqual vexprOp2pcQ int0])
                                          }
                                (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, int1, vexprA1]))

                              --        // 2,2 : only if B \in G, but G could be more...
                              --        B?B1 [op1$pc$Q == 1, op2$pc$Q == 1, B1 ==op1$Q$gnf1$A$B$x, B1 ==op2$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, -1, ANY)
                              -- ,(actionPref
                              --   ActOffer {  offers = Set.singleton(
                              --                             Offer { chanid = chanIdB
                              --                                   , chanoffers = [Quest varIdB1]
                              --                             })
                              --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int1
                              --                                                               , cstrEqual vexprOp2pcQ int1
                              --                                                               , cstrEqual vexprB1 vexprOp1QABx
                              --                                                               , cstrEqual vexprB1 vexprOp2QABx
                              --                                                               ])
                              --             }
                              --   (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, vexprMin1, anyInt]))


                      ])




-- case: G = [A]
-- P[A,B]() := Q[A,B]() |[A]| Q[A,B]()
-- Q[A,B]() := A?x >-> B!x >-> STOP
-- with procInst := P[A,B]()
-- becomes after step combination
-- P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, op2$pc$Q, op2$Q$gnf1$A$B$x) :=
--        // only op1
--        B?B1 [op1$pc$Q == 1, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, op2$pc$Q, op2$Q$gnf1$A$B$x)
--                                // note: the right side is still allowed to continue! that's intended behaviour.
--        // only op2
--        B?B1 [op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, -1, ANY)
--        // both op1 and op2
--        // 1,1 : only if A \in G, but G could be more
--        A?A1 [op1$pc$Q == 0, op2$pc$Q == 0]                           >-> P[A,B](1, A1, 1, A1)
--  with procInst = P[A,B](0, ANY, 0, ANY)
testMultiSeq2 :: Test
testMultiSeq2 = TestCase $
   assertBool "test multi-sequences 2"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where

      -- P[A,B]() := Q[A,B]() |G| Q[A,B]()
      -- Q[A,B]() := A?x >-> B!x >-> STOP
      -- with procInst := P[A,B]()
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procIdQ = procIdGen "Q" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdA] [
                procInst procIdQ [chanIdA, chanIdB] [],
                procInst procIdQ [chanIdA, chanIdB] []
              ]
            )

      procDefQ = ProcDef [chanIdA, chanIdB] [] (
                    (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = [Quest varIdX]
                                                    })
                                                , constraint = cstrConst (Cbool True)
                                    }
                          (actionPref
                              ActOffer {  offers = Set.singleton(
                                                        Offer { chanid = chanIdB
                                                              , chanoffers = [Exclam vexprX]
                                                        })
                                                    , constraint = cstrConst (Cbool True)
                                        }
                              stop)))
      procDefs = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]

      -- becomes after step combination
      -- P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, op2$pc$Q, op2$Q$gnf1$A$B$x) :=
      varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
      varIdOp1QABx = VarId (T.pack "op1$Q$gnf1$A$B$x") 33 intSort
      varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
      varIdOp2QABx = VarId (T.pack "op2$Q$gnf1$A$B$x") 33 intSort

      vexprOp1pcQ = cstrVar varIdOp1pcQ
      vexprOp1QABx = cstrVar varIdOp1QABx
      vexprOp2pcQ = cstrVar varIdOp2pcQ
      vexprOp2QABx = cstrVar varIdOp2QABx

      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp1QABx, varIdOp2pcQ, varIdOp2QABx]

      --  with procInst = P[A,B](0, ANY, 0, ANY)
      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, anyInt, int0, anyInt]


      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp1QABx, varIdOp2pcQ, varIdOp2QABx]
                      (choice [
                          --        // only op1
                          --        A?A1 [op1$pc$Q == 0]                    >-> P[A,B](1, A1, op2$pc$Q, op2$Q$gnf1$A$B$x)
                          --        B?B1 [op1$pc$Q == 1, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, op2$pc$Q, op2$Q$gnf1$A$B$x)
                          --                                // note: the right side is still allowed to continue! that's intended behaviour.
                          -- (actionPref
                          --   ActOffer {  offers = Set.singleton(
                          --                             Offer { chanid = chanIdA
                          --                                   , chanoffers = [Quest varIdA1]
                          --                             })
                          --                         , constraint = cstrEqual vexprOp1pcQ int0
                          --             }
                          --   (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, vexprOp2pcQ, vexprOp2QABx])),
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdB
                                                            , chanoffers = [Quest varIdB1]
                                                      })
                                                  , constraint =  cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprOp1pcQ int1)
                                                                                             (cstrEqual vexprB1 vexprOp1QABx)
                                                                                             (cstrConst (Cbool False))
                                                                                        ])
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, vexprOp2pcQ, vexprOp2QABx]))


                            --        // only op2
                            --        A?A1 [op2$pc$Q == 0]                    >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, 1, A1)
                            --        B?B1 [op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, -1, ANY)
                            -- ,(actionPref
                            --   ActOffer {  offers = Set.singleton(
                            --                             Offer { chanid = chanIdA
                            --                                   , chanoffers = [Quest varIdA1]
                            --                             })
                            --                         , constraint = cstrEqual vexprOp2pcQ int0
                            --             }
                            --   (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp1QABx, int1, vexprA1]))
                            ,(actionPref
                              ActOffer {  offers = Set.singleton(
                                                        Offer { chanid = chanIdB
                                                              , chanoffers = [Quest varIdB1]
                                                        })
                                                    , constraint =  cstrAnd (Set.fromList [  cstrITE (cstrEqual vexprOp2pcQ int1)
                                                                                               (cstrEqual vexprB1 vexprOp2QABx)
                                                                                               (cstrConst (Cbool False))
                                                                                          ])
                                        }
                              (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp1QABx, vexprMin1, anyInt]))



                              --        // both op1 and op2

                              --        // 1,1 : only if A \in G, but G could be more
                              --        A?A1 [op1$pc$Q == 0, op2$pc$Q == 0]                           >-> P[A,B](1, A1, 1, A1)
                              ,(actionPref
                                ActOffer {  offers = Set.singleton(
                                                          Offer { chanid = chanIdA
                                                                , chanoffers = [Quest varIdA1]
                                                          })
                                                      , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0
                                                                                            , cstrEqual vexprOp2pcQ int0
                                                                                            ])
                                          }
                                (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, int1, vexprA1]))


                              --        // 1,2 : only if G is empty: |[]|
                              --        A?A1 | B?B1 [op1$pc$Q == 0, op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](1, A1, -1, ANY)
                              -- ,(actionPref
                              --   ActOffer {  offers = Set.fromList [
                              --                               Offer { chanid = chanIdA
                              --                                     , chanoffers = [Quest varIdA1]
                              --                               },
                              --                               Offer { chanid = chanIdB
                              --                                     , chanoffers = [Quest varIdB1]
                              --                               }
                              --                           ]
                              --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0
                              --                                                               , cstrEqual vexprOp2pcQ int1
                              --                                                               , cstrEqual vexprB1 vexprOp2QABx
                              --                                                               ])
                              --             }
                              --   (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, vexprMin1, anyInt]))

                              --        // 2,1 : only if G is empty: |[]|
                              --        B?B1 | A?A1 [op1$pc$Q == 1, op2$pc$Q == 0, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, 1, A1)
                              -- ,(actionPref
                              --   ActOffer {  offers = Set.fromList [
                              --                               Offer { chanid = chanIdA
                              --                                     , chanoffers = [Quest varIdA1]
                              --                               },
                              --                               Offer { chanid = chanIdB
                              --                                     , chanoffers = [Quest varIdB1]
                              --                               }
                              --                           ]
                              --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int1
                              --                                                               , cstrEqual vexprOp2pcQ int0
                              --                                                               , cstrEqual vexprB1 vexprOp1QABx
                              --                                                               ])
                              --             }
                              --   (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, int1, vexprA1]))

                              --        // 2,2 : only if B \in G, but G could be more...
                              --        B?B1 [op1$pc$Q == 1, op2$pc$Q == 1, B1 ==op1$Q$gnf1$A$B$x, B1 ==op2$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, -1, ANY)
                              -- ,(actionPref
                              -- ActOffer {  offers = Set.singleton(
                              --                           Offer { chanid = chanIdB
                              --                                 , chanoffers = [Quest varIdB1]
                              --                           })
                              --                       , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int1
                              --                                     , cstrEqual vexprOp2pcQ int1
                              --                                     , cstrEqual vexprB1 vexprOp1QABx
                              --                                     , cstrEqual vexprB1 vexprOp2QABx
                              --                                     ])
                              --             }
                              --   (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, vexprMin1, anyInt]))


                      ])







-- case: G = [B]
-- P[A,B]() := Q[A,B]() |[B]| Q[A,B]()
-- Q[A,B]() := A?x >-> B!x >-> STOP
-- with procInst := P[A,B]()
-- becomes after step combination
-- P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, op2$pc$Q, op2$Q$gnf1$A$B$x) :=
--        // only op1
--        A?A1 [op1$pc$Q == 0]                    >-> P[A,B](1, A1, op2$pc$Q, op2$Q$gnf1$A$B$x)
--        // only op2
--        A?A1 [op2$pc$Q == 0]                    >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, 1, A1)
--        // both op1 and op2
--        // 2,2 : only if B \in G, but G could be more...
--        B?B1 [op1$pc$Q == 1, op2$pc$Q == 1, B1 ==op1$Q$gnf1$A$B$x, B1 ==op2$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, -1, ANY)
--  with procInst = P[A,B](0, ANY, 0, ANY)
testMultiSeq3 :: Test
testMultiSeq3 = TestCase $
   assertBool "test multi-sequences 3"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where

      -- P[A,B]() := Q[A,B]() |G| Q[A,B]()
      -- Q[A,B]() := A?x >-> B!x >-> STOP
      -- with procInst := P[A,B]()
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procIdQ = procIdGen "Q" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdB] [
                procInst procIdQ [chanIdA, chanIdB] [],
                procInst procIdQ [chanIdA, chanIdB] []
              ]
            )

      procDefQ = ProcDef [chanIdA, chanIdB] [] (
                    (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = [Quest varIdX]
                                                    })
                                                , constraint = cstrConst (Cbool True)
                                    }
                          (actionPref
                              ActOffer {  offers = Set.singleton(
                                                        Offer { chanid = chanIdB
                                                              , chanoffers = [Exclam vexprX]
                                                        })
                                                    , constraint = cstrConst (Cbool True)
                                        }
                              stop)))
      procDefs = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]

      -- becomes after step combination
      -- P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, op2$pc$Q, op2$Q$gnf1$A$B$x) :=
      varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
      varIdOp1QABx = VarId (T.pack "op1$Q$gnf1$A$B$x") 33 intSort
      varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
      varIdOp2QABx = VarId (T.pack "op2$Q$gnf1$A$B$x") 33 intSort

      vexprOp1pcQ = cstrVar varIdOp1pcQ
      vexprOp1QABx = cstrVar varIdOp1QABx
      vexprOp2pcQ = cstrVar varIdOp2pcQ
      vexprOp2QABx = cstrVar varIdOp2QABx

      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp1QABx, varIdOp2pcQ, varIdOp2QABx]

      --  with procInst = P[A,B](0, ANY, 0, ANY)
      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, anyInt, int0, anyInt]


      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp1QABx, varIdOp2pcQ, varIdOp2QABx]
                      (choice [
                          --        // only op1
                          --        A?A1 [op1$pc$Q == 0]                    >-> P[A,B](1, A1, op2$pc$Q, op2$Q$gnf1$A$B$x)
                          --        B?B1 [op1$pc$Q == 1, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, op2$pc$Q, op2$Q$gnf1$A$B$x)
                          --                                // note: the right side is still allowed to continue! that's intended behaviour.
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      })
                                                  , constraint = cstrEqual vexprOp1pcQ int0
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, vexprOp2pcQ, vexprOp2QABx]))
                          -- ,(actionPref
                          --   ActOffer {  offers = Set.singleton(
                          --                             Offer { chanid = chanIdB
                          --                                   , chanoffers = [Quest varIdB1]
                          --                             })
                          --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int1
                          --                                                               , cstrEqual vexprB1 vexprOp1QABx
                          --                                                               ])
                          --             }
                          --   (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, vexprOp2pcQ, vexprOp2QABx]))


                            --        // only op2
                            --        A?A1 [op2$pc$Q == 0]                    >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, 1, A1)
                            --        B?B1 [op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, -1, ANY)
                            ,(actionPref
                              ActOffer {  offers = Set.singleton(
                                                        Offer { chanid = chanIdA
                                                              , chanoffers = [Quest varIdA1]
                                                        })
                                                    , constraint = cstrEqual vexprOp2pcQ int0
                                        }
                              (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp1QABx, int1, vexprA1]))
                            -- ,(actionPref
                            --   ActOffer {  offers = Set.singleton(
                            --                             Offer { chanid = chanIdB
                            --                                   , chanoffers = [Quest varIdB1]
                            --                             })
                            --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp2pcQ int1
                            --                                                               , cstrEqual vexprB1 vexprOp2QABx
                            --                                                               ])
                            --             }
                            --   (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp1QABx, vexprMin1, anyInt]))



                              --        // both op1 and op2

                              --        // 1,1 : only if A \in G, but G could be more
                              --        A?A1 [op1$pc$Q == 0, op2$pc$Q == 0]                           >-> P[A,B](1, A1, 1, A1)
                              -- ,(actionPref
                              --   ActOffer {  offers = Set.singleton(
                              --                             Offer { chanid = chanIdA
                              --                                   , chanoffers = [Quest varIdA1]
                              --                             })
                              --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0
                              --                                                               , cstrEqual vexprOp2pcQ int0
                              --                                                               ])
                              --             }
                              --   (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, int1, vexprA1]))


                              --        // 1,2 : only if G is empty: |[]|
                              --        A?A1 | B?B1 [op1$pc$Q == 0, op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](1, A1, -1, ANY)
                              -- ,(actionPref
                              --   ActOffer {  offers = Set.fromList [
                              --                               Offer { chanid = chanIdA
                              --                                     , chanoffers = [Quest varIdA1]
                              --                               },
                              --                               Offer { chanid = chanIdB
                              --                                     , chanoffers = [Quest varIdB1]
                              --                               }
                              --                           ]
                              --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0
                              --                                                               , cstrEqual vexprOp2pcQ int1
                              --                                                               , cstrEqual vexprB1 vexprOp2QABx
                              --                                                               ])
                              --             }
                              --   (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, vexprMin1, anyInt]))

                              --        // 2,1 : only if G is empty: |[]|
                              --        B?B1 | A?A1 [op1$pc$Q == 1, op2$pc$Q == 0, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, 1, A1)
                              -- ,(actionPref
                              --   ActOffer {  offers = Set.fromList [
                              --                               Offer { chanid = chanIdA
                              --                                     , chanoffers = [Quest varIdA1]
                              --                               },
                              --                               Offer { chanid = chanIdB
                              --                                     , chanoffers = [Quest varIdB1]
                              --                               }
                              --                           ]
                              --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int1
                              --                                                               , cstrEqual vexprOp2pcQ int0
                              --                                                               , cstrEqual vexprB1 vexprOp1QABx
                              --                                                               ])
                              --             }
                              --   (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, int1, vexprA1]))

                              --        // 2,2 : only if B \in G, but G could be more...
                              --        B?B1 [op1$pc$Q == 1, op2$pc$Q == 1, B1 ==op1$Q$gnf1$A$B$x, B1 ==op2$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, -1, ANY)
                              ,(actionPref
                                ActOffer {  offers = Set.singleton(
                                                          Offer { chanid = chanIdB
                                                                , chanoffers = [Quest varIdB1]
                                                          })
                                                      , constraint =  cstrAnd (Set.fromList [  cstrITE (cstrEqual vexprOp1pcQ int1)
                                                                                                  (cstrEqual vexprB1 vexprOp1QABx)
                                                                                                  (cstrConst (Cbool False)),
                                                                                              cstrITE (cstrEqual vexprOp2pcQ int1)
                                                                                                  (cstrEqual vexprB1 vexprOp2QABx)
                                                                                                  (cstrConst (Cbool False))
                                                                                              ])
                                          }
                                (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, vexprMin1, anyInt]))


                      ])



-- case: G = [A,B]
-- P[A,B]() := Q[A,B]() |[A,B]| Q[A,B]()
-- Q[A,B]() := A?x >-> B!x >-> STOP
-- with procInst := P[A,B]()
-- becomes after step combination

-- P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, op2$pc$Q, op2$Q$gnf1$A$B$x) :=
--        // both op1 and op2
--        // 1,1 : only if A \in G, but G could be more
--        A?A1 [op1$pc$Q == 0, op2$pc$Q == 0]                           >-> P[A,B](1, A1, 1, A1)
--        // 2,2 : only if B \in G, but G could be more...
--        B?B1 [op1$pc$Q == 1, op2$pc$Q == 1, B1 ==op1$Q$gnf1$A$B$x, B1 ==op2$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, -1, ANY)
--  with procInst = P[A,B](0, ANY, 0, ANY)
testMultiSeq4 :: Test
testMultiSeq4 = TestCase $
   assertBool "test multi-sequences 4"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where

      -- P[A,B]() := Q[A,B]() |G| Q[A,B]()
      -- Q[A,B]() := A?x >-> B!x >-> STOP
      -- with procInst := P[A,B]()
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procIdQ = procIdGen "Q" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [chanIdA, chanIdB] [
                procInst procIdQ [chanIdA, chanIdB] [],
                procInst procIdQ [chanIdA, chanIdB] []
              ]
            )

      procDefQ = ProcDef [chanIdA, chanIdB] [] (
                    (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = [Quest varIdX]
                                                    })
                                                , constraint = cstrConst (Cbool True)
                                    }
                          (actionPref
                              ActOffer {  offers = Set.singleton(
                                                        Offer { chanid = chanIdB
                                                              , chanoffers = [Exclam vexprX]
                                                        })
                                                    , constraint = cstrConst (Cbool True)
                                        }
                              stop)))
      procDefs = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]

      -- becomes after step combination
      -- P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, op2$pc$Q, op2$Q$gnf1$A$B$x) :=
      varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
      varIdOp1QABx = VarId (T.pack "op1$Q$gnf1$A$B$x") 33 intSort
      varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
      varIdOp2QABx = VarId (T.pack "op2$Q$gnf1$A$B$x") 33 intSort

      vexprOp1pcQ = cstrVar varIdOp1pcQ
      vexprOp1QABx = cstrVar varIdOp1QABx
      vexprOp2pcQ = cstrVar varIdOp2pcQ
      vexprOp2QABx = cstrVar varIdOp2QABx

      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp1QABx, varIdOp2pcQ, varIdOp2QABx]

      --  with procInst = P[A,B](0, ANY, 0, ANY)
      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, anyInt, int0, anyInt]


      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp1QABx, varIdOp2pcQ, varIdOp2QABx]
                      (choice [
                          --        // only op1
                          --        A?A1 [op1$pc$Q == 0]                    >-> P[A,B](1, A1, op2$pc$Q, op2$Q$gnf1$A$B$x)
                          --        B?B1 [op1$pc$Q == 1, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, op2$pc$Q, op2$Q$gnf1$A$B$x)
                          --                                // note: the right side is still allowed to continue! that's intended behaviour.
                          -- (actionPref
                          --   ActOffer {  offers = Set.singleton(
                          --                             Offer { chanid = chanIdA
                          --                                   , chanoffers = [Quest varIdA1]
                          --                             })
                          --                         , constraint = cstrEqual vexprOp1pcQ int0
                          --             }
                          --   (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, vexprOp2pcQ, vexprOp2QABx]))
                          -- ,(actionPref
                          --   ActOffer {  offers = Set.singleton(
                          --                             Offer { chanid = chanIdB
                          --                                   , chanoffers = [Quest varIdB1]
                          --                             })
                          --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int1
                          --                                                               , cstrEqual vexprB1 vexprOp1QABx
                          --                                                               ])
                          --             }
                          --   (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, vexprOp2pcQ, vexprOp2QABx]))


                            --        // only op2
                            --        A?A1 [op2$pc$Q == 0]                    >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, 1, A1)
                            --        B?B1 [op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](op1$pc$Q, op1$Q$gnf1$A$B$x, -1, ANY)
                            -- ,(actionPref
                            --   ActOffer {  offers = Set.singleton(
                            --                             Offer { chanid = chanIdA
                            --                                   , chanoffers = [Quest varIdA1]
                            --                             })
                            --                         , constraint = cstrEqual vexprOp2pcQ int0
                            --             }
                            --   (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp1QABx, int1, vexprA1]))
                            -- ,(actionPref
                            --   ActOffer {  offers = Set.singleton(
                            --                             Offer { chanid = chanIdB
                            --                                   , chanoffers = [Quest varIdB1]
                            --                             })
                            --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp2pcQ int1
                            --                                                               , cstrEqual vexprB1 vexprOp2QABx
                            --                                                               ])
                            --             }
                            --   (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp1QABx, vexprMin1, anyInt]))



                              --        // both op1 and op2

                              --        // 1,1 : only if A \in G, but G could be more
                              --        A?A1 [op1$pc$Q == 0, op2$pc$Q == 0]                           >-> P[A,B](1, A1, 1, A1)
                              (actionPref
                                ActOffer {  offers = Set.singleton(
                                                          Offer { chanid = chanIdA
                                                                , chanoffers = [Quest varIdA1]
                                                          })
                                                      , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0
                                                                                            , cstrEqual vexprOp2pcQ int0
                                                                                            ])
                                          }
                                (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, int1, vexprA1]))


                              --        // 1,2 : only if G is empty: |[]|
                              --        A?A1 | B?B1 [op1$pc$Q == 0, op2$pc$Q == 1, B1 == op2$Q$gnf1$A$B$x] >-> P[A,B](1, A1, -1, ANY)
                              -- ,(actionPref
                              --   ActOffer {  offers = Set.fromList [
                              --                               Offer { chanid = chanIdA
                              --                                     , chanoffers = [Quest varIdA1]
                              --                               },
                              --                               Offer { chanid = chanIdB
                              --                                     , chanoffers = [Quest varIdB1]
                              --                               }
                              --                           ]
                              --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0
                              --                                                               , cstrEqual vexprOp2pcQ int1
                              --                                                               , cstrEqual vexprB1 vexprOp2QABx
                              --                                                               ])
                              --             }
                              --   (procInst procIdP' [chanIdA, chanIdB] [int1, vexprA1, vexprMin1, anyInt]))

                              --        // 2,1 : only if G is empty: |[]|
                              --        B?B1 | A?A1 [op1$pc$Q == 1, op2$pc$Q == 0, B1 == op1$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, 1, A1)
                              -- ,(actionPref
                              --   ActOffer {  offers = Set.fromList [
                              --                               Offer { chanid = chanIdA
                              --                                     , chanoffers = [Quest varIdA1]
                              --                               },
                              --                               Offer { chanid = chanIdB
                              --                                     , chanoffers = [Quest varIdB1]
                              --                               }
                              --                           ]
                              --                         , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int1
                              --                                                               , cstrEqual vexprOp2pcQ int0
                              --                                                               , cstrEqual vexprB1 vexprOp1QABx
                              --                                                               ])
                              --             }
                              --   (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, int1, vexprA1]))

                              --        // 2,2 : only if B \in G, but G could be more...
                              --        B?B1 [op1$pc$Q == 1, op2$pc$Q == 1, B1 ==op1$Q$gnf1$A$B$x, B1 ==op2$Q$gnf1$A$B$x] >-> P[A,B](-1, ANY, -1, ANY)
                              ,(actionPref
                                ActOffer {  offers = Set.singleton(
                                                          Offer { chanid = chanIdB
                                                                , chanoffers = [Quest varIdB1]
                                                          })
                                                      , constraint =  cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprOp1pcQ int1)
                                                                                                  (cstrEqual vexprB1 vexprOp1QABx)
                                                                                                  (cstrConst (Cbool False)),
                                                                                              cstrITE (cstrEqual vexprOp2pcQ int1)
                                                                                                  (cstrEqual vexprB1 vexprOp2QABx)
                                                                                                  (cstrConst (Cbool False))
                                                                                              ])
                                          }
                                (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, anyInt, vexprMin1, anyInt]))


                      ])




-- -------------------------------------------------
-- test 3 operands
-- P[A]() := Q[A](1) |G| Q[A](1) |G| Q[A](1)
-- Q[A](s) := A >-> STOP
-- becomes after operand LPE translation
-- Q[A](pc$Q, Q$A$s) := A [pc$Q == 0] >-> Q[A](-1, ANY)
-- becomes after LPEPar
-- combination of op1, op2:
--  // only 1
--  A [op1$pc$Q == 0] >-> P[A](-1, ANY, op2$pc$Q, op2$Q$s, op3$pc$Q, op3$Q$s)
--  // only 2
--  A [op2$pc$Q == 0] >-> P[A](op1$pc$Q, op1$Q$s, -1, ANY, op3$pc$Q, op3$Q$s)
--  // 1 and 2
--  A [op1$pc$Q == 0, op2$pc$Q == 0] >-> P[A](-1, ANY, -1, ANY, op3$pc$Q, op3$Q$s)

-- combination of [op1, op2] and op3
-- // only [1,2] is already given above
-- // only 3
--  A [op3$pc$Q == 0] >-> P[A](op1$pc$Q, op1$Q$s, op2$pc$Q, op2$Q$s, -1, ANY)

-- // [1,2] AND 3
--  A [op1$pc$Q == 0, op3$pc$Q == 0] >-> P[A](-1, ANY, op2$pc$Q, op2$Q$s, -1, ANY)              // only if [1] existed after [1,2] and then sync on A possible: i.e. A \in G
--  A [op2$pc$Q == 0, op3$pc$Q == 0] >-> P[A](op1$pc$Q, op1$Q$s,  -1, ANY, -1, ANY)
--  A [op1$pc$Q == 0, op2$pc$Q == 0, op3$pc$Q == 0] >-> P[A](-1, ANY, -1, ANY, -1, ANY)


-- case G = []
-- P[A](op1$pc$Q, op1$Q$s, op2$pc$Q, op2$Q$s, op3$pc$Q, op3$Q$s) :=
testThreeOperands1 :: Test
testThreeOperands1 = TestCase $
   assertBool "test three operands"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] [VarId (T.pack "s") 0 intSort]
      procDefP = ProcDef [chanIdA] [] (
            parallel [] [
                procInst procIdQ [chanIdA] [int1],
                procInst procIdQ [chanIdA] [int1],
                procInst procIdQ [chanIdA] [int1]
              ]
            )
      procDefQ = ProcDef [chanIdA] [VarId (T.pack "s") 0 intSort] (
            actionPref actOfferA stop)
      procDefs = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]

      varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
      varIdOp1QAs = VarId (T.pack "op1$Q$A$s") 0 intSort
      varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
      varIdOp2QAs = VarId (T.pack "op2$Q$A$s") 0 intSort
      varIdOp3pcQ = VarId (T.pack "op3$pc$Q") 0 intSort
      varIdOp3QAs = VarId (T.pack "op3$Q$A$s") 0 intSort
      vexprOp1pcQ = cstrVar varIdOp1pcQ
      vexprOp1QAs = cstrVar varIdOp1QAs
      vexprOp2pcQ = cstrVar varIdOp2pcQ
      vexprOp2QAs = cstrVar varIdOp2QAs
      vexprOp3pcQ = cstrVar varIdOp3pcQ
      vexprOp3QAs = cstrVar varIdOp3QAs

      -- with procInst := P[A](0,0,0)
      procIdP' = procIdGen "P" [chanIdA] [varIdOp1pcQ, varIdOp1QAs, varIdOp2pcQ, varIdOp2QAs, varIdOp3pcQ, varIdOp3QAs]
      procDefP' = ProcDef [chanIdA] [varIdOp1pcQ, varIdOp1QAs, varIdOp2pcQ, varIdOp2QAs, varIdOp3pcQ, varIdOp3QAs]
                      (choice [
                        -- combination of 1 and 2
                        --  // only 1
                        --  A [op1$pc$Q == 0] >-> P[A](-1, op2$pc$Q, op3$pc$Q)
                        --  // only 2
                        --  A [op2$pc$Q == 0] >-> P[A](op1$pc$Q, -1, op3$pc$Q)
                        (actionPref
                          ActOffer {  offers = Set.fromList [
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    }]
                                                , constraint =  cstrEqual vexprOp1pcQ int0

                                    }
                          (procInst procIdP' [chanIdA] [vexprMin1, anyInt, vexprOp2pcQ, vexprOp2QAs, vexprOp3pcQ, vexprOp3QAs]))
                          ,(actionPref
                            ActOffer {  offers = Set.fromList [
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = []
                                                      }]
                                                  , constraint = cstrEqual vexprOp2pcQ int0


                                      }
                            (procInst procIdP' [chanIdA] [vexprOp1pcQ, vexprOp1QAs, vexprMin1, anyInt, vexprOp3pcQ, vexprOp3QAs]))

                              -- combination of [op1, op2] and op3
                              -- // only [1,2] is already given above
                              -- // only 3
                              --  A [op3$pc$Q == 0] >-> P[A](op1$pc$Q, op2$pc$Q, -1)
                              ,(actionPref
                                ActOffer {  offers = Set.fromList [
                                                          Offer { chanid = chanIdA
                                                                , chanoffers = []
                                                          }]
                                                      , constraint = cstrEqual vexprOp3pcQ int0


                                          }
                                (procInst procIdP' [chanIdA] [vexprOp1pcQ, vexprOp1QAs, vexprOp2pcQ, vexprOp2QAs, vexprMin1, anyInt]))])


      procInst' = procInst procIdP' [chanIdA] [int0, int1, int0, int1, int0, int1]



-- case G = [A]
-- P[A](op1$pc$Q, op2$pc$Q, op3$pc$Q) :=
testThreeOperands2 :: Test
testThreeOperands2 = TestCase $
   assertBool "test three operands 2"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
     procInst'' = procInst procIdP [chanIdA] []
     procIdP = procIdGen "P" [chanIdA] []
     procIdQ = procIdGen "Q" [chanIdA] [VarId (T.pack "s") 0 intSort]
     procDefP = ProcDef [chanIdA] [] (
           parallel [chanIdA] [
               procInst procIdQ [chanIdA] [int1],
               procInst procIdQ [chanIdA] [int1],
               procInst procIdQ [chanIdA] [int1]
             ]
           )
     procDefQ = ProcDef [chanIdA] [VarId (T.pack "s") 0 intSort] (
           actionPref actOfferA stop)
     procDefs = Map.fromList  [  (procIdP, procDefP)
                               , (procIdQ, procDefQ)]

     varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
     varIdOp1QAs = VarId (T.pack "op1$Q$A$s") 0 intSort
     varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
     varIdOp2QAs = VarId (T.pack "op2$Q$A$s") 0 intSort
     varIdOp3pcQ = VarId (T.pack "op3$pc$Q") 0 intSort
     varIdOp3QAs = VarId (T.pack "op3$Q$A$s") 0 intSort
     vexprOp1pcQ = cstrVar varIdOp1pcQ
     vexprOp1QAs = cstrVar varIdOp1QAs
     vexprOp2pcQ = cstrVar varIdOp2pcQ
     vexprOp2QAs = cstrVar varIdOp2QAs
     vexprOp3pcQ = cstrVar varIdOp3pcQ
     vexprOp3QAs = cstrVar varIdOp3QAs

     -- with procInst := P[A](0,0,0)
     procIdP' = procIdGen "P" [chanIdA] [varIdOp1pcQ, varIdOp1QAs, varIdOp2pcQ, varIdOp2QAs, varIdOp3pcQ, varIdOp3QAs]
     procDefP' = ProcDef [chanIdA] [varIdOp1pcQ, varIdOp1QAs, varIdOp2pcQ, varIdOp2QAs, varIdOp3pcQ, varIdOp3QAs]
                                  (actionPref
                                    ActOffer {  offers = Set.fromList [
                                                              Offer { chanid = chanIdA
                                                                    , chanoffers = []
                                                              }]
                                                          , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0,
                                                                                                  cstrEqual vexprOp2pcQ int0,
                                                                                                  cstrEqual vexprOp3pcQ int0
                                                                          ])


                                              }
                                    (procInst procIdP' [chanIdA] [vexprMin1, anyInt, vexprMin1, anyInt, vexprMin1, anyInt]))
     procInst' = procInst procIdP' [chanIdA] [int0, int1, int0, int1, int0, int1]



-- -------------------------------------------------
-- test 3 operands, one operand with a different channel
-- P[A,B]() := Q[A]() |G| Q[A]() |G| R[B]()
-- Q[A]() := A >-> STOP
-- R[B]()  := B >-> STOP
-- becomes after operand LPE translation
-- Q[A](pc$Q) := A [pc$Q == 0] >-> Q[A](-1)
-- R[B](pc$R) := B [pc$R == 0] >-> R[B](-1)
--
-- becomes after LPEPar - all possible cases listed:
-- P[A,B](op1$pc$Q, op2$pc$Q, op3$pc$R) :=
-- combination of op1, op2:
--  // only 1
--  A [op1$pc$Q == 0] >-> P[A,B](-1, op2$pc$Q,  op3$pc$R)
--  // only 2
--  A [op2$pc$Q == 0] >-> P[A,B](op1$pc$Q, -1, op3$pc$R)
--  // 1 and 2
--  A [op1$pc$Q == 0, op2$pc$Q == 0] >-> P[A,B](-1, -1, op3$pc$R)
--
-- combination of [op1, op2] and op3
-- // only [1,2] is already given above
-- // only 3
--  B [op3$pc$R == 0] >-> P[A,B](op1$pc$Q, op2$pc$Q, -1)
--
-- // [1,2] AND 3
--  A | B [op1$pc$Q == 0, op3$pc$Q == 0] >-> P[A,B](-1, op2$pc$Q, -1)              // only if [1] existed after [1,2] and then sync on A possible: i.e. A \in G
--  A | B [op2$pc$Q == 0, op3$pc$Q == 0] >-> P[A,B](op1$pc$Q, -1, -1)
--  A | B [op1$pc$Q == 0, op2$pc$Q == 0, op3$pc$R == 0] >-> P[A,B](-1, -1, -1)
--
-- with procInst: P[A,B](0,0,0)
-- -------------------------------------------------
testThreeOperandsDiffChannelsGEN :: Test
testThreeOperandsDiffChannelsGEN = TestCase $
 assertBool "test three operands - different channels"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
 where
   procInst'' = procInst procIdP [chanIdA, chanIdB] []
   procIdP = procIdGen "P" [chanIdA] []
   procIdQ = procIdGen "Q" [chanIdA] []
   procIdR = procIdGen "R" [chanIdB] []

   procDefP = ProcDef [chanIdA, chanIdB] [] (
         parallel [] [
             procInst procIdQ [chanIdA] [],
             procInst procIdQ [chanIdA] [],
             procInst procIdR [chanIdB] []
           ]
         )
   procDefQ = ProcDef [chanIdA] [] (
                    actionPref actOfferA stop)
   procDefR = ProcDef [chanIdB] [] (
                   actionPref actOfferB stop)

   procDefs = Map.fromList  [  (procIdP, procDefP)
                             , (procIdQ, procDefQ)
                             , (procIdR, procDefR)]

   varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
   varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
   varIdOp3pcR = VarId (T.pack "op3$pc$R") 0 intSort
   vexprOp1pcQ = cstrVar varIdOp1pcQ
   vexprOp2pcQ = cstrVar varIdOp2pcQ
   vexprOp3pcR = cstrVar varIdOp3pcR

   -- with procInst := P[A](0,0,0)
   procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ, varIdOp3pcR]
   procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ, varIdOp3pcR]
                    (choice  [
                              -- combination of op1, op2:
                              --  // only 1
                              --  A [op1$pc$Q == 0] >-> P[A,B](-1, op2$pc$Q,  op3$pc$Q)
                              --  // only 2
                              --  A [op2$pc$Q == 0] >-> P[A,B](op1$pc$Q, -1, op3$pc$Q)
                              --  // 1 and 2
                              --  A [op1$pc$Q == 0, op2$pc$Q == 0] >-> P[A,B](-1, -1, op3$pc$Q)
                              (actionPref
                                  ActOffer {  offers = Set.fromList [
                                                            Offer { chanid = chanIdA
                                                                  , chanoffers = []
                                                            }]
                                                        , constraint =  cstrEqual vexprOp1pcQ int0


                                            }
                                  (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcQ, vexprOp3pcR]))
                              , (actionPref
                                    ActOffer {  offers = Set.fromList [
                                                              Offer { chanid = chanIdA
                                                                    , chanoffers = []
                                                              }]
                                                          , constraint =  cstrEqual vexprOp2pcQ int0


                                              }
                                    (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprMin1, vexprOp3pcR]))
                                , (actionPref
                                          ActOffer {  offers = Set.fromList [
                                                                    Offer { chanid = chanIdA
                                                                          , chanoffers = []
                                                                    }]
                                                                , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0,
                                                                                                        cstrEqual vexprOp2pcQ int0
                                                                                ])
                                                    }
                                          (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1, vexprOp3pcR]))


                                -- combination of [op1, op2] and op3
                                -- // only [1,2] is already given above
                                -- // only 3
                                --  B [op3$pc$R == 0] >-> P[A,B](op1$pc$Q, op2$pc$Q, -1)
                                , (actionPref
                                          ActOffer {  offers = Set.fromList [
                                                                    Offer { chanid = chanIdB
                                                                          , chanoffers = []
                                                                    }]
                                                                , constraint =  cstrEqual vexprOp3pcR int0


                                                    }
                                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp2pcQ, vexprMin1]))


                                -- // [1,2] AND 3
                                --  A | B [op1$pc$Q == 0, op3$pc$Q == 0] >-> P[A,B](-1, op2$pc$Q, -1)              // only if [1] existed after [1,2] and then sync on A possible: i.e. A \in G
                                --  A | B [op2$pc$Q == 0, op3$pc$Q == 0] >-> P[A,B](op1$pc$Q, -1, -1)
                                --  A | B [op1$pc$Q == 0, op2$pc$Q == 0, op3$pc$Q == 0] >-> P[A,B](-1, -1, -1)
                                --
                                , (actionPref
                                          ActOffer {  offers = Set.fromList [
                                                                    Offer { chanid = chanIdA
                                                                          , chanoffers = []
                                                                    },
                                                                    Offer { chanid = chanIdB
                                                                          , chanoffers = []
                                                                    }]
                                                                , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0,
                                                                                                        cstrEqual vexprOp3pcR int0
                                                                                ])
                                                    }
                                          (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcQ, vexprMin1]))
                                          , (actionPref
                                                    ActOffer {  offers = Set.fromList [
                                                                              Offer { chanid = chanIdA
                                                                                    , chanoffers = []
                                                                              },
                                                                              Offer { chanid = chanIdB
                                                                                    , chanoffers = []
                                                                              }]
                                                                          , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp2pcQ int0,
                                                                                                                  cstrEqual vexprOp3pcR int0
                                                                                          ])
                                                              }
                                                    (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprMin1, vexprMin1]))
                                            , (actionPref
                                                      ActOffer {  offers = Set.fromList [
                                                                                Offer { chanid = chanIdA
                                                                                      , chanoffers = []
                                                                                },
                                                                                Offer { chanid = chanIdB
                                                                                      , chanoffers = []
                                                                                }]
                                                                            , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0,
                                                                                                                    cstrEqual vexprOp2pcQ int0,
                                                                                                                    cstrEqual vexprOp3pcR int0
                                                                                            ])
                                                                }
                                                      (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1, vexprMin1]))
                                ])
   procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0, int0]






-- case G = []
-- P[A,B]() := Q[A]() |[]| Q[A]() |[]| R[B]()
-- becomes after LPEPar
-- combination of op1, op2:
--  // only 1
--  A [op1$pc$Q == 0] >-> P[A,B](-1, op2$pc$Q,  op3$pc$Q)
--  // only 2
--  A [op2$pc$Q == 0] >-> P[A,B](op1$pc$Q, -1, op3$pc$Q)
--
-- combination of [op1, op2] and op3
-- // only [1,2] is already given above
-- // only 3
--  B [op3$pc$Q == 0] >-> P[A,B](op1$pc$Q, op2$pc$Q, -1)
--
-- // [1,2] AND 3
--  A | B [op1$pc$Q == 0, op3$pc$Q == 0] >-> P[A,B](-1, op2$pc$Q, -1)              // only if [1] existed after [1,2] and then sync on A possible: i.e. A \in G
--  A | B [op2$pc$Q == 0, op3$pc$Q == 0] >-> P[A,B](op1$pc$Q, -1, -1)
testThreeOperandsDiffChannels1 :: Test
testThreeOperandsDiffChannels1 = TestCase $
 assertBool "test three operands - different channels"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
 where
   procInst'' = procInst procIdP [chanIdA, chanIdB] []
   procIdP = procIdGen "P" [chanIdA] []
   procIdQ = procIdGen "Q" [chanIdA] []
   procIdR = procIdGen "R" [chanIdB] []

   procDefP = ProcDef [chanIdA, chanIdB] [] (
         parallel [] [
             procInst procIdQ [chanIdA] [],
             procInst procIdQ [chanIdA] [],
             procInst procIdR [chanIdB] []
           ]
         )
   procDefQ = ProcDef [chanIdA] [] (
                    actionPref actOfferA stop)
   procDefR = ProcDef [chanIdB] [] (
                   actionPref actOfferB stop)

   procDefs = Map.fromList  [  (procIdP, procDefP)
                             , (procIdQ, procDefQ)
                             , (procIdR, procDefR)]

   varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
   varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
   varIdOp3pcR = VarId (T.pack "op3$pc$R") 0 intSort
   vexprOp1pcQ = cstrVar varIdOp1pcQ
   vexprOp2pcQ = cstrVar varIdOp2pcQ
   vexprOp3pcR = cstrVar varIdOp3pcR

   -- with procInst := P[A](0,0,0)
   procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ, varIdOp3pcR]
   procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ, varIdOp3pcR]
                    (choice  [
                              -- combination of op1, op2:
                              --  // only 1
                              --  A [op1$pc$Q == 0] >-> P[A,B](-1, op2$pc$Q,  op3$pc$Q)
                              --  // only 2
                              --  A [op2$pc$Q == 0] >-> P[A,B](op1$pc$Q, -1, op3$pc$Q)
                              --  // 1 and 2
                              --  A [op1$pc$Q == 0, op2$pc$Q == 0] >-> P[A,B](-1, -1, op3$pc$Q)
                              (actionPref
                                  ActOffer {  offers = Set.fromList [
                                                            Offer { chanid = chanIdA
                                                                  , chanoffers = []
                                                            }]
                                                        , constraint =  cstrEqual vexprOp1pcQ int0


                                            }
                                  (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcQ, vexprOp3pcR]))
                              , (actionPref
                                    ActOffer {  offers = Set.fromList [
                                                              Offer { chanid = chanIdA
                                                                    , chanoffers = []
                                                              }]
                                                          , constraint =  cstrEqual vexprOp2pcQ int0


                                              }
                                    (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprMin1, vexprOp3pcR]))
                                -- , (actionPref
                                --           ActOffer {  offers = Set.fromList [
                                --                                     Offer { chanid = chanIdA
                                --                                           , chanoffers = []
                                --                                     }]
                                --                                 , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0,
                                --                                                                         cstrEqual vexprOp2pcQ int0
                                --                                                 ])
                                --                     }
                                --           (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1, vexprOp3pcR]))


                                -- combination of [op1, op2] and op3
                                -- // only [1,2] is already given above
                                -- // only 3
                                --  B [op3$pc$R == 0] >-> P[A,B](op1$pc$Q, op2$pc$Q, -1)
                                , (actionPref
                                          ActOffer {  offers = Set.fromList [
                                                                    Offer { chanid = chanIdB
                                                                          , chanoffers = []
                                                                    }]
                                                                , constraint =  cstrEqual vexprOp3pcR int0


                                                    }
                                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp2pcQ, vexprMin1]))


                                -- // [1,2] AND 3
                                --  A | B [op1$pc$Q == 0, op3$pc$Q == 0] >-> P[A,B](-1, op2$pc$Q, -1)              // only if [1] existed after [1,2] and then sync on A possible: i.e. A \in G
                                --  A | B [op2$pc$Q == 0, op3$pc$Q == 0] >-> P[A,B](op1$pc$Q, -1, -1)
                                --  A | B [op1$pc$Q == 0, op2$pc$Q == 0, op3$pc$Q == 0] >-> P[A,B](-1, -1, -1)
                                --

                                , (actionPref
                                          ActOffer {  offers = Set.fromList [
                                                                    Offer { chanid = chanIdA
                                                                          , chanoffers = []
                                                                    },
                                                                    Offer { chanid = chanIdB
                                                                          , chanoffers = []
                                                                    }]
                                                      , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp2pcQ int0,
                                                                                              cstrEqual vexprOp3pcR int0
                                                                      ])
                                                    }
                                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprMin1, vexprMin1]))
                                  , (actionPref
                                            ActOffer {  offers = Set.fromList [
                                                                      Offer { chanid = chanIdA
                                                                            , chanoffers = []
                                                                      },
                                                                      Offer { chanid = chanIdB
                                                                            , chanoffers = []
                                                                      }]
                                                                  , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0,
                                                                                                          cstrEqual vexprOp3pcR int0
                                                                                  ])
                                                      }
                                            (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcQ, vexprMin1]))
                                  -- , (actionPref
                                  --           ActOffer {  offers = Set.fromList [
                                  --                                     Offer { chanid = chanIdA
                                  --                                           , chanoffers = []
                                  --                                     },
                                  --                                     Offer { chanid = chanIdB
                                  --                                           , chanoffers = []
                                  --                                     }]
                                  --                                 , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0,
                                  --                                                                         cstrEqual vexprOp2pcQ int0,
                                  --                                                                         cstrEqual vexprOp3pcR int0
                                  --                                                 ])
                                  --                     }
                                  --           (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1, vexprMin1]))
                                ])
   procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0, int0]






-- case G = [A]
-- P[A,B]() := Q[A]() |[A]| Q[A]() |[A]| R[B]()
-- becomes after LPEPar
-- combination of op1, op2: NONE
-- combination of [op1, op2] and op3
-- // only [1,2] is already given above
-- // only 3
--  B [op3$pc$Q == 0] >-> P[A,B](op1$pc$Q, op2$pc$Q, -1)
--
-- // [1,2] AND 3: NONE
-- -------------------------------------------------
testThreeOperandsDiffChannels2 :: Test
testThreeOperandsDiffChannels2 = TestCase $
 assertBool "test three operands - different channels"  $ eqProcDef (Just (procInst', procDefP'))  (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
 where
   procInst'' = procInst procIdP [chanIdA, chanIdB] []
   procIdP = procIdGen "P" [chanIdA] []
   procIdQ = procIdGen "Q" [chanIdA] []
   procIdR = procIdGen "R" [chanIdB] []

   procDefP = ProcDef [chanIdA, chanIdB] [] (
         parallel [chanIdA] [
             procInst procIdQ [chanIdA] [],
             procInst procIdQ [chanIdA] [],
             procInst procIdR [chanIdB] []
           ]
         )
   procDefQ = ProcDef [chanIdA] [] (
                    actionPref actOfferA stop)
   procDefR = ProcDef [chanIdB] [] (
                   actionPref actOfferB stop)

   procDefs = Map.fromList  [  (procIdP, procDefP)
                             , (procIdQ, procDefQ)
                             , (procIdR, procDefR)]

   varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
   varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
   varIdOp3pcR = VarId (T.pack "op3$pc$R") 0 intSort
   vexprOp1pcQ = cstrVar varIdOp1pcQ
   vexprOp2pcQ = cstrVar varIdOp2pcQ
   vexprOp3pcR = cstrVar varIdOp3pcR

   -- with procInst := P[A](0,0,0)
   procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ, varIdOp3pcR]
   procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ, varIdOp3pcR]
                                -- combination of [op1, op2] and op3
                                -- // only [1,2] is already given above
                                -- // only 3
                                --  B [op3$pc$R == 0] >-> P[A,B](op1$pc$Q, op2$pc$Q, -1)
                                (actionPref
                                          ActOffer {  offers = Set.fromList [
                                                                    Offer { chanid = chanIdB
                                                                          , chanoffers = []
                                                                    }]
                                                                , constraint =  cstrEqual vexprOp3pcR int0


                                                    }
                                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprOp2pcQ, vexprMin1]))
   procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0, int0]





-- case G = [B]
-- P[A,B]() := Q[A]() |[B]| Q[A]() |[B]| R[B]()
-- becomes after LPEPar
-- combination of op1, op2:
--  // only 1
--  A [op1$pc$Q == 0] >-> P[A,B](-1, op2$pc$Q,  op3$pc$Q)
--  // only 2
--  A [op2$pc$Q == 0] >-> P[A,B](op1$pc$Q, -1, op3$pc$Q)
-- combination of [op1, op2] and op3
-- // only [1,2] is already given above
-- // only 3: NO
-- // [1,2] AND 3: NO
testThreeOperandsDiffChannels3 :: Test
testThreeOperandsDiffChannels3 = TestCase $
 assertBool "test three operands - different channels" $ eqProcDef (Just (procInst', procDefP')) (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
 where
   procInst'' = procInst procIdP [chanIdA, chanIdB] []
   procIdP = procIdGen "P" [chanIdA] []
   procIdQ = procIdGen "Q" [chanIdA] []
   procIdR = procIdGen "R" [chanIdB] []

   procDefP = ProcDef [chanIdA, chanIdB] [] (
         parallel [chanIdB] [
             procInst procIdQ [chanIdA] [],
             procInst procIdQ [chanIdA] [],
             procInst procIdR [chanIdB] []
           ]
         )
   procDefQ = ProcDef [chanIdA] [] (
                    actionPref actOfferA stop)
   procDefR = ProcDef [chanIdB] [] (
                   actionPref actOfferB stop)

   procDefs = Map.fromList  [  (procIdP, procDefP)
                             , (procIdQ, procDefQ)
                             , (procIdR, procDefR)]

   varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
   varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
   varIdOp3pcR = VarId (T.pack "op3$pc$R") 0 intSort
   vexprOp1pcQ = cstrVar varIdOp1pcQ
   vexprOp2pcQ = cstrVar varIdOp2pcQ
   vexprOp3pcR = cstrVar varIdOp3pcR

   -- with procInst := P[A](0,0,0)
   procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ, varIdOp3pcR]
   procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ, varIdOp3pcR]
                    (choice  [
                              -- combination of op1, op2:
                              --  // only 1
                              --  A [op1$pc$Q == 0] >-> P[A,B](-1, op2$pc$Q,  op3$pc$Q)
                              --  // only 2
                              --  A [op2$pc$Q == 0] >-> P[A,B](op1$pc$Q, -1, op3$pc$Q)
                              --  // 1 and 2
                              --  A [op1$pc$Q == 0, op2$pc$Q == 0] >-> P[A,B](-1, -1, op3$pc$Q)
                              (actionPref
                                  ActOffer {  offers = Set.fromList [
                                                            Offer { chanid = chanIdA
                                                                  , chanoffers = []
                                                            }]
                                                        , constraint =  cstrEqual vexprOp1pcQ int0


                                            }
                                  (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcQ, vexprOp3pcR]))
                              , (actionPref
                                    ActOffer {  offers = Set.fromList [
                                                              Offer { chanid = chanIdA
                                                                    , chanoffers = []
                                                              }]
                                                          , constraint =  cstrEqual vexprOp2pcQ int0


                                              }
                                    (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprMin1, vexprOp3pcR]))
                              ])
   procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0, int0]




-- case G = [A,B]
-- P[A,B]() := Q[A]() |[A,B]| Q[A]() |[A,B]| R[B]()
-- becomes after LPEPar
-- NONE
testThreeOperandsDiffChannels4 :: Test
testThreeOperandsDiffChannels4 = TestCase $
 assertBool "test three operands - different channels" $ eqProcDef (Just (procInst', procDefP')) (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
 where
   procInst'' = procInst procIdP [chanIdA, chanIdB] []
   procIdP = procIdGen "P" [chanIdA] []
   procIdQ = procIdGen "Q" [chanIdA] []
   procIdR = procIdGen "R" [chanIdB] []

   procDefP = ProcDef [chanIdA, chanIdB] [] (
         parallel [chanIdA, chanIdB] [
             procInst procIdQ [chanIdA] [],
             procInst procIdQ [chanIdA] [],
             procInst procIdR [chanIdB] []
           ]
         )
   procDefQ = ProcDef [chanIdA] [] (
                    actionPref actOfferA stop)
   procDefR = ProcDef [chanIdB] [] (
                   actionPref actOfferB stop)

   procDefs = Map.fromList  [  (procIdP, procDefP)
                             , (procIdQ, procDefQ)
                             , (procIdR, procDefR)]

   varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
   varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
   varIdOp3pcR = VarId (T.pack "op3$pc$R") 0 intSort
   vexprOp1pcQ = cstrVar varIdOp1pcQ
   vexprOp2pcQ = cstrVar varIdOp2pcQ
   vexprOp3pcR = cstrVar varIdOp3pcR

   -- with procInst := P[A](0,0,0)
   procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ, varIdOp3pcR]
   procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ, varIdOp3pcR]
                    (choice  [])
   procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0, int0]


-- -------------------------------------------------
-- test channel instantiation at top-level
--     the channel given in the procInst is *NOT* instantiated into the BExpr
--
-- P[A]() := A >-> STOP |[]| A >-> STOP
-- with procInst = P[B]()
-- becomes:
-- P[A](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // only left side
      --    A [op1$pc$P$op1 == 0] >->  P[A](-1, op2$pc$P$op2)
      -- // only right side
      -- ## A [op2$pc$P$op2 == 0] >->  P[A](op1$pc$P$op1, -1)
      -- // both sides: NO

-- with procInst := P[B](0,0)
-- -------------------------------------------------

testChannelInst :: Test
testChannelInst = TestCase $
   assertBool "test channel instantiations" $ eqProcDef (Just (procInst', procDefP')) (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdB] []
      procIdP = procIdGen "P" [chanIdA] []

      procDefP = ProcDef [chanIdA] [] (
            parallel [] [
                actionPref actOfferA stop,
                actionPref actOfferA stop
              ]
            )
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      varIdOp1pcPop1 = VarId (T.pack "op1$pc$P$op1") 0 intSort
      varIdOp2pcPop2 = VarId (T.pack "op2$pc$P$op2") 0 intSort
      vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
      vexprOp2pcPop2 = cstrVar varIdOp2pcPop2

      procIdP' = procIdGen "P" [chanIdA] [varIdOp1pcPop1, varIdOp2pcPop2]
      procDefP' = ProcDef [chanIdA] [varIdOp1pcPop1, varIdOp2pcPop2]
                      (choice [
                          -- // only left side
                          -- A [op1$pc$P$op1 == 0] >->  P[A](-1, op2$pc$P$op2)
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = []
                                                      })
                                                  , constraint = cstrEqual vexprOp1pcPop1 int0
                                      }
                            (procInst procIdP' [chanIdA] [vexprMin1, vexprOp2pcPop2]))
                      , -- // only right side
                        -- ## A [op2$pc$P$op2 == 0] >->  P[A](op1$pc$P$op1, -1)
                        (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    })
                                                , constraint = cstrEqual vexprOp2pcPop2 int0
                                    }
                          (procInst procIdP' [chanIdA] [vexprOp1pcPop1, vexprMin1]))
                      ])

      procInst' = procInst procIdP' [chanIdB] [int0, int0]


-- -------------------------------------------------
-- test channel instantiation: same operands with different channels
-- P[A,B]() := Q[A]() |[]| Q[B]()
-- Q[A]() := A >-> STOP
-- with procInst: P[A,B]()
-- becomes
-- P[A,B](op1$pc$Q, op2$pc$Q) :=
    -- // only left side
    --    A [op1$pc$Q == 0] >->  P[A,B](-1, op2$pc$Q)
    -- // only right side
    -- ## B [op2$pc$Q == 0] >->  P[A,B](op1$pc$Q, -1)
    -- // both sides:
    -- ## A | B [op1$pc$Q == 0, op2$pc$Q == 0] >->  P[A,B](-1, -1)
-- -- with procInst: P[A,B](0,0)
-- -------------------------------------------------

testChannelInst2 :: Test
testChannelInst2 = TestCase $
   assertBool "test channel instantiations 2" $ eqProcDef (Just (procInst', procDefP')) (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procIdQ = procIdGen "Q" [chanIdA] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [] [
                procInst procIdQ [chanIdA] [],
                procInst procIdQ [chanIdB] []
              ]
            )
      procDefQ = ProcDef [chanIdA] [] (
            actionPref actOfferA stop
          )
      procDefs = Map.fromList  [  (procIdP, procDefP)
                               ,  (procIdQ, procDefQ)]

      varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
      varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
      vexprOp1pcQ = cstrVar varIdOp1pcQ
      vexprOp2pcQ = cstrVar varIdOp2pcQ

      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ]
                      (choice [
                          -- // only left side
                          -- A [op1$pc$P$op1 == 0] >->  P[A](-1, op2$pc$P$op2)
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = []
                                                      })
                                                  , constraint = cstrEqual vexprOp1pcQ int0
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcQ]))
                      , -- // only right side
                        -- ## A [op2$pc$P$op2 == 0] >->  P[A](op1$pc$P$op1, -1)
                        (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdB
                                                          , chanoffers = []
                                                    })
                                                , constraint = cstrEqual vexprOp2pcQ int0
                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprMin1]))
                      ,  (actionPref
                          ActOffer {  offers = Set.fromList [
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    },
                                                    Offer { chanid = chanIdB
                                                          , chanoffers = []
                                                    }
                                                    ]
                                                , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0
                                                                                      , cstrEqual vexprOp2pcQ int0
                                                                ])


                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1]))
                      ])

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]


-- -------------------------------------------------
-- test channel instantiation: flipping channels in operands
-- P[A,B]() := Q[A,B]() |[]| Q[B,A]
-- Q[A,B]() := A >-> STOP
-- becomes
-- P[A,B]() :=
-- P[A](op1$pc$P$op1, op2$pc$P$op2) :=
      -- // only left side
      --    A [op1$pc$P$op1 == 0] >->  P[A](-1, op2$pc$P$op2)
      -- // only right side
      -- ## A [op2$pc$P$op2 == 0] >->  P[A](op1$pc$P$op1, -1)
      -- // both sides:
      -- ## A | B [op1$pc$Q == 0, op2$pc$Q == 0] >->  P[A,B](-1, -1)
-- -- with procInst: P[A,B](0,0)
-- -------------------------------------------------

testChannelInst3 :: Test
testChannelInst3 = TestCase $
   assertBool "test channel instantiations 2" $ eqProcDef (Just (procInst', procDefP')) (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []
      procIdQ = procIdGen "Q" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (
            parallel [] [
                procInst procIdQ [chanIdA, chanIdB] [],
                procInst procIdQ [chanIdB, chanIdA] []
              ]
            )
      procDefQ = ProcDef [chanIdA, chanIdB] [] (
            actionPref actOfferA stop
          )
      procDefs = Map.fromList  [  (procIdP, procDefP)
                               ,  (procIdQ, procDefQ)]

      varIdOp1pcQ = VarId (T.pack "op1$pc$Q") 0 intSort
      varIdOp2pcQ = VarId (T.pack "op2$pc$Q") 0 intSort
      vexprOp1pcQ = cstrVar varIdOp1pcQ
      vexprOp2pcQ = cstrVar varIdOp2pcQ

      procIdP' = procIdGen "P" [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ]
      procDefP' = ProcDef [chanIdA, chanIdB] [varIdOp1pcQ, varIdOp2pcQ]
                      (choice [
                          -- // only left side
                          -- A [op1$pc$P$op1 == 0] >->  P[A](-1, op2$pc$P$op2)
                          (actionPref
                            ActOffer {  offers = Set.singleton(
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = []
                                                      })
                                                  , constraint = cstrEqual vexprOp1pcQ int0
                                      }
                            (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprOp2pcQ]))
                      , -- // only right side
                        -- ## A [op2$pc$P$op2 == 0] >->  P[A](op1$pc$P$op1, -1)
                        (actionPref
                          ActOffer {  offers = Set.singleton(
                                                    Offer { chanid = chanIdB
                                                          , chanoffers = []
                                                    })
                                                , constraint = cstrEqual vexprOp2pcQ int0
                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprOp1pcQ, vexprMin1]))
                      ,  (actionPref
                          ActOffer {  offers = Set.fromList [
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = []
                                                    },
                                                    Offer { chanid = chanIdB
                                                          , chanoffers = []
                                                    }
                                                    ]
                                                , constraint =  cstrAnd (Set.fromList [ cstrEqual vexprOp1pcQ int0
                                                                                      , cstrEqual vexprOp2pcQ int0
                                                                ])


                                    }
                          (procInst procIdP' [chanIdA, chanIdB] [vexprMin1, vexprMin1]))
                      ])

      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, int0]



-- -------------------------------------------------
-- test loop detection (recursively nested PAR statements)
-- -------------------------------------------------


-- cycle detection
--  P[]() :=      A >-> STOP
--            ||| P[]()
-- should fail
-- testLoop1 :: Test
-- testLoop1 = TestCase $
--   let err =  "loop (LPEPar) detected in P" in
--   // TODO: handle result with error
--   assertBool "test loop 1" (_, _) (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
--  where
--     procIdP = procIdGen "P" [] []
--     procInst'' = procInst procIdP [] []
--     procDefP = ProcDef [] [] (parallel [] [
--                                 (actionPref actOfferA stop),
--                                 (procInst procIdP [] [])
--                                 ])
--     procDefs = Map.fromList  [  (procIdP, procDefP)]


-- -- cycle detection
-- --  P[]() :=      A >-> STOP
-- --            ||| Q[]()
-- --  Q[]() := P[]()
-- -- should fail
-- testLoop2 :: Test
-- testLoop2 = TestCase $
--   let err =  "loop (LPEPar) detected in P" in
--   // TODO: handle result with error
--   assertBool "test loop 2" (_, _) (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
--  where
--     procIdP = procIdGen "P" [] []
--     procIdQ = procIdGen "Q" [] []
--     procInst'' = procInst procIdP [] []
--     procDefP = ProcDef [] [] (parallel [] [
--                                 (actionPref actOfferA stop),
--                                 (procInst procIdQ [] [])
--                                 ])
--     procDefQ = ProcDef [] [] (choice [
--                                 (actionPref actOfferA stop),
--                                 (procInst procIdP [] [])
--                                 ])
--     procDefs = Map.fromList  [  (procIdP, procDefP),
--                                 (procIdQ, procDefQ)]



-- testLoop3 :: Test
-- testLoop3 = TestCase $
--   let err =  "loop (LPEPar) detected in P" in
--   assertBool "test loop 1" (procInst', procDefP', err) (lpeParTestWrapper procInst'' emptyTranslatedProcDefs procDefs)
--  where
--     procIdP = procIdGen "P" [] []
--     procIdQ = procIdGen "Q" [] []
--     procDefP = ProcDef [] [] (choice [
--                                 (actionPref actOfferAx (procInst procIdP [] [])),
--                                 (procInst procIdQ [] [])
--                                 ])
--     procDefQ = ProcDef [] [] (procInst procIdQ [] [])
--     procDefs = Map.fromList  [  (procIdP, procDefP),
--                                 (procIdQ, procDefQ)]
--
--



----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testLPEParList :: Test
testLPEParList = TestList [ TestLabel "single actions 1" testSingleAction1
                          , TestLabel "single actions 2" testSingleAction2
                          , TestLabel "single actions 3" testSingleAction3
                          , TestLabel "single actions 4" testSingleAction4

                          , TestLabel "single actions, different actions 1" testSingleActionDifferentActions1
                          , TestLabel "single actions, different actions 2" testSingleActionDifferentActions2
                          , TestLabel "single actions, different actions 3" testSingleActionDifferentActions3
                          , TestLabel "single actions, different actions 4" testSingleActionDifferentActions4

                          , TestLabel "multi actions 1" testMultiActions1
                          , TestLabel "multi actions 2" testMultiActions2
                          , TestLabel "multi actions 3" testMultiActions3
                          , TestLabel "multi actions 4" testMultiActions4
                          , TestLabel "multi actions 5" testMultiActions5
                          , TestLabel "multi actions 6" testMultiActions6
                          , TestLabel "multi actions 7" testMultiActions7
                          , TestLabel "multi actions 8" testMultiActions8

                          , TestLabel "params" testParams

                          , TestLabel "multi-sequences 1" testMultiSeq1
                          , TestLabel "multi-sequences 2" testMultiSeq2
                          , TestLabel "multi-sequences 3" testMultiSeq3
                          , TestLabel "multi-sequences 4" testMultiSeq4

                          , TestLabel "three operands 1" testThreeOperands1
                          , TestLabel "three operands 2" testThreeOperands2

                          , TestLabel "channel instantiation: top-level" testChannelInst
                          , TestLabel "channel instantiation: same operands different channels" testChannelInst2
                          , TestLabel "channel instantiation: flipping channels in operands" testChannelInst3

                          , TestLabel "channel instantiation: different channels 1" testThreeOperandsDiffChannels1
                          , TestLabel "channel instantiation: different channels 2" testThreeOperandsDiffChannels2
                          , TestLabel "channel instantiation: different channels 3" testThreeOperandsDiffChannels3
                          , TestLabel "channel instantiation: different channels 4" testThreeOperandsDiffChannels4

                        ]
