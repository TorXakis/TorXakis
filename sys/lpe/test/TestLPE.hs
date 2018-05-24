{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestLPE
(
testLPEList
)
where

import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map

import TxsDefs
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
procIdGen name' chans vars' = ProcId   {  ProcId.name       = T.pack name'
                                        , ProcId.unid       = 111
                                        , ProcId.procchans  = chans
                                        , ProcId.procvars   = vars'
                                        , ProcId.procexit   = NoExit
                                    }

varIdX :: VarId
varIdX = VarId (T.pack "x") 33 intSort
varIdA1 :: VarId
varIdA1 = VarId (T.pack "A$1") 34 intSort
varIdB1 :: VarId
varIdB1 = VarId (T.pack "B$1") 34 intSort

vexprX :: VExpr
vexprX = cstrVar varIdX
vexprA1 :: VExpr
vexprA1 = cstrVar varIdA1
vexprB1 :: VExpr
vexprB1 = cstrVar varIdB1

vexpr0 :: VExpr
vexpr0 = cstrConst (Cint 0)
vexpr1 :: VExpr
vexpr1 = cstrConst (Cint 1)
vexpr2 :: VExpr
vexpr2 = cstrConst (Cint 2)
vexprMin1 :: VExpr
vexprMin1 = cstrConst (Cint (-1))

int0 :: VExpr               -- PvdL : what is the difference with vexpr0?
int0 = cstrConst (Cint 0)
int1 :: VExpr
int1 = cstrConst (Cint 1)
int2 :: VExpr
int2 = cstrConst (Cint 2)
varIdPcP :: VarId
varIdPcP = VarId (T.pack "pc$P") 0 intSort
vexprPcP :: VExpr
vexprPcP = cstrVar varIdPcP


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

-- action: B?x
actOfferBx :: ActOffer
actOfferBx   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Quest varIdX]
                                        }
                        , hiddenvars = Set.empty
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
anyInt :: VExpr
anyInt = cstrConst $ Cany intSort

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

-- expression is translated to GNF first
-- P[A]() := Q[A]()
-- Q[B]() := B?x >-> STOP
-- with procInst = P[A]()
-- becomes after GNF
-- P[A]() := A?x >-> STOP
-- Q[B]() := B?x >-> STOP
-- becomes
-- LPE_P[A](pc$P) := A?A1 [pc$P == 0] >-> LPE_P[A](-1)
-- with procInst = LPE_P[A](0)


testGNFFirst :: Test
testGNFFirst = TestCase $
   assertBool "translation to GNF worked" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst'' procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdB] []

      procDefP = ProcDef [chanIdA] [] (procInst procIdQ [chanIdA] [])
      procDefQ = ProcDef [chanIdB] [] (actionPref actOfferBx stop)

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP]
                      (actionPref
                        -- action: A?A1 [pc$P == 0]
                        ActOffer {  offers = Set.singleton
                                                  Offer { chanid = chanIdA
                                                        , chanoffers = [Quest varIdA1]
                                                  }
                                 , hiddenvars = Set.empty
                                 , constraint = cstrEqual vexprPcP int0
                                 }
                        (procInst procIdPlpe [chanIdA] [vexprMin1]))

      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procInst' = procInst procIdPlpe [chanIdA] [int0]


-- STOP becomes empty choice
-- P[]() := STOP
-- with procInst = P[]()
-- becomes
-- P[](pc$P) :=       // technically: choice []
-- with procInst = LPE_P[](0)
testStop :: Test
testStop = TestCase $
   assertBool "STOP becomes empty choice" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst'' procDefs'))
   where
      procInst'' = procInst procIdP [] []
      procIdP = procIdGen "P" [] []

      procDefP = ProcDef [] [] stop

      procIdPlpe = procIdGen "LPE_P" [] [varIdPcP]
      procDefPlpe = ProcDef [] [varIdPcP] (choice $ Set.fromList [])

      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procInst' = procInst procIdPlpe [] [int0]


-- actionPref with STOP
-- P[A]() := A?x >-> STOP
-- with procInst = P[A]()
-- becomes
-- LPE_P[A](pc$P) := A?A1 [pc$P == 0] >-> LPE_P[A](-1)
-- with procInst = LPE_P[A](0)
testActionPrefStop :: Test
testActionPrefStop = TestCase $
   assertBool "actionPref stop" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst'' procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []

      procDefP = ProcDef [chanIdA] [] (actionPref actOfferAx stop)

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP]
                      (actionPref
                        -- action: A?A1 [pc$P == 0]
                        ActOffer {  offers = Set.singleton
                                                  Offer { chanid = chanIdA
                                                        , chanoffers = [Quest varIdA1]
                                                  }
                                 , hiddenvars = Set.empty
                                 , constraint = cstrEqual vexprPcP int0
                                 }
                        (procInst procIdPlpe [chanIdA] [vexprMin1]))

      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procInst' = procInst procIdPlpe [chanIdA] [int0]



-- actionPref: existing constraints are kept
-- P[A]() := A?x[x==1] >-> STOP
-- with procInst = P[A]()
-- becomes
-- LPE_P[A](pc$P) := A?A1 [pc$P == 0 and A1 == 1] >-> P[A](-1)
-- with procInst = LPE_P[A](0)
testActionPrefConstraints :: Test
testActionPrefConstraints = TestCase $
   assertBool "Action Pref constraints are kept" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst'' procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []

      procDefP = ProcDef [chanIdA] [] (
                        actionPref
                        -- action: A?x [x == 1]
                        ActOffer {  offers = Set.singleton
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = [Quest varIdX]
                                                    }
                                 , hiddenvars = Set.empty
                                 , constraint = cstrEqual vexprX int1
                                 }
                        stop)

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP]
                        (actionPref
                        -- action: A?A1 [pc$P == 0 and x == 1]
                        ActOffer {  offers = Set.singleton
                                                  Offer { chanid = chanIdA
                                                        , chanoffers = [Quest varIdA1]
                                                  }
                                 , hiddenvars = Set.empty
                                 , constraint = cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprPcP int0)
                                                                                         (cstrEqual vexprA1 int1)
                                                                                         (cstrConst (Cbool False))
                                                              ])
                                 }
                        (procInst procIdPlpe [chanIdA] [vexprMin1]))

      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procInst' = procInst procIdPlpe [chanIdA] [int0]



-- actionPref with procInst
-- P[A]() := A?x >-> P[A]()
-- with procInst = P[A]()
-- becomes
-- LPE_P[A](pc$P) := A?A1 [pc$P == 0] >-> LPE_P[A](0)
-- with procInst = LPE_P[A](0)
testActionPrefProcInst :: Test
testActionPrefProcInst = TestCase $
   assertBool "actionPref procInst" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst'' procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []

      procDefP = ProcDef [chanIdA] [] (actionPref actOfferAx (procInst procIdP [chanIdA] []))

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP]
                      (actionPref
                        -- action: A?A1 [pc$P == 0]
                        ActOffer {  offers = Set.singleton
                                                  Offer { chanid = chanIdA
                                                        , chanoffers = [Quest varIdA1]
                                                  }
                                 , hiddenvars = Set.empty
                                 , constraint = cstrEqual vexprPcP int0
                                 }
                        (procInst procIdPlpe [chanIdA] [vexpr0]))

      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procInst' = procInst procIdPlpe [chanIdA] [int0]


-- choice operator
-- P[A]() :=    A?x >-> STOP
--          ##  A?x >-> P[A]()
-- with procInst = P[A]()
-- becomes
-- P$LP$[A](pc$P) :=     A?A1 [pc$P == 0] >-> LPE_P[A](-1)
--                    ## A?A1 [pc$P == 0] >-> LPE_P[A](0)
-- with procInst = LPE_P[A](0)
testChoice :: Test
testChoice = TestCase $
   assertBool "choice" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst''  procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []

      procDefP = ProcDef [chanIdA] [] (
                        choice $ Set.fromList [ actionPref actOfferAx stop
                                              , actionPref actOfferAx (procInst procIdP [chanIdA] [])
                                              ])

      -- action: A?A1 [pc$P == 0]
      actOfferA1P0 = ActOffer {  offers = Set.singleton
                                              Offer { chanid = chanIdA
                                                    , chanoffers = [Quest varIdA1]
                                              }
                              , hiddenvars = Set.empty
                              , constraint = cstrEqual vexprPcP int0
                              }

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (choice $ Set.fromList [ actionPref actOfferA1P0 (procInst procIdPlpe [chanIdA] [vexprMin1])
                                                                        , actionPref actOfferA1P0 (procInst procIdPlpe [chanIdA] [vexpr0])
                                                                        ])


      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procInst' = procInst procIdPlpe [chanIdA] [int0]


-- multiple ProcDefs: simple case
-- P[A]() := A?x >-> Q[A]()
-- Q[B]() := B?x >-> STOP
-- with procInst = P[A]()
-- becomes
-- LPE_P[A,B](pc$P) :=    A?A1[pc$P == 0] >-> LPE_P[A,B](1)
--                     ## A?A1[pc$P == 1] >-> LPE_P[A,B](-1)
-- with procInst = LPE_P[A](0)
testMultipleProcDefs1 :: Test
testMultipleProcDefs1 = TestCase $
   assertBool "multiple ProcDefs simple" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst'' procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdB] []

      procDefP = ProcDef [chanIdA] [] (actionPref actOfferAx (procInst procIdQ [chanIdA] []))
      procDefQ = ProcDef [chanIdB] [] (actionPref actOfferBx stop)

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP]
                      (choice $ Set.fromList [
                          actionPref
                            -- action: A?A1 [pc$P == 0]
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int0
                                     }
                            (procInst procIdPlpe [chanIdA] [vexpr1])
                      , actionPref
                            -- action: A?A1 [pc$P == 1]
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int1
                                     }
                            (procInst procIdPlpe [chanIdA] [vexprMin1])
                      ])

      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procInst' = procInst procIdPlpe [chanIdA] [int0]



-- multiple ProcDefs: circular reference
-- P[A]() := A?x >-> Q[A]()
-- Q[B]() := B?x >-> P[B]()
-- with procInst = P[A]()
-- becomes
-- LPE_P[A](pc$P) :=    A?A1[pc$P == 0] >-> LPE_P[A](1)
--                   ## A?A1[pc$P == 1] >-> LPE_P[A](0)
-- with procInst = LPE_P[A](0)
testMultipleProcDefs2 :: Test
testMultipleProcDefs2 = TestCase $
   assertBool "multiple ProcDefs simple" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst'' procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] []

      procDefP = ProcDef [chanIdA] [] (actionPref actOfferAx (procInst procIdQ [chanIdA] []))
      procDefQ = ProcDef [chanIdB] [] (actionPref actOfferBx (procInst procIdP [chanIdB] []))

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP]
                      (choice $ Set.fromList [
                          actionPref
                            -- action: A?A1 [pc$P == 0]
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int0
                                     }
                            (procInst procIdPlpe [chanIdA] [vexpr1])
                      , actionPref
                            -- action: A?A1 [pc$P == 1]
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int1
                                     }
                            (procInst procIdPlpe [chanIdA] [vexpr0])
                      ])

      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procInst' = procInst procIdPlpe [chanIdA] [int0]





-- multiple ProcDefs: removal of a STOP ProcDef
-- P[A]() :=   A?x >-> Q[A]()
--          ## A?x >-> R[A]()
-- Q[B]() := STOP               // Q will be removed, thus the program counter will point to a non-existing instance: that's ok
-- R[B]() := B?x >-> P[B]()
-- with procInst = P[A]()
-- becomes
-- LPE_P[A](pc$P) :=      A?A1[pc$P == 0] >-> LPE_P[A](1)
--                    ##  A?A1[pc$P == 0] >-> LPE_P[A](2)
--                    ##  A?A1[pc$P == 2] >-> LPE_P[A](0)
-- with procInst = LPE_P[A](0)
--  // NOTE: there is no step for pc$P == 1, which means it's a dead end (i.e. STOP)
testMultipleProcDefs3 :: Test
testMultipleProcDefs3 = TestCase $
   assertBool "multiple ProcDefs simple" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst''  procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdB] []
      procIdR = procIdGen "R" [chanIdB] []

      procDefP = ProcDef [chanIdA] [] (choice $ Set.fromList [ actionPref actOfferAx (procInst procIdQ [chanIdA] [])
                                              , actionPref actOfferAx (procInst procIdR [chanIdA] [])
                                      ])
      procDefQ = ProcDef [chanIdB] [] stop
      procDefR = ProcDef [chanIdB] [] (actionPref actOfferBx (procInst procIdP [chanIdB] []))

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP]
                      (choice $ Set.fromList [
                          actionPref
                            -- action: A?A1 [pc$P == 0]
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int0
                                     }
                            (procInst procIdPlpe [chanIdA] [vexpr1])
                      , actionPref
                            -- action: A?A1 [pc$P == 0]
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int0
                                     }
                            (procInst procIdPlpe [chanIdA] [vexpr2])
                      , actionPref
                            -- action: A?A1 [pc$P == 2]
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int2
                                     }
                            (procInst procIdPlpe [chanIdA] [vexpr0])
                      ])

      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)
                                , (procIdR, procDefR)]
      procInst' = procInst procIdPlpe [chanIdA] [int0]






-- channels: identity of ProcDefs includes the channel list!
-- P[A,B]() :=    A?x >-> Q[A](x)
--            ##  A?x >-> Q[B](x)
-- Q[A](x) := A!x >-> STOP
-- with procInst = P[A,B]()
-- becomes
-- // results in two separate instantiations of Q!
-- LPE_P[A,B](pc$P, Q$A$x, Q$B$x) :=   A?A1 [pc$P == 0] >-> LPE_P[A,B](1,A1, Q$B$x)
--                                  ## A?A1 [pc$P == 0] >-> LPE_P[A,B](2,Q$A$x, A1)
--                                  ## A!A1 [pc$P == 1, A1 == Q$A$x] >-> LPE_P[A,B](-1,ANY,ANY)
--                                  ## B!B1 [pc$P == 2, B1 == Q$B$x] >-> LPE_P[A,B](-1,ANY,ANY)
-- with procInst = LPE_P[A,B](0, ANY, ANY)
testProcDefIdentity :: Test
testProcDefIdentity = TestCase $
   assertBool "ProcDef identity" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst''  procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA,chanIdB] []
      procIdQ = procIdGen "Q" [chanIdA] [varIdX]

      procDefP = ProcDef [chanIdA,chanIdB] [] (choice $ Set.fromList [ actionPref actOfferAx (procInst procIdQ [chanIdA] [vexprX])
                                                      , actionPref actOfferAx (procInst procIdQ [chanIdB] [vexprX])
                                                      ])
      procDefQ = ProcDef [chanIdA] [varIdX] (actionPref actOfferAExclamX stop)

      procIdPlpe = procIdGen "LPE_P" [chanIdA, chanIdB] [varIdPcP, varIdQAx, varIdQBX]
      varIdQAx = VarId (T.pack "Q$A$x") 33 intSort
      varIdQBX = VarId (T.pack "Q$B$x") 33 intSort
      vexprQAx = cstrVar varIdQAx
      vexprQBx = cstrVar varIdQBX

      procDefPlpe = ProcDef [chanIdA, chanIdB] [varIdPcP, varIdQAx, varIdQBX]
                      (choice $ Set.fromList [
                          --  A?A1 [pc$P == 0] >-> LPE_P(1,A1, Q$B$x)
                          actionPref
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int0
                                     }
                            (procInst procIdPlpe [chanIdA, chanIdB] [vexpr1, vexprA1, vexprQBx])
                      , -- A?A1 [pc$P == 0] >-> LPE_P(2,Q$A$x, A1)
                        actionPref
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int0
                                     }
                            (procInst procIdPlpe [chanIdA, chanIdB] [vexpr2, vexprQAx, vexprA1])
                      , -- A!A1 [pc$P == 1, A1 == Q$A$x] >-> LPE_P(-1,ANY,ANY)
                        actionPref
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrAnd (Set.fromList [cstrITE (cstrEqual vexprPcP int1)
                                                                                             (cstrEqual vexprA1 vexprQAx)
                                                                                             (cstrConst (Cbool False))
                                                                                        ])
                                     }
                            (procInst procIdPlpe [chanIdA, chanIdB] [vexprMin1, anyInt, anyInt])
                      , -- B!B1 [pc$P == 2, B1 == Q$B$x] >-> LPE_P(-1,ANY,ANY)
                        actionPref
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdB
                                                            , chanoffers = [Quest varIdB1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrAnd (Set.fromList [ cstrITE (  cstrEqual vexprPcP int2)
                                                                                             (cstrEqual vexprB1 vexprQBx)
                                                                                             (cstrConst (Cbool False))
                                                                                        ])
                                     }
                            (procInst procIdPlpe [chanIdA, chanIdB] [vexprMin1, anyInt, anyInt])
                      ])

      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procInst' = procInst procIdPlpe [chanIdA, chanIdB] [int0, anyInt, anyInt]





-- parameters are made unique
-- P[A]() := A?x >-> Q[A](x)      // introduce param and pass it to Q
-- Q[A](x) := A!x >-> R[A](x)     // use the passed param and pass the param on to R
-- R[A](x) := A!x >-> STOP        // use the param passed from Q (!) again
-- with procInst = P[A]()
-- becomes
-- LPE_P[A](pc$P, Q$A$x, R$A$x) :=    A?A1 [pc$P == 0]  >-> LPE_P[A](1, A1, R$A$x)
--                                ##  A!A1 [pc$P == 1, A1 == Q$A$x] >-> LPE_P[A](2, Q$A$x, Q$A$x)
--                                ##  A!A1 [pc$P == 1, A1 == R$A$x] >-> LPE_P[A](-1, ANY, ANY)
-- with procInst = LPE_P[A](0, ANY, ANY)
testParamsUnique :: Test
testParamsUnique = TestCase $
   assertBool "params unique" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst'' procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] [varIdX]
      procIdR = procIdGen "R" [chanIdA] [varIdX]

      procDefP = ProcDef [chanIdA] [] (actionPref actOfferAx (procInst procIdQ [chanIdA] [vexprX]))
      procDefQ = ProcDef [chanIdA] [varIdX] (actionPref actOfferAExclamX (procInst procIdR [chanIdA] [vexprX]))
      procDefR = ProcDef [chanIdA] [varIdX] (actionPref actOfferAExclamX stop)

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP, varIdQAx, varIdRAx]
      varIdQAx = VarId (T.pack "Q$A$x") 33 intSort
      varIdRAx = VarId (T.pack "R$A$x") 33 intSort
      vexprQAx = cstrVar varIdQAx
      vexprRAx = cstrVar varIdRAx

      procDefPlpe = ProcDef [chanIdA] [varIdPcP, varIdQAx, varIdRAx]
                      (choice $ Set.fromList [
                          --  A?A1 [pc$P == 0]  >-> LPE_P[A](1, A1, R$A$x)
                          actionPref
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty

                                     , constraint = cstrEqual vexprPcP int0
                                     }
                            (procInst procIdPlpe [chanIdA] [vexpr1, vexprA1, vexprRAx])
                      , -- A!A1 [pc$P == 1, A1 == Q$A$x] >-> LPE_P[A](2, Q$A$x, Q$A$x)
                        actionPref
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrAnd (Set.fromList [ cstrITE (  cstrEqual vexprPcP int1)
                                                                                             (cstrEqual vexprA1 vexprQAx)
                                                                                             (cstrConst (Cbool False))
                                                                                        ])

                                     }
                            (procInst procIdPlpe [chanIdA] [vexpr2, vexprQAx, vexprQAx])
                        , -- A!A1 [pc$P == 2, A1 == R$A$x] >-> LPE_P[A](-1, ANY, ANY)
                          actionPref
                              ActOffer {  offers = Set.singleton
                                                        Offer { chanid = chanIdA
                                                              , chanoffers = [Quest varIdA1]
                                                        }
                                       , hiddenvars = Set.empty
                                       , constraint = cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprPcP int2)
                                                                                               (cstrEqual vexprA1 vexprRAx)
                                                                                               (cstrConst (Cbool False))
                                                                                          ])

                                       }
                              (procInst procIdPlpe [chanIdA] [vexprMin1, anyInt, anyInt])
                      ])

      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)
                                , (procIdR, procDefR)]
      procInst' = procInst procIdPlpe [chanIdA] [int0, anyInt, anyInt]



-- channel switching
-- P[A,B]() := A!1 >-> P[B,A]()
-- becomes
-- LPE_P[A,B](pc$P) :=    A?A1 [pc$P == 0, A1 == 1] >-> LPE_P[A,B](1)
--                    ##  B?B1 [pc$P == 1, B1 == 1] >-> LPE_P[A,B](0)
-- with procInst = LPE_P[A,B](0)
testChannelSwitch :: Test
testChannelSwitch = TestCase $
   assertBool "switching channels" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst'' procDefs'))
   where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] []

      procDefP = ProcDef [chanIdA, chanIdB] [] (actionPref actOfferA1 (procInst procIdP [chanIdB, chanIdA] []))

      procIdPlpe = procIdGen "LPE_P" [chanIdA, chanIdB] [varIdPcP]

      procDefPlpe = ProcDef [chanIdA, chanIdB] [varIdPcP]
                      (choice $ Set.fromList [
                          --  A?A1 [pc$P == 0, A1 == 1] >-> LPE_P[A,B](1)
                          actionPref
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprPcP int0)
                                                                                             (cstrEqual vexprA1 int1)
                                                                                             (cstrConst (Cbool False))
                                                                                        ])
                                     }
                            (procInst procIdPlpe [chanIdA, chanIdB] [vexpr1])
                      , -- B?B1 [pc$P == 1, B1 == 1] >-> LPE_P[A,B](0)
                        actionPref
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdB
                                                            , chanoffers = [Quest varIdB1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrAnd (Set.fromList [  cstrITE (cstrEqual vexprPcP int1)
                                                                                             (cstrEqual vexprB1 int1)
                                                                                             (cstrConst (Cbool False))
                                                                                        ])

                                     }
                            (procInst procIdPlpe [chanIdA, chanIdB] [vexpr0])
                    ])

      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procInst' = procInst procIdPlpe [chanIdA, chanIdB] [int0]



-- more complete example including necessity for GNF translation
-- P[A,B](s) := A?x >-> B!s >-> STOP
-- becomes with GNF:
-- P[A,B](s) := A?x >-> P$gnf1[A,B](s,x)
-- P$gnf1[A,B](s,x) := B!s >-> STOP
-- with procInst = P[A,B](1)
-- becomes
-- LPE_P[A,B](pc$P, P$A$B$s, P$gnf1$A$B$s, P$gnf1$A$B$x) :=
--       A?A1 [pc$P == 0] >-> LPE_P[A,B](1, P$A$B$s, P$A$B$s, A1)
--    ## B?B1 [pc$P == 1, B1 ==  P$gnf1$A$B$s] >-> LPE_P[A,B](-1, ANY, ANY, ANY)
-- with procInst = LPE_P[A,B](0,1,ANY,ANY)
testMultiAction :: Test
testMultiAction = TestCase $
   assertBool "multi action" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst'' procDefs'))
   where
      varIdS = VarId (T.pack "s") 33 intSort
      vexprS = cstrVar varIdS
      procInst'' = procInst procIdP [chanIdA, chanIdB] [int1]
      procIdP = procIdGen "P" [chanIdA, chanIdB] [varIdS]

      procDefP = ProcDef [chanIdA, chanIdB] [varIdS] (actionPref actOfferAx
                                              (actionPref
                                                  ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdB
                                                            , chanoffers = [Exclam vexprS]
                                                      }
                                                  , hiddenvars = Set.empty
                                                  , constraint = cstrConst (Cbool True)
                                                  }
                                                  stop
                                                ))

      procIdPlpe = procIdGen "LPE_P" [chanIdA, chanIdB] [varIdPcP, varIdPABs, varIdPgnf1ABs, varIdPgnf1ABx]
      varIdPABs = VarId (T.pack "P$A$B$s") 33 intSort
      varIdPgnf1ABs = VarId (T.pack "P$gnf1$A$B$s") 33 intSort
      varIdPgnf1ABx = VarId (T.pack "P$gnf1$A$B$x") 33 intSort

      vexprPABs = cstrVar varIdPABs
      vexprPgnf1ABs = cstrVar varIdPgnf1ABs
      -- vexprPgnf1ABx = cstrVar varIdPgnf1ABx            -- PvdL: not used, but was that intentionally?

      procDefPlpe = ProcDef [chanIdA, chanIdB] [varIdPcP, varIdPABs, varIdPgnf1ABs, varIdPgnf1ABx]
                      (choice $ Set.fromList [
                          --  A?A1 [pc$P == 0] >-> LPE_P[A,B](1, P$A$B$s, P$gnf1$A$B$s, A1)
                          actionPref
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int0
                                     }
                            (procInst procIdPlpe [chanIdA, chanIdB] [vexpr1, vexprPABs, vexprPABs, vexprA1])
                      , -- B?B1 [pc$P == 1, B1 ==  P$gnf1$A$B$s] >-> LPE_P[A,B](-1, ANY, ANY, ANY)
                        actionPref
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdB
                                                            , chanoffers = [Quest varIdB1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprPcP int1)
                                                                                             (cstrEqual vexprB1 vexprPgnf1ABs)
                                                                                             (cstrConst (Cbool False))
                                                                                        ])

                                     }
                            (procInst procIdPlpe [chanIdA, chanIdB] [vexprMin1, anyInt, anyInt, anyInt])
                      ])

      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procInst' = procInst procIdPlpe [chanIdA, chanIdB] [int0, int1, anyInt, anyInt]


-- Channel instantiation not executed for top-level procInst
-- P[A]() := A?x >-> STOP
-- with procInst = P[B]()
-- becomes
-- LPE_P[A](pc$P) := A?A1 [pc$P == 0] >-> LPE_P[A](-1)
-- with procInst = LPE_P[B](0)
testChannelInstantiation :: Test
testChannelInstantiation = TestCase $
   assertBool "actionPref stop" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst'' procDefs'))
   where
      procInst'' = procInst procIdP [chanIdB] []
      procIdP = procIdGen "P" [chanIdA] []

      procDefP = ProcDef [chanIdA] [] (actionPref actOfferAx stop)

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP]
                      (actionPref
                        -- action: A?A1 [pc$P == 0]
                        ActOffer {  offers = Set.singleton
                                                  Offer { chanid = chanIdA
                                                        , chanoffers = [Quest varIdA1]
                                                  }
                                 , hiddenvars = Set.empty
                                 , constraint = cstrEqual vexprPcP int0
                                 }
                        (procInst procIdPlpe [chanIdA] [vexprMin1]))

      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procInst' = procInst procIdPlpe [chanIdB] [int0]



-- -------------------------------------------------
-- LPEPar integration (it's called at preGNF level)
-- -------------------------------------------------

-- test parallel bexpr at lower level
-- P[A]() := A?x >-> (A!x >-> STOP || A!x >-> STOP)
-- with procInst P[A]()
-- first preGNF
-- P[A]() := A?x >-> P$pre1[A](x)
-- P$pre1[A](x) := A!x >-> STOP || A!x >-> STOP
-- then parallel translation:
-- P$pre1[A](op1$pc$P$pre1$op1, op1$P$pre1$x, op2$pc$P$pre1$op2, op2$P$pre1$x) :=
--   A?A1 [op1$pc$P$pre1$op1 == 0, op2$pc$P$pre1$op2 == 0,
--         A1 == op1$P$pre1$x, A1 == op2$P$pre1$x] >->  P$pre1[A](-1, ANY, -1, ANY)
--  with P$pre1[A](0, x, 0, x) // which is put into P[A]() !!!

-- LPE of P becomes:
-- P[A](pc$P, P$pre1$A$op1$pc$P$pre1$op1, P$pre1$A$op1$P$pre1$op1$A$x, P$pre1$A$op2$pc$P$pre1$op2, P$pre1$A$op2$P$pre1$op2$A$x) :=
--    A?A1 [pc$P == 0] >-> P[A](1, x, 0, x, 0)
-- ## A?A1 [pc$P == 1,
--          P$pre1$A$op1$pc$P$pre1$op1 == 0, P$pre1$A$op2$pc$P$pre1$op2 == 0,
--          A1 == P$pre1$A$op1$P$pre1$x, A1 == P$pre1$A$op2$P$pre1$x] >->  P$pre1[A](1, -1, ANY, -1, ANY)
--    // note that pc$P is set to 1, but the pc=1 for each operand makes sure, the STOP is translated correctly
-- resulting procInst is put in the original place in preGNF!!
-- with procInst: P[A](0, ANY, ANY, ANY, ANY)

-- description of the param names:
    --  P$pre1$A$ op1$  P$pre1$op1$A$x
    --                  name after LPE of left operator: temporary ProcId = P$pre1$op1!
    --                  full process name + var name
    --            made unique during parallel translation: prefix of operator nr
    --  name after LPE of temporary procDef P$pre1$A

testLPEPar :: Test
testLPEPar = TestCase $
   -- assertEqual "test LPEPar integration"  (Just (procInst', procDefP')) (lpeTransformFunc procInst'' procDefs')
   assertBool "test LPEPar integration" $ eqProcDef (Just (procInst', procDefP')) (lpeTransformFunc procInst'' procDefs')
   where
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []

      procDefP = ProcDef [chanIdA] [] (
            actionPref actOfferAx (
                parallel (Set.singleton chanIdA) [
                    actionPref actOfferAExclamX stop,
                    actionPref actOfferAExclamX stop
                  ]
                ))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      -- pc$P, P$pre1$A$op1$pc$P$pre1$op1, P$pre1$A$op1$P$pre1$x, P$pre1$A$op2$pc$P$pre1$op2, P$pre1$A$op2$P$pre1$x) :=
      varIdpcP = VarId (T.pack "pc$P") 0 intSort
      varIdOp1pc = VarId (T.pack "P$pre1$A$op1$pc$P$pre1$op1") 0 intSort

      varIdOp1x = VarId (T.pack "P$pre1$A$op1$P$pre1$op1$A$x") 33 intSort


      varIdOp2pc = VarId (T.pack "P$pre1$A$op2$pc$P$pre1$op2") 0 intSort
      varIdOp2x = VarId (T.pack "P$pre1$A$op2$P$pre1$op2$A$x") 33 intSort

      vexprpcP = cstrVar varIdpcP
      vexprOp1pc = cstrVar varIdOp1pc
      vexprOp1x = cstrVar varIdOp1x
      vexprOp2pc = cstrVar varIdOp2pc
      vexprOp2x = cstrVar varIdOp2x

      procIdP' = procIdGen "LPE_P" [chanIdA] [varIdpcP, varIdOp1pc, varIdOp1x, varIdOp2pc, varIdOp2x]
      procDefP' = ProcDef [chanIdA] [varIdpcP, varIdOp1pc, varIdOp1x, varIdOp2pc, varIdOp2x] (
                    choice $ Set.fromList [
                        --    A?A1 [pc$P == 0] >-> P[A](1, x, 0, x, 0)
                        actionPref
                          ActOffer {  offers = Set.singleton
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = [Quest varIdA1]
                                                    }
                                   , hiddenvars = Set.empty
                                   , constraint =  cstrEqual vexprpcP int0
                                   }
                          (procInst procIdP' [chanIdA] [int1, int0, vexprA1, int0, vexprA1])
                          -- ## A?A1 [pc$P == 1,
                          --          P$pre1$A$op1$pc$P$pre1$op1 == 0, P$pre1$A$op2$pc$P$pre1$op2 == 0,
                          --          A1 == P$pre1$A$op1$P$pre1$x, A1 == P$pre1$A$op2$P$pre1$x] >->  P$pre1[A](-1, -1, ANY, -1, ANY)
                          , actionPref
                          ActOffer {  offers = Set.singleton
                                                    Offer { chanid = chanIdA
                                                          , chanoffers = [Quest varIdA1]
                                                    }
                                   , hiddenvars = Set.empty
                                   , constraint =  cstrAnd (Set.fromList [ cstrITE (cstrEqual vexprpcP int1)
                                                                                            (cstrAnd (Set.fromList [
                                                                                                cstrITE (cstrEqual vexprOp1pc int0)
                                                                                                   (cstrEqual vexprA1 vexprOp1x)
                                                                                                   (cstrConst (Cbool False)),
                                                                                                cstrITE (cstrEqual vexprOp2pc int0)
                                                                                                  (cstrEqual vexprA1 vexprOp2x)
                                                                                                  (cstrConst (Cbool False))
                                                                                            ]))
                                                                                           (cstrConst (Cbool False))
                                                                ])


                                   }
                          (procInst procIdP' [chanIdA] [int1, vexprMin1, anyInt, vexprMin1, anyInt])])

      procInst' = procInst procIdP' [chanIdA] [int0, anyInt, anyInt, anyInt, anyInt]


-- -------------------------------------------------
-- test parallel nesting
-- e.g. P[A]() := Q[A]() || Q[B]()
-- Q[A]() := A >-> STOP || A >-> STOP
-- -------------------------------------------------


----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testLPEList :: Test
testLPEList = TestList [  TestLabel "translation to GNF did work" testGNFFirst
                        , TestLabel "STOP becomes empty choice" testStop
                        , TestLabel "actionPref stop" testActionPrefStop
                        , TestLabel "actionPref Constraints are kept" testActionPrefConstraints
                        , TestLabel "actionPref procInst" testActionPrefProcInst
                        , TestLabel "choice" testChoice
                        , TestLabel "Multiple ProcDefs simple" testMultipleProcDefs1
                        , TestLabel "Multiple ProcDefs circular" testMultipleProcDefs2
                        , TestLabel "Multiple ProcDefs removal of STOP" testMultipleProcDefs3
                        , TestLabel "ProcDef Identity" testProcDefIdentity
                        , TestLabel "Params are made unique" testParamsUnique
                        , TestLabel "switching channels" testChannelSwitch
                        , TestLabel "multi action" testMultiAction
                        , TestLabel "channel instantiation not for top-level procInst" testChannelInstantiation

                        , TestLabel "lpePar integration" testLPEPar
                        ]
