{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestLPEPar
(
testLPEParList
)
where
import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map

import TxsDefs
import TxsShow
import ProcId
import ChanId
import SortId
import qualified Data.Text         as T

import LPEPar
import TranslatedProcDefs

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

emptyTranslatedProcDefs = TranslatedProcDefs { TranslatedProcDefs.lPreGNF = []
                                             , TranslatedProcDefs.lGNF = []
                                             , TranslatedProcDefs.lLPE = [] }

procIdGen :: String -> [ChanId] -> [VarId] -> ProcId
procIdGen name chans vars = ProcId   {    ProcId.name       = T.pack name
                                        , ProcId.unid       = 111
                                        , ProcId.procchans  = chans
                                        , ProcId.procvars   = vars
                                        , ProcId.procexit   = NoExit
                                    }

varIdX = VarId (T.pack "x") 33 intSort
varIdY = VarId (T.pack "y") 34 intSort
varIdA1 = VarId (T.pack "A1") 34 intSort
varIdB1 = VarId (T.pack "B1") 34 intSort

vexprX = cstrVar varIdX
vexprA1 = cstrVar varIdA1
vexprB1 = cstrVar varIdB1

vexpr0 = cstrConst (Cint 0)
vexpr1 = cstrConst (Cint 1)
vexpr2 = cstrConst (Cint 2)
vexprMin1 = cstrConst (Cint (-1))

int0 = (cstrConst (Cint 0))
int1 = (cstrConst (Cint 1))
int2 = (cstrConst (Cint 2))
varIdPcP = VarId (T.pack "pc$P") 0 intSort
vexprPcP = cstrVar varIdPcP


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
-- action: A!x
actOfferAExclamX   = ActOffer {  offers = Set.singleton(
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Exclam vexprX]
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

-- action: B?x
actOfferBx   = ActOffer {  offers = Set.singleton(
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Quest varIdX]
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

anyInt = cstrConst $ Cany intSort

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------


-- STOP becomes empty choice
-- P[]() := STOP
-- with procInst = P[]()
-- becomes
-- P[](pc$P) :=       // technically: Choice []
-- with procInst = P$LPE[](0)
testStop :: Test
testStop = TestCase $
   assertEqual "STOP becomes empty Choice" (procInst', procDefs) (lpePar procInst emptyTranslatedProcDefs procDefs)
   where
      procInst = ProcInst procIdP [] []
      procIdP = procIdGen "P" [] []

      procDefP = ProcDef [] [] Stop

      procIdPlpe = procIdGen "P$LPE" [] [varIdPcP]
      procDefPlpe = ProcDef [] [varIdPcP] (Choice [])

      procDefs = Map.fromList  [  (procIdP, procDefP)]
      procInst' = ProcInst procIdPlpe [] [int0]


-- testing parallel semantics for single-actions
-- A >-> STOP |[]| A >-> STOP
-- A >-> STOP |[A]| A >-> STOP
-- A >-> STOP |[B]| A >-> STOP
-- A >-> STOP |[A,B]| A >-> STOP

-- A >-> STOP |[]| B >-> STOP
-- A >-> STOP |[A]| B >-> STOP
-- A >-> STOP |[B]| B >-> STOP
-- A >-> STOP |[A,B]| B >-> STOP


-- testing parallel semantics for multi-actions
-- A | B >-> STOP  |[]| A >-> STOP
-- A | B >-> STOP  |[A]| A >-> STOP
-- A | B >-> STOP  |[B]| A >-> STOP
-- A | B >-> STOP  |[A,B]| A >-> STOP

-- A | B >-> STOP  |[]| A | B >-> STOP
-- A | B >-> STOP  |[A]| A | B >-> STOP
-- A | B >-> STOP  |[B]| A | B >-> STOP
-- A | B >-> STOP  |[A,B]| A | B >-> STOP


-- algorithm tests
-- params: none
-- P[A]() := A >-> STOP |[]| A >-> STOP
-- ProcInst: P[A]()
-- become
-- P[A]() := P$op1[A]() |[]| P$op2[A]()
-- P$op1[A]() :=  A >-> STOP
-- P$op2[A]() :=  A >-> STOP
-- becomes after op-LPE translation
-- P[A]() := P$op1[A](0) |[]| P$op2[A](0)
-- P$op1[A](pc$P$op1) :=  A [pc$P$op1 == 0] >-> STOP
-- P$op2[A](pc$P$op2) :=  A [pc$P$op2 == 0] >-> STOP
-- becomes
-- P[A](pc$P$op1, pc$P$op2) := A [pc$P$op1 == 0] >-> STOP
--                          ## A [pc$P$op2 == 0] >-> STOP
-- with ProcInst := P[A](0,0)


-- params: by P and Q / R
-- P[A](s) := Q[A](s) |[]| R[A](s)
-- Q[A](s) := A!s >-> STOP
-- R[A](s) := A!s >-> STOP
-- ProcInst: P[A](1)
-- becomes
-- P[A](op1$pc$Q, op1$s, op2$pc$R, op2$s) :=
--         A?A1
-- ProcInst := P[A](0, s, 0, s)







-- multi action prefixes
-- P[A,B]() := Q[A,B]() |G| Q[A,B]()
-- Q[A,B]() := A?x >-> B!x >-> STOP
-- with ProcInst := P[A,B]()
-- We try this in the following for G = [], [A], [B], [A,B]

-- the general case: generate all possible step combinations (allowed or not)
-- becomes after operand LPE translation for the operands:
-- P[A,B]() := Q[A,B](0, ANY) |G| Q[A,B](0, ANY)
-- Q[A,B](pc$Q, x) :=    A?A1 [pc$Q == 0] >-> Q[A,B](1, A1)
--                    ## B!B1 [pc$Q == 1, B1 == x] >-> Q[A,B](-1, ANY)
-- becomes after step combination
-- P[A,B](op1$pc$Q, op1$x, op2$pc$Q, op2$x) :=
--        // only op1
--        A?A1 [op1$pc$Q == 0]              >-> P[A,B](1, A1, op2$pc$Q, op2$x)
--        B?B1 [op1$pc$Q == 1, B1 == op1$x] >-> P[A,B](-1, ANY, op2$pc$Q, op2$x)
--                                // note: the right side is still allowed to continue! that's intended behaviour.
--        // only op2
--        A?A1 [op2$pc$Q == 0]              >-> P[A,B](op1$pc$Q, op1$x, 1, A1)
--        B?B1 [op2$pc$Q == 1, B1 == op2$x] >-> P[A,B](op1$pc$Q, op1$x, -1, ANY)
--        // both op1 and op2
--        // 1,1 : only if A \in G, but G could be more
--        A?A1 [op1$pc$Q == 0, op2$pc$Q == 0] >-> P[A,B](1, A1, 1, A1)
--        // 1,2 : only if G is empty: |[]|
--        A?A1 | B?B1 [op1$pc$Q == 0, op2$pc$Q == 1, B1 == op2$x] >-> P[A,B](1, A1, -1, ANY)
--        // 2,1 : only if G is empty: |[]|
--        B?B1 | A?A1 [op1$pc$Q == 1, op2$pc$Q == 0, B1 == op1$x] >-> P[A,B](-1, ANY, 1, A1)
--        // 2,2 : only if B \in G, but G could be more...
--        B?B1 [op1$pc$Q == 1, op2$pc$Q == 1] >-> P[A,B](-1, ANY, -1, ANY)
--  with ProcInst = P[A,B](0, ANY, 0, ANY)


-- case: G = []
-- P[A,B]() := Q[A,B]() |[]| Q[A,B]()
-- Q[A,B]() := A?x >-> B!x >-> STOP
-- with ProcInst := P[A,B]()
-- becomes after step combination
-- P[A,B](op1$pc$Q, op1$x, op2$pc$Q, op2$x) :=
--        // only op1
--        A?A1 [op1$pc$Q == 0]              >-> P[A,B](1, A1, op2$pc$Q, op2$x)
--        B?B1 [op1$pc$Q == 1, B1 == op1$x] >-> P[A,B](-1, ANY, op2$pc$Q, op2$x)
--                                // note: the right side is still allowed to continue! that's intended behaviour.
--        // only op2
--        A?A1 [op2$pc$Q == 0]              >-> P[A,B](op1$pc$Q, op1$x, 1, A1)
--        B?B1 [op2$pc$Q == 1, B1 == op2$x] >-> P[A,B](op1$pc$Q, op1$x, -1, ANY)
--        // both op1 and op2
--        // 1,2 : only if |[]|
--        A?A1 | B?B1 [op1$pc$Q == 0, op2$pc$Q == 1, B1 == op2$x] >-> P[A,B](1, A1, -1, ANY)
--        // 2,1 : only if |[]|
--        B?B1 | A?A1 [op1$pc$Q == 1, op2$pc$Q == 0, B1 == op1$x] >-> P[A,B](-1, ANY, 1, A1)
--  with ProcInst = P[A,B](0, ANY, 0, ANY)

-- case: G = [A]
-- P[A,B]() := Q[A,B]() |[A]| Q[A,B]()
-- Q[A,B]() := A?x >-> B!x >-> STOP
-- with ProcInst := P[A,B]()
-- becomes after step combination
-- P[A,B](op1$pc$Q, op1$x, op2$pc$Q, op2$x) :=
--        // only op1
--        B?B1 [op1$pc$Q == 1, B1 == op1$x] >-> P[A,B](-1, ANY, op2$pc$Q, op2$x)
--                                // note: the right side is still allowed to continue! that's intended behaviour.
--        // only op2
--        B?B1 [op2$pc$Q == 1, B1 == op2$x] >-> P[A,B](op1$pc$Q, op1$x, -1, ANY)
--        // both op1 and op2
--        // 1,1 : only if A \in G, but G could be more
--        A?A1 [op1$pc$Q == 0, op2$pc$Q == 0] >-> P[A,B](1, A1, 1, A1)
--  with ProcInst = P[A,B](0, ANY, 0, ANY)


-- case: G = [B]
-- P[A,B]() := Q[A,B]() |[B]| Q[A,B]()
-- Q[A,B]() := A?x >-> B!x >-> STOP
-- with ProcInst := P[A,B]()
-- becomes after step combination
-- P[A,B](op1$pc$Q, op1$x, op2$pc$Q, op2$x) :=
--        // only op1
--        A?A1 [op1$pc$Q == 0]              >-> P[A,B](1, A1, op2$pc$Q, op2$x)
--        // only op2
--        A?A1 [op2$pc$Q == 0]              >-> P[A,B](op1$pc$Q, op1$x, 1, A1)
--        // both op1 and op2
--        // 2,2 : only if B \in G, but G could be more...
--        B?B1 [op1$pc$Q == 1, op2$pc$Q == 1, B1 == op1$x, B1 == op2$x] >-> P[A,B](-1, ANY, -1, ANY)
--  with ProcInst = P[A,B](0, ANY, 0, ANY)

-- case: G = [A,B]
-- P[A,B]() := Q[A,B]() |[A,B]| Q[A,B]()
-- Q[A,B]() := A?x >-> B!x >-> STOP
-- with ProcInst := P[A,B]()
-- becomes after step combination
-- P[A,B](op1$pc$Q, op1$x, op2$pc$Q, op2$x) :=
--        // both op1 and op2
--        // 1,1 : only if A \in G, but G could be more
--        A?A1 [op1$pc$Q == 0, op2$pc$Q == 0] >-> P[A,B](1, A1, 1, A1)
--        // 2,2 : only if B \in G, but G could be more...
--        B?B1 [op1$pc$Q == 1, op2$pc$Q == 1, B1 == op1$x, B1 == op2$x] >-> P[A,B](-1, ANY, -1, ANY)
--  with ProcInst = P[A,B](0, ANY, 0, ANY)









-- Q[A]() |[]| Q[A]()           , Q[A]() := A >-> STOP
-- Q[A]() |[]| A >-> STOP       , ...

-- Q[A]() |[]| R[A]()           , Q[A]() := A >-> STOP
--                              , R[A]() := A >-> STOP




-- P'[A](1) |[]| P'[A](2)











----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testLPEParList :: Test
testLPEParList = TestList [ --  TestLabel "STOP becomes empty choice" testStop

                        ]
