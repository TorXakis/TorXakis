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
import           Data.Maybe

import TxsDefs
import qualified Data.Text         as T
import VarId
import Constant
import ValExpr

import LPEfunc

import TestDefinitions

-- import TxsShow
-- import Debug.Trace



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
                        (procInst procIdPlpe [chanIdA] [intMin1]))

      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                , (procIdQ, procDefQ)]
      procInst' = procInst procIdPlpe [chanIdA] [int0]


-- STOP becomes empty choice
-- P[]() := STOP
-- with procInst = P[]()
-- becomes
-- P[](pc$P) :=       // technically: choice Set.empty
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
                        (procInst procIdPlpe [chanIdA] [intMin1]))

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
                        (procInst procIdPlpe [chanIdA] [intMin1]))

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
                        (procInst procIdPlpe [chanIdA] [int0]))

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
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (choice $ Set.fromList [ actionPref actOfferA1P0 (procInst procIdPlpe [chanIdA] [intMin1])
                                                                        , actionPref actOfferA1P0 (procInst procIdPlpe [chanIdA] [int0])
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
                            (procInst procIdPlpe [chanIdA] [int1])
                      , actionPref
                            -- action: A?A1 [pc$P == 1]
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int1
                                     }
                            (procInst procIdPlpe [chanIdA] [intMin1])
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
                            (procInst procIdPlpe [chanIdA] [int1])
                      , actionPref
                            -- action: A?A1 [pc$P == 1]
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int1
                                     }
                            (procInst procIdPlpe [chanIdA] [int0])
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
                            (procInst procIdPlpe [chanIdA] [int1])
                      , actionPref
                            -- action: A?A1 [pc$P == 0]
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int0
                                     }
                            (procInst procIdPlpe [chanIdA] [int2])
                      , actionPref
                            -- action: A?A1 [pc$P == 2]
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int2
                                     }
                            (procInst procIdPlpe [chanIdA] [int0])
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
     assertBool "ProcDef identity"  (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst''  procDefs'))
     where
      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA,chanIdB] []
      procIdQ = procIdGen "Q" [chanIdA] [varIdX]

      procDefP = ProcDef [chanIdA,chanIdB] [] (choice $ Set.fromList [ actionPref actOfferAx (procInst procIdQ [chanIdA] [vexprX])
                                                      , actionPref actOfferAx (procInst procIdQ [chanIdB] [vexprX])
                                                      ])
      procDefQ = ProcDef [chanIdA] [varIdX] (actionPref actOfferAExclamX stop)

      procIdPlpe = procIdGen "LPE_P" [chanIdA, chanIdB] [varIdPcP, varIdQAx, varIdQBX]
      varIdQAx = VarId (T.pack "Q$A$x") 333 intSort
      varIdQBX = VarId (T.pack "Q$B$x") 334 intSort
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
                            (procInst procIdPlpe [chanIdA, chanIdB] [int1, vexprA1, vexprQBx])
                      , -- A?A1 [pc$P == 0] >-> LPE_P(2,Q$A$x, A1)
                        actionPref
                            ActOffer {  offers = Set.singleton
                                                      Offer { chanid = chanIdA
                                                            , chanoffers = [Quest varIdA1]
                                                      }
                                     , hiddenvars = Set.empty
                                     , constraint = cstrEqual vexprPcP int0
                                     }
                            (procInst procIdPlpe [chanIdA, chanIdB] [int2, vexprQAx, vexprA1])
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
                            (procInst procIdPlpe [chanIdA, chanIdB] [intMin1, anyInt, anyInt])
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
                            (procInst procIdPlpe [chanIdA, chanIdB] [intMin1, anyInt, anyInt])
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
      varIdQAx = VarId (T.pack "Q$A$x") 433 intSort
      varIdRAx = VarId (T.pack "R$A$x") 434 intSort
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
                            (procInst procIdPlpe [chanIdA] [int1, vexprA1, vexprRAx])
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
                            (procInst procIdPlpe [chanIdA] [int2, vexprQAx, vexprQAx])
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
                              (procInst procIdPlpe [chanIdA] [intMin1, anyInt, anyInt])
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
                            (procInst procIdPlpe [chanIdA, chanIdB] [int1])
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
                            (procInst procIdPlpe [chanIdA, chanIdB] [int0])
                    ])

      procDefs' = Map.fromList  [  (procIdP, procDefP)]
      procInst' = procInst procIdPlpe [chanIdA, chanIdB] [int0]



-- more complete example including necessity for GNF translation
-- P[A,B](s) := A?x >-> B!s >-> STOP
-- becomes with GNF:
-- P[A,B](s) := A?x >-> P$gnf1[A,B](s,x)
-- P$gnf1[A,B](P$gnf1$s,P$gnf1$x) := B!s >-> STOP
-- with procInst = P[A,B](1)
-- becomes
-- LPE_P[A,B](pc$P, P$A$B$s, P$gnf1$A$B$P$gnf1$s, P$gnf1$A$B$P$gnf1$x) :=
--       A?A1 [pc$P == 0] >-> LPE_P[A,B](1, P$A$B$s, P$A$B$s, A1)
--    ## B?B1 [pc$P == 1, B1 ==  P$gnf1$A$B$P$gnf1$s] >-> LPE_P[A,B](-1, ANY, ANY, ANY)
-- with procInst = LPE_P[A,B](0,1,ANY,ANY)
testMultiAction :: Test
testMultiAction = TestCase $
   assertBool "multi action" (eqProcDef (Just (procInst', procDefPlpe)) (lpeTransformFunc procInst'' procDefs'))
   where
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
      varIdPABs = VarId (T.pack "P$A$B$s") 633 intSort
      varIdPgnf1ABs = VarId (T.pack "P$gnf1$A$B$P$gnf1$s") 634 intSort
      varIdPgnf1ABx = VarId (T.pack "P$gnf1$A$B$P$gnf1$x") 635 intSort

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
                            (procInst procIdPlpe [chanIdA, chanIdB] [int1, vexprPABs, vexprPABs, vexprA1])
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
                            (procInst procIdPlpe [chanIdA, chanIdB] [intMin1, anyInt, anyInt, anyInt])
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
                        (procInst procIdPlpe [chanIdA] [intMin1]))

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
-- P$pre1[A](P$pre1$x) := A!P$pre1$x >-> STOP || A!P$pre1$x >-> STOP
-- then parallel translation:
-- P$pre1[A](op1$pc$P$pre1$op1, op1$P$pre1$P$pre1$x, op2$pc$P$pre1$op2, op2$P$pre1$P$pre1$x) :=
--   A?A1 [op1$pc$P$pre1$op1 == 0, op2$pc$P$pre1$op2 == 0,
--         A1 == op1$P$pre1$P$pre1$x, A1 == op2$P$pre1$P$pre1$x] >->  P$pre1[A](-1, ANY, -1, ANY)
--  with P$pre1[A](0, x, 0, x) // which is put into P[A]() !!!

-- LPE of P becomes:
-- P[A](pc$P, P$pre1$A$op1$pc$P$pre1$op1, P$pre1$A$op1$P$pre1$op1$A$P$pre1$x, P$pre1$A$op2$pc$P$pre1$op2, P$pre1$A$op2$P$pre1$op2$A$P$pre1$x) :=
--    A?A1 [pc$P == 0] >-> P[A](1, A1, 0, A1, 0)
-- ## A?A1 [pc$P == 1,
--          P$pre1$A$op1$pc$P$pre1$op1 == 0, P$pre1$A$op2$pc$P$pre1$op2 == 0,
--          A1 == P$pre1$A$op1$P$pre1$P$pre1$x, A1 == P$pre1$A$op2$P$pre1$P$pre1$x] >->  P$pre1[A](1, -1, ANY, -1, ANY)
--    // note that pc$P is set to 1, but the pc=1 for each operand makes sure, the STOP is translated correctly
-- resulting procInst is put in the original place in preGNF!!
-- with procInst: P[A](0, ANY, ANY, ANY, ANY)

-- description of the param names:
    --  P$pre1$A$ op1$  P$pre1$op1$A$P$pre1$x
    --                  name after LPE of left operator: temporary ProcId = P$pre1$op1!
    --                  full process name + var name
    --            made unique during parallel translation: prefix of operator nr
    --  name after LPE of temporary procDef P$pre1$A

testLPEPar :: Test
testLPEPar = TestCase $
   assertBool "test LPEPar integration" 
      --    $ trace ("\n\n expected: "  ++ pshow (procInst', DefProc procDefP') ++ "\n\ngot: " ++ pshow (res1, DefProc res2)) $ 
      $ eqProcDef (Just (procInst', procDefP')) res
   where
      -- (Just (res1, res2)) = res
      res = lpeTransformFunc procInst'' procDefs'
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
      varIdpcP = VarId (T.pack "pc$P") 700 intSort
      varIdOp1pc = VarId (T.pack "P$pre1$A$op1$pc$P$pre1$op1") 701 intSort

      varIdOp1x = VarId (T.pack "P$pre1$A$op1$P$pre1$op1$A$x") 733 intSort


      varIdOp2pc = VarId (T.pack "P$pre1$A$op2$pc$P$pre1$op2") 800 intSort
      varIdOp2x = VarId (T.pack "P$pre1$A$op2$P$pre1$op2$A$x") 833 intSort

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
                          (procInst procIdP' [chanIdA] [int1, intMin1, anyInt, intMin1, anyInt])])

      procInst' = procInst procIdP' [chanIdA] [int0, anyInt, anyInt, anyInt, anyInt]




-- -------------------------------------------------
-- test 
-- -------------------------------------------------


-- chanIdA3 = ChanId    { ChanId.name = T.pack "A"
--                     , ChanId.unid = 2
--                     , ChanId.chansorts = [intSort, intSort, intSort]
--                     }

-- -- action: A?x?y?z
-- actOfferA3   = ActOffer {  offers = Set.singleton
--                                         Offer { chanid = chanIdA3
--                                               , chanoffers = [Quest varIdX,
--                                                                   Quest varIdY,
--                                                                   Quest varIdZ]
--                                         }
--                         , constraint = cstrConst (Cbool True)
--             }
            
            
-- testMultiChanOffer :: Test
-- testMultiChanOffer = TestCase $
--       assertBool "multi chan offer" (eqProcDef (Just (procInst, procDefP)) (lpeTransformFunc procInst'' procDefs))
--       where
--       varIdS = VarId (T.pack "s") 866 intSort
--       vexprS = cstrVar varIdS
--       procInst'' = procInst procIdP [chanIdA3, chanIdB] [int1]
--       procIdP = procIdGen "P" [chanIdA3, chanIdB] [varIdS]

--       procDefP = ProcDef [chanIdA3, chanIdB] [varIdS] (actionPref actOfferA3
--                                                 (actionPref
--                                                       ActOffer {  offers = Set.fromList [
--                                                                                     Offer { chanid = chanIdA3
--                                                                                           , chanoffers = [Exclam int1,
--                                                                                                             Exclam int2,
--                                                                                                             Exclam int2]
--                                                                                     },
--                                                                                     Offer { chanid = chanIdB
--                                                                                           , chanoffers = [Exclam vexprS]
--                                                                                     }
--                                                                               ]     
--                                                       , constraint = cstrConst (Cbool True)
--                                                       }
--                                                       stop
--                                                 ))


--       -- procIdPlpe = procIdGen "LPE_P" [chanIdA, chanIdB] [varIdPcP, varIdPABs, varIdPgnf1ABs, varIdPgnf1ABx]
--       -- varIdPABs = VarId (T.pack "P$A$B$s") 933 intSort
--       -- varIdPgnf1ABs = VarId (T.pack "P$gnf1$A$B$s") 934 intSort
--       -- varIdPgnf1ABx = VarId (T.pack "P$gnf1$A$B$x") 935 intSort

--       -- vexprPABs = cstrVar varIdPABs
--       -- vexprPgnf1ABs = cstrVar varIdPgnf1ABs
--       -- vexprPgnf1ABx = cstrVar varIdPgnf1ABx



--       procDefs = Map.fromList  [  (procIdP, procDefP)]
--       -- procInst' = procInst procIdPlpe [chanIdA, chanIdB] [int0, int1, anyInt, anyInt]




-- -------------------------------------------------
-- LPEHide integration (it's called at preGNF level)
-- -------------------------------------------------


-- P[A]() := HIDE [] IN A -> STOP NI 
-- with procInst = P[A]()
-- LPE:
--    first GNF 
--          first preGNF
--                preGNF of HIDE hiddenChans bexpr involves generating the LPE of bexpr first:
                  --    LPE of P$pre1[A]() ::= A -> STOP 
                  --    becomes
                  --           P$pre1[A](pc$P$pre1) := A [pc$P$pre1 == 0] -> P$pre1[A](-1)
--                then HIDE is applied: nothing happens in this case
--                returns: ProcInst: P$pre1[A](0) 
--                         ProcDef:  P$pre1[A](pc$P$pre1) := A [pc$P$pre1 == 0] -> P$pre1[A](-1)
--          then GNF is applied: to the bexpr of the ProcDef
--                no changes
--    then LPE of the whole: P[A]() ::= A [pc$P$pre1 == 0] -> P$pre1[A](-1)
--          one extra call to another ProcDef: P$pre1
--                thus pc$P == 0 is for A [pc$P$pre1 == 0] -> P$pre1[A](-1)
--                and  pc$P == 1 is for the instantiation of P$pre1[A](-1) 
--                      which is the same again: A [pc$P$pre1 == 0] -> P$pre1[A](-1)
--    thus the whole becomes:
-- becomes
-- LPE_P[A](pc$P, P$pre1$A$pc$P$pre1) :=        A [pc$P == 0] -> LPE_P[A](1, -1)
--                                        ##    A [pc$P == 1, P$pre1$A$pc$P$pre1 == 0] -> LPE_P[A](1,-1)
--                                                    pc$P remains instantiated with 1 because it's just 
--                                                    the translation of the call to P$pre1[A](-1) again
--                                                    and that was mapped to LPE_P[A](1,-1): never executed
-- with procInst = P[A](0)
testLPEHide1 :: Test
testLPEHide1 = TestCase $
   assertBool "testLPEHide1" (eqProcDef (Just (procInst', procDefPlpe)) res)
   where
      res = lpeTransformFunc procInst'' procDefs'
      procInst'' = procInst procIdP [chanIdA0] []
      procIdP = procIdGen "P" [chanIdA0] []
      procDefP = ProcDef [chanIdA0] [] (hide Set.empty (actionPref actOfferA stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      varIdPcPpre1 :: VarId
      varIdPcPpre1 = VarId (T.pack "P$pre1$A$pc$P$pre1") 1000 intSort
      vexprPcPpre1 :: VExpr
      vexprPcPpre1 = cstrVar varIdPcPpre1

      procIdPlpe = procIdGen "LPE_P" [chanIdA0] [varIdPcP, varIdPcPpre1]
      procDefPlpe = ProcDef [chanIdA0] [varIdPcP, varIdPcPpre1] (choice (Set.fromList [
                                                      -- A [pc$P == 0] -> LPE_P[A](1, -1)
                                                      actionPref                         
                                                            
                                                            ActOffer {  offers = Set.singleton
                                                                                    Offer { chanid = chanIdA0
                                                                                          , chanoffers = []
                                                                                    }
                                                                        , hiddenvars = Set.empty
                                                                        , constraint = cstrEqual vexprPcP int0
                                                                        } 
                                                            (procInst procIdPlpe [chanIdA0] [int1, intMin1])

                                                      -- ##    A [pc$P == 1, P$pre1$A$pc$P$pre1 == 0] -> LPE_P[A](1,-1)
                                                      , actionPref                         
                                                            ActOffer {  offers = Set.singleton
                                                                                    Offer { chanid = chanIdA0
                                                                                          , chanoffers = []
                                                                                    }
                                                                        , hiddenvars = Set.empty
                                                                        , constraint = cstrITE (cstrEqual vexprPcP int1)
                                                                                                (cstrEqual vexprPcPpre1 int0)
                                                                                                (cstrConst (Cbool False))
                                                                  } 
                                                            (procInst procIdPlpe [chanIdA0] [int1, intMin1])

                                                      ]))
      procInst' = procInst procIdPlpe [chanIdA0] [int0, anyInt]


-- check for correct var substitution 
-- 
-- P[A]() := HIDE [A] IN A?x [x == 0]-> Q(x) NI 
-- Q[]() := STOP
-- with procInst = P[A]()
-- LPE:
--    first GNF 
--          first preGNF
--                preGNF of HIDE hiddenChans bexpr involves generating the LPE of bexpr first:
                  --    LPE of P$pre1[A]() ::= A?x [x == 0]-> Q(x)
                  --    becomes
                  --           P$pre1[A](pc$P$pre1, P$pre$A$x) := A?A$1 [pc$P$pre1 == 0, A$1 == 0] -> P$pre1[A](1, A$1)
--                then HIDE is applied
--                returns: ProcInst: P$pre1[A](0, ANY) 
--                         ProcDef:  P$pre1[A](pc$P$pre1, P$pre$A$x) := {} [pc$P$pre1 == 0, A$1_1 == 0] {A$1_1} -> P$pre1[A](1, A$1_1)
--                preGNF returns only ProcDefs:
--                     P[A]() := P$pre1[A](0, ANY) 
--                     P$pre1[A](pc$P$pre1, P$pre$A$x) := {} [pc$P$pre1 == 0, A$1_1 == 0] {A$1_1} -> P$pre1[A](1, A$1_1)
--          then GNF
--                the bexpr P$pre1[A](0, ANY) of the ProcDef P[A]() is instantiated:
--                      P[A]() := {} [A$1_1 == 0] {A$1_1} -> P$pre1[A](1, A$1_1)
--                      note that the constraint pc$P$pre1 == 0 disappeared after substitution
--                GNF returns only ProcDefs:
--                      P[A]() := {} [pc$P$pre1 == 0, A$1_1 == 0] {A$1_1} -> P$pre1[A](1, A$1_1)
--                      P$pre1[A](pc$P$pre1, P$pre$A$x) := {} [pc$P$pre1 == 0, A$1_1 == 0] {A$1_1} -> P$pre1[A](1, A$1_1)
--    then LPE of P[A]()
--          P[A]() ::= {} [A$1_1 == 0] {A$1_1} -> P$pre1[A](1, A$1_1)
--          one extra call to another ProcDef: P$pre1
--                thus pc$P == 0 is for {} [A$1_1 == 0] {A$1_1} -> P$pre1[A](1, A$1_1)
--                and  pc$P == 1 is for the call to P$pre1[A](1, A$1_1)
--                      {} [pc$P$pre1 == 0, A$1_1 == 0] {A$1_1} -> P$pre1[A](1, A$1_1)
--                      note that the ProcDef of P$pre1 is taken "as is"
--                            when evaluating, the instantiation in the first step (P$pre1[A](1, A$1_1))
--                            makes sure that the constraint is never true. (pc$P$pre1 = 1 )
--                      thus we get the second step as unnecessary extra in this case
--                            but P$pre1 could have had more than one follow-up step, 
--                            then the inclusion of P$pre1 would have been useful/necessary
--                                  e.g.  P$pre1[A](pc$P$pre1, P$pre$A$x) := 
--                                              {} [A$1_1 == 0] {A$1_1} -> P$pre1[A](1, A$1_1)
--                                              B  [pc$P$pre1 == 1,]                    -> ...
--                            THUS: we get an unnecessary step in case the generated ProcDef P$Pre1 only
--                                  has ONE step
--                                  optimisation in general not possible, but post-processing possible
--    thus the whole becomes:
--
-- LPE_P[A](pc$P, P$pre1$A$pc$P$pre1, P$pre1$A$Q$A$x) :=
--          {} [pc$P == 0, A$1_1 == 0] {A$1_1} -> LPE_P[A](1, 1, A$1_1)
--    ##    {} [pc$P == 1, P$pre1$A$pc$P$pre1 == 0, A$1_1 == 0] {A$1_1} -> LPE_P[A](1, 1, A$1_1)
-- with procInst = P[A](0, ANY, ANY)
testLPEHide2 :: Test
testLPEHide2 = TestCase $
   assertBool "testLPEHide1" (eqProcDef (Just (procInst', procDefPlpe)) res)
   where
      res = lpeTransformFunc procInst'' procDefs'
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdA] []
                                    -- HIDE [A] IN A?x [x == 0]-> Q(x) NI 
      procDefP = ProcDef [chanIdA] [] (hide (Set.singleton chanIdA) (actionPref 
                                                      actOfferAx {constraint = cstrEqual vexprX int0} 
                                                      (procInst procIdQ [chanIdA] [vexprX])))
      procDefQ = ProcDef [chanIdA] [varIdX] stop
      procDefs' = Map.fromList  [  (procIdP, procDefP)
                                ,  (procIdQ, procDefQ)]


      varIdPcPpre1 = VarId (T.pack "P$pre1$A$pc$P$pre1") 1100 intSort
      vexprPcPpre1 = cstrVar varIdPcPpre1
      varIdPpre1QAx = VarId (T.pack "P$pre1$A$Q$A$x") 1200 intSort
      -- vexprPpre1QAx = cstrVar varIdPpre1QAx
      varIdA1' = VarId (T.pack "A$1_5") 1234 intSort
      vexprA1' = cstrVar varIdA1'

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP, varIdPcPpre1, varIdPpre1QAx]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP, varIdPcPpre1, varIdPpre1QAx] (choice (Set.fromList [
                                                      -- {} [pc$P == 0, A$1_1 == 0] {A$1_1} -> LPE_P[A](1, 1, A$1_1)
                                                            actionPref                         
                                                                  ActOffer {  offers = Set.empty
                                                                              , hiddenvars = Set.fromList [varIdA1']
                                                                              , constraint = cstrITE (cstrEqual vexprPcP int0)
                                                                                                      (cstrEqual vexprA1' int0)
                                                                                                      (cstrConst (Cbool False))
                                                                              } 
                                                                  (procInst procIdPlpe [chanIdA] [int1, int1, vexprA1'])
                                                      , -- {} [pc$P == 1, P$pre1$A$pc$P$pre1 == 0, A$1_1 == 0] {A$1_1} -> LPE_P[A](1, 1, A$1_1)
                                                            actionPref                         
                                                            ActOffer {  offers = Set.empty
                                                                        , hiddenvars = Set.fromList [varIdA1']
                                                                        , constraint = cstrITE (cstrEqual vexprPcP int1)
                                                                                                ( cstrITE (cstrEqual vexprPcPpre1 int0)
                                                                                                      (cstrEqual vexprA1' int0)
                                                                                                      (cstrConst (Cbool False))      
                                                                                                      )
                                                                                                (cstrConst (Cbool False))
                                                                        } 
                                                            (procInst procIdPlpe [chanIdA] [int1, int1, vexprA1'])
                                                      ]))
      procInst' = procInst procIdPlpe [chanIdA] [int0, anyInt, anyInt]




-- -------------------------------------------------------
-- preGNFEnable integration (it's called at preGNF level)
-- -------------------------------------------------------

-- P[A]() := EXIT >>> STOP
-- with procInst = P[A]()
-- becomes:
-- LPE:
--    first GNF 
--          first preGNF
--                preGNFBExpr for Enable creates a new ProcDef/ProcInst of the whole bexpr
--                      ProcDef:    P$pre1[A]() := EXIT >>> STOP
--                      ProcInst:   P$pre1[A]()
--                      thus the bexpr of the original ProcDef P is replaced with the call to the new ProcInst:
--                      ProcDef:    P[A]() := P$pre1[A]()
--                then preGNFEnable is applied
                        -- first LPE of left side of ENABLE:
                        --       P$pre1[A](pc$P) := EXIT [pc$P == 0] >-> STOP 
                        --       with procInst = P$pre1[A](0)
                        -- make a new ProcDef P$enable of the right hand side of ENABLE operator
                        --       P$pre1$enable[A]() := STOP
                        -- then replace each occurrence of EXIT >-> BExpr with an empty ActionPrefix and a ProcInst to P$enable
                        --       P$pre1[A](pc$P) := {} [pc$P == 0] >-> P$pre1$enable[A]()           
                        -- then run preGNF on P$pre1$enable
                        --       extension of scope of variables from the lhs to rhs?
                        --       just by explicit communication via EXIT/ACCEPT
                        -- returns
                        --       ProcDef:  P$pre1[A](pc$P) := {} [pc$P == 0] >-> P$pre1$enable[A]()
                        --       ProcInst: P$pre1[A](0)                // just the procInst of the LPE of lhs
--                now the originally generated ProcInst is replaced with the new one
--                     P[A]() := P$pre1[A](0)
--    GNF cont'd:
--          unfold the ProcDef P[A](), i.e. instantiate P$pre1[A](0)
--          P[A]() := P$pre1[A](0)
--          becomes:
--          P[A]() := {} [pc$P == 0] >-> P$pre1$enable[A]()
--          P$pre1$enable[A]() := STOP
-- LPE cont'd:
--    becomes: 
--    LPE_P[A](pcP) := {} [pcP == 0] >-> P[A](1)
--    LPE_P[A](0)

testEnable1 :: Test
testEnable1 = TestCase $
   --    trace ("\n\n expected:" ++ show (Just (procInst', procDefPlpe)) ++ "\ngot: " ++ show res) $ 
   assertBool "simple EXIT" (eqProcDef (Just (procInst', procDefPlpe)) res)
   where
      res = lpeTransformFunc procInst'' procDefs'
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      
      procDefP = ProcDef [chanIdA] [] (enable (actionPref actOfferExit stop)
                                              [] 
                                              stop)
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (actionPref 
                                                      ActOffer {    offers = Set.empty
                                                                  , hiddenvars = Set.empty
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (procInst procIdPlpe [chanIdA] [int1]))
      procInst' = procInst procIdPlpe [chanIdA] [int0]


-- P[A]() := EXIT >>> P[A]()
-- with procInst = P[A]()
-- becomes:
-- LPE:
--    LPE_P[A]() :=     {} [pcP == 0] >-> P[A](0)
--    LPE_P[A](0)

testEnable1Rec :: Test
testEnable1Rec = TestCase $
      -- trace ("\testEnable1Rec:\n expected:" ++  pshow (procInst', DefProc procDefPlpe)  ++ 
      --       "\ngot: " ++ pshow (res_procInst, DefProc res_procDef) ++ 
      --       "\n res_procDef: " ++ (pshow $ DefProc res_procDef)) $   
      assertBool "Recursion" (eqProcDef (Just (procInst', procDefPlpe)) (Just (res_procInst, res_procDef)))
   where
      (res_procInst, res_procDef) = fromMaybe (error "could not find the given procId 1") $ lpeTransformFunc procInst'' procDefs'
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      
      procDefP = ProcDef [chanIdA] [] (enable (actionPref actOfferExit stop)
                                              [] 
                                              procInst'')
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (choice (Set.fromList [
                                                            actionPref 
                                                                  ActOffer {    offers = Set.empty
                                                                              , hiddenvars = Set.empty
                                                                              , constraint = cstrEqual vexprPcP int0
                                                                              } 
                                                                  (procInst procIdPlpe [chanIdA] [int0])
                                                      ]))
      procInst' = procInst procIdPlpe [chanIdA] [int0]


-- P[A]() := EXIT >>> A >-> P[A]()
-- with procInst = P[A]()
-- becomes:
-- LPE:
--    LPE_P[A]() :=     {} [pcP == 0] >-> P[A](1)
--                  ##  A [pcP == 1] >-> P[A](0)  
--    LPE_P[A](0)

testEnable2Rec :: Test
testEnable2Rec = TestCase $
      -- trace ("\ntestEnable:\n expected:" ++  pshow (procInst', DefProc procDefPlpe)  ++ 
      --       "\ngot: " ++ pshow (res_procInst, DefProc res_procDef) ++ 
      --       "\n res_procDef: " ++ (pshow $ DefProc res_procDef)) $
            assertBool "ActionPref" $ eqProcDef (Just (procInst', procDefPlpe)) (Just (res_procInst, res_procDef))--((eqProcDef procDefExpected res_procDef)  && (procInst' ~~ res_procInst))
      where
      (res_procInst, res_procDef) = fromMaybe (error "could not find the given procId 1") $ lpeTransformFunc procInst'' procDefs'
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      
      procDefP = ProcDef [chanIdA] [] (enable (actionPref actOfferExit stop)
                                              [] 
                                              (actionPref actOfferA procInst''))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP] (choice (Set.fromList [
                                                            actionPref 
                                                                  ActOffer {    offers = Set.empty
                                                                              , hiddenvars = Set.empty
                                                                              , constraint = cstrEqual vexprPcP int0
                                                                              } 
                                                                  (procInst procIdPlpe [chanIdA] [int1]),
                                                            actionPref 
                                                                  actOfferA {  constraint = cstrEqual vexprPcP int1
                                                                              } 
                                                                  (procInst procIdPlpe [chanIdA] [int0])
                                                      ]))

      procInst' = procInst procIdPlpe [chanIdA] [int0]





-- P[A]() := A | EXIT !1 >>> ACCEPT ?id IN A!id >-> STOP NI
-- with procInst = P[A]()
-- 
-- becomes
-- LPE_P[A](pc$P, P$pre1$enable$A$id) :=
--                        A     [pc$P == 0, EXIT$1 == 1] {EXIT$1}               >-> LPE_P[A](1, EXIT$1)
--                    ##  A?A$1 [pc$P == 1, A$1 == P$pre1$enable$A$id] {}       >-> LPE_P[A](-1, ANY)
-- with procInst = LPE_P[A](0, ANY)
testEnable2 :: Test
testEnable2 = TestCase $
      -- trace ("\n\n expected:" ++ pshow procInstlpe ++ "\n" ++ (pshow $ DefProc procDefPlpe) ++ 
      --       "\n\ngot: " ++ pshow procInstRes ++ "\n" ++ (pshow $ DefProc procDefRes) ) $ 
      assertBool "simple EXIT" (eqProcDef (Just (procInstlpe, procDefPlpe)) (Just (procInstRes, procDefRes)))
      where
      (procInstRes, procDefRes) = fromMaybe (error "could not find the given procId 1") $ lpeTransformFunc procInst' procDefs'
      procInst' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      

      -- action: A | EXIT !1
      actOfferExit1 :: ActOffer
      actOfferExit1   = ActOffer {  offers = Set.fromList [
                                          Offer { chanid = chanIdA0
                                                , chanoffers = []
                                          },
                                          Offer { chanid = chanIdExit
                                                , chanoffers = [Exclam int1]
                                          }]
                              , hiddenvars = Set.empty
                              , constraint = cstrConst (Cbool True)
                  }

      varIdid = VarId (T.pack "id") 1333 intSort
      vexprid = cstrVar varIdid
      -- action: A!id
      actOfferAid :: ActOffer
      actOfferAid   = ActOffer {  offers = Set.singleton
                                          Offer { chanid = chanIdA
                                                , chanoffers = [Exclam vexprid]
                                    }
                  , hiddenvars = Set.empty
                  , constraint = cstrConst (Cbool True)
      }
    

      varIdExit1 = VarId (T.pack "EXIT$1") 122 intSort
      vexprExit1 = cstrVar varIdExit1

      varIdPenableId = VarId (T.pack "P$pre1$rhs$A$id") 123 intSort
      vexprPenableId = cstrVar varIdPenableId

      procDefP = ProcDef [chanIdA] [] (enable (actionPref actOfferExit1 stop)
                                                [Quest varIdid]
                                                (actionPref actOfferAid stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]



      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP, varIdPenableId]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP, varIdPenableId] 
                                                (choice (Set.fromList [
                                                            actionPref ActOffer {    offers = Set.singleton
                                                                                                Offer { chanid = chanIdA0
                                                                                                      , chanoffers = []
                                                                                                }
                                                                                    , hiddenvars = Set.fromList [varIdExit1]
                                                                                    , constraint = cstrITE (cstrEqual vexprPcP int0)
                                                                                                                  (cstrEqual vexprExit1 int1)
                                                                                                                  (cstrConst (Cbool False))}
                                                                                    
                                                            (procInst procIdPlpe [chanIdA] [int1, vexprExit1])
                                                            , actionPref actOfferA1 {     offers = Set.singleton
                                                                                                      Offer { chanid = chanIdA
                                                                                                            , chanoffers = [Quest varIdA1]
                                                                                                      }
                                                                                          , constraint = cstrITE (cstrEqual vexprPcP int1)
                                                                                                                  (cstrEqual vexprA1 vexprPenableId)
                                                                                                                  (cstrConst (Cbool False))}
                                                              (procInst procIdPlpe [chanIdA] [intMin1, anyInt])
                                                            


                                                ]))
                                                
      procInstlpe = procInst procIdPlpe [chanIdA] [int0, anyInt]







-- -------------------------------------------------------
-- LPE of guard
-- -------------------------------------------------------

-- P[A]() := [[x == 1]] =>> STOP
-- with procInst = P[A]()
-- 
-- becomes
-- LPE_P[A](pc$P) :=  [[x == 1]] =>> Choice []              -- ie. STOP
-- with procInst = LPE_P[A](0)

testLPEGuardStop :: Test
testLPEGuardStop = TestCase $
         --    trace ("\n\nexpected: " ++ show (procInstP', procDefP') ++ "\n\ngot: " ++ show res) $ 
         assertBool "test guard with parallel" $ eqProcDef (Just (procInstP', procDefP')) res
         where
            res = lpeTransformFunc procInstP procDefs'

            procInstP = procInst procIdP [chanIdA] []
            procIdP = procIdGen "P" [chanIdA] []
      
            procDefP = ProcDef [chanIdA] []  (guard   (cstrEqual vexprX int1 )
                                                      stop)
            procDefs' = Map.fromList  [  (procIdP, procDefP)]
      
            varIdpcP = VarId (T.pack "pc$P") 2000 intSort
            procIdP' = procIdGen "LPE_P" [chanIdA] [varIdpcP]
            procInstP' = procInst procIdP' [chanIdA] [int0]
            
            procDefP' = ProcDef [chanIdA] [varIdpcP]
                                    (guard   (cstrEqual vexprX int1 )
                                             (choice Set.empty))
                  

-- P[A]() := [[y == 1]] =>> (A?x >-> STOP)
-- with procInst = P[A]()
-- 
-- becomes
-- LPE_P[A](pc$P) :=  (A?A$1 [pc$P == 0, y == 1] >-> LPE_P[A](-1))             
--                     - note that the original expression was first translated to   (A?x [y == 1] >-> STOP) due to smart constructors
--                     - then the pc$P == 0 constraint gets added *before* that
-- with procInst = LPE_P[A](0)

testLPEGuardActionPref :: Test
testLPEGuardActionPref = TestCase $
         --    trace ("\n\nexpected: " ++ show (procInstP', procDefP') ++ "\n\ngot: " ++ show res) $ 
         assertBool "test guard with parallel" $ eqProcDef (Just (procInstP', procDefP')) res
         where
            res = lpeTransformFunc procInstP procDefs'

            procInstP = procInst procIdP [chanIdA] []
            procIdP = procIdGen "P" [chanIdA] []
      
            procDefP = ProcDef [chanIdA] []  (guard   (cstrEqual vexprY int1 )
                                                      (actionPref actOfferAx stop))
            procDefs' = Map.fromList  [  (procIdP, procDefP)]
      
            varIdpcP = VarId (T.pack "pc$P") 2100 intSort
            procIdP' = procIdGen "LPE_P" [chanIdA] [varIdpcP]
            procInstP' = procInst procIdP' [chanIdA] [int0]
            
            procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
            procDefP' = ProcDef [chanIdA] [varIdpcP]
                                                (actionPref actOfferA1 {     offers = Set.singleton
                                                                                          Offer { chanid = chanIdA
                                                                                                , chanoffers = [Quest varIdA1]
                                                                                          }
                                                                              , constraint = cstrITE (cstrEqual vexprPcP int0)
                                                                                                      (cstrEqual vexprY int1)
                                                                                                      (cstrConst (Cbool False))}
                                                            (procInst procIdPlpe [chanIdA] [intMin1]) )
            

-- P[A]() := [[y == 1]] =>> (A?x >-> STOP ## A?x >-> STOP )
-- with procInst = P[A]()
-- 
-- becomes
-- LPE_P[A](pc$P) :=  (A?A$1 [pc$P == 0, y == 1] >-> LPE_P[A](-1))                 
--                ##  (A?A$1 [pc$P == 0, y == 1] >-> LPE_P[A](-1))               
-- with procInst = LPE_P[A](0)

testLPEGuardChoice :: Test
testLPEGuardChoice = TestCase $
         --    trace ("\n\nexpected: " ++ show (procInstP', procDefP') ++ "\n\ngot: " ++ show res) $ 
         assertBool "test guard with parallel" $ eqProcDef (Just (procInstP', procDefP')) res
         where
            res = lpeTransformFunc procInstP procDefs'

            procInstP = procInst procIdP [chanIdA] []
            procIdP = procIdGen "P" [chanIdA] []
      
            procDefP = ProcDef [chanIdA] []  (guard   (cstrEqual vexprY int1 )
                                                      (choice $ Set.fromList [
                                                            actionPref actOfferAx stop,
                                                            actionPref actOfferBx stop
                                                            ]))
            procDefs' = Map.fromList  [  (procIdP, procDefP)]
      
            varIdpcP = VarId (T.pack "pc$P") 2200 intSort
            procIdP' = procIdGen "LPE_P" [chanIdA] [varIdpcP]
            procInstP' = procInst procIdP' [chanIdA] [int0]
            
            procDefP' = ProcDef [chanIdA] [varIdpcP]
                                    (choice $ Set.fromList [
                                                        actionPref actOfferA1 {      offers = Set.singleton
                                                                                          Offer { chanid = chanIdA
                                                                                                , chanoffers = [Quest varIdA1]
                                                                                          }
                                                                                    , constraint =  cstrITE (cstrEqual vexprPcP int0)
                                                                                                            (cstrEqual vexprY int1)
                                                                                                            (cstrConst (Cbool False))}
                                                                  (procInst procIdP' [chanIdA] [intMin1])
                                                     ,  actionPref actOfferA1 {      offers = Set.singleton
                                                                                                Offer { chanid = chanIdB
                                                                                                      , chanoffers = [Quest varIdB1]
                                                                                                }
                                                                                    , constraint =  cstrITE (cstrEqual vexprPcP int0)
                                                                                                            (cstrEqual vexprY int1)
                                                                                                            (cstrConst (Cbool False))}
                                                                  (procInst procIdP' [chanIdA] [intMin1])
                                                     ])
                                                      
                  

-- P[A]() := [[y == 1]] =>> Q[A]()
-- Q[A]() := A?x >-> STOP
-- with procInst = P[A]()
-- 
-- becomes
-- LPE_P[A](pc$P) :=  A?A$1 [pc$P == 0, y == 1]  >-> LPE_P[A](-1)        
-- with procInst = LPE_P[A](0)

testLPEGuardProcInst :: Test
testLPEGuardProcInst = TestCase $
         --    trace ("\n\nexpected: " ++ show (procInstP', procDefP') ++ "\n\ngot: " ++ show res) $ 
         assertBool "test guard with parallel" $ eqProcDef (Just (procInstP', procDefP')) res
         where
            res = lpeTransformFunc procInstP procDefs'

            procInstP = procInst procIdP [chanIdA] []
            procIdP = procIdGen "P" [chanIdA] []
            procIdQ = procIdGen "Q" [chanIdA] []

            procDefP = ProcDef [chanIdA] []  (guard   (cstrEqual vexprY int1 )
                                                      (procInst procIdQ [chanIdA] []))
            procDefQ = ProcDef [chanIdA] []  (actionPref actOfferAx stop)
            procDefs' = Map.fromList  [  (procIdP, procDefP)
                                       , (procIdQ, procDefQ)]
      
            varIdpcP = VarId (T.pack "pc$P") 2300 intSort
            procIdP' = procIdGen "LPE_P" [chanIdA] [varIdpcP]
            procInstP' = procInst procIdP' [chanIdA] [int0]
            
            procDefP' = ProcDef [chanIdA] [varIdpcP] (
                                                 actionPref actOfferA1 {  offers = Set.singleton
                                                                                    Offer { chanid = chanIdA
                                                                                          , chanoffers = [Quest varIdA1]
                                                                                    }
                                                                        , constraint = cstrITE (cstrEqual vexprPcP int0)
                                                                                                (cstrEqual vexprY int1)
                                                                                                (cstrConst (Cbool False))}
                                                      (procInst procIdP' [chanIdA] [intMin1])  )
                  




-- P[A]() := [[z == 1]] =>> (A?x >-> STOP |[A]| A?y >-> STOP)
-- with procInst = P[A]()
-- 
-- becomes
-- LPE_P[A](pc$P, op1$pc$P$op1, op2$pc$P$op2) :=
--             A?A$1 [pc$P == 0, z == 1]                                >->  P[A](1, -1, -1)
--          ## A?A$1 [pc$P == 1, op1$pc$P$op1 == 0, op2$pc$P$op2 == 0]  >->  P[A](1, -1, -1)         -- this branch is never reachable!
--                                                                                                   -- exists only due to unfolding during GNF translation
-- with ProcInst := P[A](0,0,0)
testLPEGuardPar :: Test
testLPEGuardPar = TestCase $
         --    trace ("\n\nexpected: " ++ show (procInstP', procDefP') ++ "\n\ngot: " ++ show res) $ 
         assertBool "test guard with parallel" $ eqProcDef (Just (procInstP', procDefP')) res
         where
            res = lpeTransformFunc procInstP procDefs'

            procInstP = procInst procIdP [chanIdA] []
            procIdP = procIdGen "P" [chanIdA] []
      
            procDefP = ProcDef [chanIdA] []  (guard   (cstrEqual vexprZ int1 )
                                                      (parallel (Set.singleton chanIdA) [
                                                            actionPref actOfferAx stop,
                                                            actionPref actOfferAy stop
                                                          ])) 
            procDefs' = Map.fromList  [  (procIdP, procDefP)]
      

            varIdOp1pcPop1 = VarId (T.pack "P$pre1$A$op1$pc$P$pre1$op1") 2400 intSort
            varIdOp2pcPop2 = VarId (T.pack "P$pre1$A$op2$pc$P$pre1$op2") 2500 intSort
            vexprOp1pcPop1 = cstrVar varIdOp1pcPop1
            vexprOp2pcPop2 = cstrVar varIdOp2pcPop2
            varIdpcP = VarId (T.pack "pc$P") 2600 intSort
            procIdP' = procIdGen "LPE_P" [chanIdA] [varIdpcP, varIdOp1pcPop1, varIdOp2pcPop2]
            procInstP' = procInst procIdP' [chanIdA] [int0, anyInt, anyInt]
            
            procDefP' = ProcDef [chanIdA] [varIdpcP, varIdOp1pcPop1, varIdOp2pcPop2] (
                                          choice $ Set.fromList[
                                                actionPref actOfferA1 {  offers = Set.singleton
                                                                                    Offer { chanid = chanIdA
                                                                                          , chanoffers = [Quest varIdA1]
                                                                                    }
                                                                        , constraint = cstrITE  (cstrEqual vexprPcP int0)
                                                                                                (cstrEqual vexprZ int1)
                                                                                                (cstrConst (Cbool False))}
                                                            (procInst procIdP' [chanIdA] [int1, intMin1, intMin1]) 
                                              , actionPref actOfferA1 {  offers = Set.singleton
                                                                                    Offer { chanid = chanIdA
                                                                                          , chanoffers = [Quest varIdA1]
                                                                                    }
                                                                        , constraint = cstrITE  (cstrEqual vexprPcP int1)
                                                                                                (cstrAnd (Set.fromList [ cstrEqual vexprOp1pcPop1 int0
                                                                                                                        , cstrEqual vexprOp2pcPop2 int0]))
                                                                                                (cstrConst (Cbool False))}
                                                            (procInst procIdP' [chanIdA] [int1, intMin1, intMin1]) 
                                              ])

-- same test as testLPEHide1 just with a guard added
--
-- P[A]() := [[x == 1]] =>> HIDE [] IN A -> STOP NI 
-- with procInst = P[A]()
-- becomes
-- LPE_P[A](pc$P, P$pre1$A$pc$P$pre1) :=        A [pc$P == 0, x == 1] -> LPE_P[A](1, -1)
--                                        ##    A [pc$P == 1, P$pre1$A$pc$P$pre1 == 0] -> LPE_P[A](1,-1)
-- with procInst = P[A](0)
testLPEGuardHide :: Test
testLPEGuardHide = TestCase $
      assertBool "test LPE Guard with HIDE" (eqProcDef (Just (procInst', procDefPlpe)) res)
      where
            res = lpeTransformFunc procInst'' procDefs'
            procInst'' = procInst procIdP [chanIdA0] []
            procIdP = procIdGen "P" [chanIdA0] []
            procDefP = ProcDef [chanIdA0] [] (guard   (cstrEqual vexprX int1)
                                                      (hide Set.empty (actionPref actOfferA stop)))
            procDefs' = Map.fromList  [  (procIdP, procDefP)]

            varIdPcPpre1 :: VarId
            varIdPcPpre1 = VarId (T.pack "P$pre1$A$pc$P$pre1") 2700 intSort
            vexprPcPpre1 :: VExpr
            vexprPcPpre1 = cstrVar varIdPcPpre1

            procIdPlpe = procIdGen "LPE_P" [chanIdA0] [varIdPcP, varIdPcPpre1]
            procDefPlpe = ProcDef [chanIdA0] [varIdPcP, varIdPcPpre1] 
                                                (choice $ Set.fromList [
                                                                  -- A [pc$P == 0] -> LPE_P[A](1, -1)
                                                                  actionPref                         
                                                                        ActOffer {  offers = Set.singleton
                                                                                                Offer { chanid = chanIdA0
                                                                                                      , chanoffers = []
                                                                                                }
                                                                                    , hiddenvars = Set.empty
                                                                                    , constraint = cstrITE (cstrEqual vexprPcP int0)
                                                                                                            (cstrEqual vexprX int1)
                                                                                                            (cstrConst (Cbool False))
                                                                                    } 
                                                                        (procInst procIdPlpe [chanIdA0] [int1, intMin1])
                                                      ,                                                                   -- ##    A [pc$P == 1, P$pre1$A$pc$P$pre1 == 0] -> LPE_P[A](1,-1)
                                                                  actionPref                         
                                                                        ActOffer {  offers = Set.singleton
                                                                                                Offer { chanid = chanIdA0
                                                                                                      , chanoffers = []
                                                                                                }
                                                                                    , hiddenvars = Set.empty
                                                                                    , constraint = cstrITE (cstrEqual vexprPcP int1)
                                                                                                            (cstrEqual vexprPcPpre1 int0)
                                                                                                            (cstrConst (Cbool False))
                                                                              } 
                                                                        (procInst procIdPlpe [chanIdA0] [int1, intMin1])
                                                                  ])
            procInst' = procInst procIdPlpe [chanIdA0] [int0, anyInt]


-- the same as testEnable1 but now with a guard 
-- P[A]() := [[x == 1]] ==> EXIT >>> STOP
-- with procInst = P[A]()
-- becomes:
--    LPE_P[A]() := EXIT [pcP == 0, x == 1] >-> P[A](1)
--    LPE_P[A](0)

testLPEGuardEnable :: Test
testLPEGuardEnable = TestCase $
   --    trace ("\n\n expected:" ++ show (Just (procInst', procDefPlpe)) ++ "\ngot: " ++ show res) $ 
   assertBool "guard with enable" (eqProcDef (Just (procInst', procDefPlpe)) res)
   where
      res = lpeTransformFunc procInst'' procDefs'
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procDefP = ProcDef [chanIdA] [] (guard    (cstrEqual vexprX int1)
                                                (enable (actionPref actOfferExit stop)
                                                            [] 
                                                            stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP]           (actionPref 
                                                                  ActOffer {    offers = Set.empty
                                                                              , hiddenvars = Set.empty
                                                                              , constraint = cstrITE (cstrEqual vexprPcP int0)
                                                                                                      (cstrEqual vexprX int1)
                                                                                                      (cstrConst (Cbool False))
                                                                              } 
                                                                  (procInst procIdPlpe [chanIdA] [int1]))
      procInst' = procInst procIdPlpe [chanIdA] [int0]



-- test LPE translation of DISABLE
-- P[A]() := EXIT >-> STOP [>> A >-> STOP
-- with procInst = P[A]()
-- becomes:
--                P[A](pc$P, P$pre1$A$P$pre1$disable$lhs, P$pre1$A$P$pre1$lhs$pc$P$pre1$lhs, P$pre1$A$P$pre1$rhs$pc$P$pre1$rhs) :=
--                      A [[ pc$P == 0 ]] >-> LPE_P[A](1,1,0,-1)
--                      A [[ pc$P == 1, P$pre1$A$P$pre1$rhs$pc$P$pre1$rhs == 0]] >-> LPE_P[A](1, 1, P$pre1$A$P$pre1$lhs$pc$P$pre1$lhs, -1 )
--                      EXIT [[ pc$P == 0 ]] >-> LPE_P[A](1,0,-1,-1)
--                      EXIT [[ pc$P == 0, P$pre1$A$P$pre1$disable$lhs == 0, P$pre1$A$P$pre1$lhs$pc$P$pre1$lhs == 0]] >-> LPE_P[A](1,0,-1,-1)

--                      EXIT [P$disable$lhs == 0, P$lhs$pc$P$lhs == 0] >-> P[A](0, -1, -1)                       -- NOTICE that RHS has been disabled by setting pc$P$rhs to -1! 
--                 ##   A [P$rhs$pc$P$rhs == 0]                        >-> P[A](1, P$lhs$pc$P$lhs, -1)   
--          with ProcInst: P[A](0, ANY, ANY, ANY)

testLPEDisable1 :: Test
testLPEDisable1 = TestCase $
--    trace ("\ntestDisable:\n expected:" ++  pshow (procInst', DefProc procDefExpected)  ++ 
--             "\ngot: " ++ pshow (res_procInst, DefProc res_procDef) ++ 
--             "\n res_procDef: " ++ (pshow $ DefProc res_procDef)) $
      assertBool "EXIT, ActionPref" $ eqProcDef (Just (procInst', procDefExpected)) (Just (res_procInst, res_procDef))--((eqProcDef procDefExpected res_procDef)  && (procInst' ~~ res_procInst))
   where
      (res_procInst, res_procDef) = fromMaybe (error "could not find the given procId 1") $ lpeTransformFunc procInst'' procDefs'
      -- extract expected ProcDef from all results:
      -- res_procDef = fromMaybe (error "could not find the given procId") (Map.lookup res_procId res_procDefs')

      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] [ ]
      procDefP = ProcDef [chanIdA] [] (disable (actionPref actOfferExit stop)
                                                (actionPref actOfferA stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      varIdPdisable' :: VarId
      varIdPdisable' = VarId (T.pack "P$pre1$A$P$pre1$disable$lhs") 2833 intSort
      varIdPpcLHS' :: VarId
      varIdPpcLHS' = VarId (T.pack "P$pre1$A$P$pre1$lhs$pc$P$pre1$lhs") 2834 intSort
      varIdPpcRHS' :: VarId
      varIdPpcRHS' = VarId (T.pack "P$pre1$A$P$pre1$rhs$pc$P$pre1$rhs") 2835 intSort
      
      vexprPdisable' :: VExpr
      vexprPdisable' = cstrVar varIdPdisable'
      vexprPpcLHS' :: VExpr
      vexprPpcLHS' = cstrVar varIdPpcLHS'
      vexprPpcRHS' :: VExpr
      vexprPpcRHS' = cstrVar varIdPpcRHS'


      procIdP' = procIdGen "LPE_P" [chanIdA] [varIdPcP, varIdPdisable', varIdPpcLHS', varIdPpcRHS']
      procInst' = procInst procIdP' [chanIdA] [int0, anyInt, anyInt, anyInt]
      procDefExpected = ProcDef [chanIdA] [varIdPcP, varIdPdisable', varIdPpcLHS', varIdPpcRHS']
                                                (choice $ Set.fromList [

                                                            actionPref 
                                                                  actOfferA { constraint = cstrEqual vexprPcP int0
                                                                              } 
                                                                  (procInst procIdP' [chanIdA] [int1, int1, int0, intMin1]),
                                                            
                                                            actionPref 
                                                                  actOfferA { constraint =    cstrITE (cstrEqual vexprPcP int1)
                                                                                                      (cstrEqual vexprPpcRHS' int0)
                                                                                                      (cstrConst (Cbool False))
                                                                              } 
                                                                  (procInst procIdP' [chanIdA] [int1, int1, vexprPpcLHS', intMin1]),
                                                            
                                                            actionPref 
                                                                  actOfferExit { constraint =  cstrEqual vexprPcP int0
                                                                              } 
                                                                  (procInst procIdP' [chanIdA] [int1, int0, intMin1, intMin1]),
                                                            
                                                            actionPref 
                                                                  actOfferExit { constraint = cstrAnd (Set.fromList [ 
                                                                                                      cstrITE (cstrEqual vexprPcP int1)
                                                                                                            (cstrITE (cstrEqual vexprPdisable' int0)
                                                                                                                  (cstrEqual vexprPpcLHS' int0)
                                                                                                                  (cstrConst (Cbool False)))
                                                                                                            (cstrConst (Cbool False))
                                                                                                ])
                                                                              } 
                                                                  (procInst procIdP' [chanIdA] [int1, int0, intMin1, intMin1])

                                                      ])


-- test LPE translation of DISABLE with recursion
-- P[A,B]() := A!1 [>> P[A,B]()
-- ERROR!
--    there is a possible trace with NO progress, i.e. when the RHS is always executed. 
--    

-- test LPE translation of DISABLE with recursion
-- P[A,B]() := A!1 [>> B?x >-> P[A,B]()
-- with procInst = P[A,B]()
-- becomes:
--                P[A,B](pc$P P$pre1$A$B$P$pre1$disable$lhs P$pre1$A$B$P$pre1$lhs$pc$P$pre1$lhs P$pre1$A$B$P$pre1$rhs$pc$P$pre1$rhs) :=
--                      A?A$1 [[ IF pc$P == 0 THEN A$1 == 1 ELSE FALSE FI  ]] 
--                            >-> LPE_P[A](1, 0, -1, 0)
--                 ##   A?A$1 [[ IF pc$P == 1 THEN (IF P$pre1$A$B$P$pre1$disable$lhs == 0 THEN (IF P$pre1$A$B$P$pre1$lhs$pc$P$pre1$lhs == 0 THEN A$1 == 1 ELSE FALSE) ELSE FALSE) ELSE FALSE FI  ]] 
--                            >-> LPE_P[A](1, 0, -1, P$pre1$A$B$P$pre1$rhs$pc$P$pre1$rhs)
--                 ##   B?B$1 [[ pc$P == 0 ]] 
--                            >-> LPE_P[A](0, P$pre1$A$B$P$pre1$disable$lhs, P$pre1$A$B$P$pre1$lhs$pc$P$pre1$lhs, P$pre1$A$B$P$pre1$rhs$pc$P$pre1$rhs)
--                 ##   B?B$1 [[ IF pc$P == 1 THEN P$pre1$A$B$P$pre1$rhs$pc$P$pre1$rhs == 0 ELSE FALSE FI  ]] 
--                            >-> LPE_P[A]( 0, P$pre1$A$B$P$pre1$disable$lhs, P$pre1$A$B$P$pre1$lhs$pc$P$pre1$lhs, P$pre1$A$B$P$pre1$rhs$pc$P$pre1$rhs)
--          with ProcInst: P[A](0, ANY, ANY, ANY)

testLPEDisableRec :: Test
testLPEDisableRec = TestCase $
--    trace ("\ntestDisable2:\n expected:" ++  pshow (procInst', DefProc procDefExpected)  ++ 
--             "\ngot: " ++ pshow (res_procInst, DefProc res_procDef) ++ 
--             "\n res_procDef: " ++ (pshow $ DefProc res_procDef)) $
      assertBool "LPE Disable Rec" $ eqProcDef (Just (procInst', procDefExpected)) (Just (res_procInst, res_procDef))--((eqProcDef procDefExpected res_procDef)  && (procInst' ~~ res_procInst))
   where
      (res_procInst, res_procDef) = fromMaybe (error "could not find the given procId 1") $ lpeTransformFunc procInst'' procDefs'
      -- extract expected ProcDef from all results:
      -- res_procDef = fromMaybe (error "could not find the given procId") (Map.lookup res_procId res_procDefs')

      procInst'' = procInst procIdP [chanIdA, chanIdB] []
      procIdP = procIdGen "P" [chanIdA, chanIdB] [ ]
      procDefP = ProcDef [chanIdA, chanIdB] [] (disable (actionPref actOfferA1 stop)
                                                            (actionPref actOfferBx procInst''))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      varIdPdisable' :: VarId
      varIdPdisable' = VarId (T.pack "P$pre1$A$B$P$pre1$disable$lhs") 3033 intSort
      varIdPpcLHS' :: VarId
      varIdPpcLHS' = VarId (T.pack "P$pre1$A$B$P$pre1$lhs$pc$P$pre1$lhs") 3034 intSort
      varIdPpcRHS' :: VarId
      varIdPpcRHS' = VarId (T.pack "P$pre1$A$B$P$pre1$rhs$pc$P$pre1$rhs") 3035 intSort
      
      vexprPdisable' :: VExpr
      vexprPdisable' = cstrVar varIdPdisable'
      vexprPpcLHS' :: VExpr
      vexprPpcLHS' = cstrVar varIdPpcLHS'
      vexprPpcRHS' :: VExpr
      vexprPpcRHS' = cstrVar varIdPpcRHS'



      procIdP' = procIdGen "LPE_P" [chanIdA, chanIdB] [varIdPcP, varIdPdisable', varIdPpcLHS', varIdPpcRHS']
      procInst' = procInst procIdP' [chanIdA, chanIdB] [int0, anyInt, anyInt, anyInt]
      procDefExpected = ProcDef [chanIdA, chanIdB] [varIdPcP, varIdPdisable', varIdPpcLHS', varIdPpcRHS']
                                                (choice $ Set.fromList [
                                                            -- A?A$1 [[ IF pc$P == 0 THEN A$1 == 1 ELSE FALSE FI  ]] 
                                                            --    >-> LPE_P[A](1, 0, -1, 0)
                                                            actionPref 
                                                                  actOfferAA1 { constraint = cstrITE (cstrEqual vexprPcP int0)
                                                                                                     (cstrEqual vexprA1 int1)
                                                                                                     (cstrConst (Cbool False))
                                                                              } 
                                                                  (procInst procIdP' [chanIdA, chanIdB] [int1, int0, intMin1, int0]),
                                                            -- ##   A?A$1 [[ IF pc$P == 1 THEN (IF P$pre1$A$B$P$pre1$disable$lhs == 0 THEN (IF P$pre1$A$B$P$pre1$lhs$pc$P$pre1$lhs == 0 THEN A$1 == 1 ELSE FALSE) ELSE FALSE) ELSE FALSE FI  ]] 
                                                            --          >-> LPE_P[A](1, 0, -1, P$pre1$A$B$P$pre1$rhs$pc$P$pre1$rhs)
                                                            actionPref 
                                                                  actOfferAA1 { constraint =      cstrITE (cstrEqual vexprPcP int1)
                                                                                                            (cstrITE (cstrEqual vexprPdisable' int0)
                                                                                                                  (cstrITE  (cstrEqual vexprPpcLHS' int0)
                                                                                                                        (cstrEqual vexprA1 int1)
                                                                                                                        (cstrConst (Cbool False)))
                                                                                                                  (cstrConst (Cbool False)))
                                                                                                            (cstrConst (Cbool False))
                                                                              } 
                                                                  (procInst procIdP' [chanIdA, chanIdB] [int1, int0, intMin1, vexprPpcRHS']),
                                                            
                                                            -- ##   B?B$1 [[ pc$P == 0 ]] 
                                                            --          >-> LPE_P[A](0, P$pre1$A$B$P$pre1$disable$lhs, P$pre1$A$B$P$pre1$lhs$pc$P$pre1$lhs, P$pre1$A$B$P$pre1$rhs$pc$P$pre1$rhs)
                                                            actionPref 
                                                                  actOfferBB1 { constraint =  cstrEqual vexprPcP int0
                                                                              } 
                                                                  (procInst procIdP' [chanIdA, chanIdB] [int0, vexprPdisable', vexprPpcLHS', vexprPpcRHS']),
                                                            
                                                            -- ##   B?B$1 [[ IF pc$P == 1 THEN P$pre1$A$B$P$pre1$rhs$pc$P$pre1$rhs == 0 ELSE FALSE FI  ]] 
                                                            --          >-> LPE_P[A]( 0, P$pre1$A$B$P$pre1$disable$lhs, P$pre1$A$B$P$pre1$lhs$pc$P$pre1$lhs, P$pre1$A$B$P$pre1$rhs$pc$P$pre1$rhs)
                                                            actionPref 
                                                                  actOfferBB1 { constraint = cstrITE (cstrEqual vexprPcP int1)
                                                                                                            (cstrEqual vexprPpcRHS' int0)
                                                                                                            (cstrConst (Cbool False))
                                                                              } 
                                                                  (procInst procIdP' [chanIdA, chanIdB] [int0, vexprPdisable', vexprPpcLHS', vexprPpcRHS'])
                                                      ])



-- -- P[]() ::= STOP [>< EXIT
-- --    with procInst = P[]()
-- -- becomes 
-- -- P[](pc$P$interrupt$lhs, pc$P$interrupt$rhs) ::=
-- --             {} [pc$P$interrupt$rhs == 0] >-> P$interrupt[](0,0)     
-- -- with procInst = P(0, 0)
        
-- testLPEInterruptStopExit :: Test
-- testLPEInterruptStopExit = TestCase $
--    trace ("\testLPEInterruptStopExit:\n expected:" ++  pshow (procInst', DefProc procDefPlpe)  ++ 
--             "\ngot: " ++ pshow (res_procInst, DefProc res_procDef)) $
--             -- "\n res_procDefs': " ++ pshow_procDefs res_procDefs') $
--       assertBool "testStopExit" (eqProcDef (Just (procInst', procDefPlpe)) (Just (res_procInst, res_procDef) ))
--    where
--       (res_procInst, res_procDef) = fromMaybe (error "could not find the given procId") (lpeTransformFunc procInst'' procDefs')
--       -- (res_procInst, res_procDef) = result
--       -- (res_procInst@(TxsDefs.view -> ProcInst res_procId _ _), res_procDefs') = preGNFDisableFunc procInst'' chanOffers emptyTranslatedProcDefs procDefs'
--       -- -- extract expected ProcDef from all results:
--       -- res_procDef = fromMaybe (error "could not find the given procId") (Map.lookup res_procId res_procDefs')

--       procInst'' = procInst procIdP [] []
--       procIdP = procIdGen "P" [] []
--       procDefP = ProcDef [] [] (interrupt stop (actionPref actOfferExit stop))
--       procDefs' = Map.fromList  [  (procIdP, procDefP)]

--       procIdPlpe = procIdGen "LPE_P" [] [varIdPpcLHS, varIdPpcRHS]
--       procDefPlpe = ProcDef [chanIdA] [varIdPpcLHS, varIdPpcRHS] (
--                                     actionPref  ActOffer {  offers = Set.empty
--                                                             , hiddenvars = Set.fromList []
--                                                             , constraint = (cstrEqual vexprPpcRHS int0)
--                                                             } 
--                                                 (procInst procIdPlpe [] [int0, int0]))
--       procInst' = procInst procIdPlpe [] [int0, int0]

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

                        , TestLabel "lpeHide integration" testLPEHide1
                        , TestLabel "lpeHide integration" testLPEHide2
                        , TestLabel "lpeEnable integration" testEnable1
                        , TestLabel "lpeEnable integration recursion" testEnable1Rec
                        , TestLabel "lpeEnable integration recursion 2" testEnable2Rec

                        , TestLabel "lpeEnable integration 2" testEnable2

                        , TestLabel "lpe guard stop" testLPEGuardStop
                        , TestLabel "lpe guard ActionPref" testLPEGuardActionPref
                        , TestLabel "lpe guard Choice" testLPEGuardChoice
                        , TestLabel "lpe guard ProcInst" testLPEGuardProcInst
 
                        , TestLabel "lpe guard par" testLPEGuardPar
                        , TestLabel "lpe guard hide" testLPEGuardHide
                        , TestLabel "lpe guard enable" testLPEGuardEnable

                        , TestLabel "lpe disable" testLPEDisable1
                        , TestLabel "lpe disable rec" testLPEDisableRec
                        -- TestLabel "testLPEInterruptStopExit" testLPEInterruptStopExit
                        -- , TestLabel "multi chanoffer translation" testMultiChanOffer
                        ]
