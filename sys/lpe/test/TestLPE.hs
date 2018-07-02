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
import qualified Data.Text         as T
import VarId
import Constant
import ValExpr

import LPEfunc

import TestDefinitions

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
   assertBool "test LPEPar integration" $ eqProcDef (Just (procInst', procDefP')) res
   where
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
--       varIdS = VarId (T.pack "s") 33 intSort
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
--       -- varIdPABs = VarId (T.pack "P$A$B$s") 33 intSort
--       -- varIdPgnf1ABs = VarId (T.pack "P$gnf1$A$B$s") 33 intSort
--       -- varIdPgnf1ABx = VarId (T.pack "P$gnf1$A$B$x") 33 intSort

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
      varIdPcPpre1 = VarId (T.pack "P$pre1$A$pc$P$pre1") 0 intSort
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


      varIdPcPpre1 = VarId (T.pack "P$pre1$A$pc$P$pre1") 0 intSort
      vexprPcPpre1 = cstrVar varIdPcPpre1
      varIdPpre1QAx = VarId (T.pack "P$pre1$A$Q$A$x") 0 intSort
      -- vexprPpre1QAx = cstrVar varIdPpre1QAx
      varIdA1' = VarId (T.pack "A$1_5") 34 intSort
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
--    LPE_P[A]() := EXIT[pcP == 0] >-> P[A](1)
--    LPE_P[A](0)

testEnable1 :: Test
testEnable1 = TestCase $
   --    trace ("\n\n expected:" ++ show (Just (procInst', procDefPlpe)) ++ "\ngot: " ++ show res) $ 
   assertBool "simple EXIT" (eqProcDef (Just (procInst', procDefPlpe)) res)
   where
      res = lpeTransformFunc procInst'' procDefs'
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      
      -- action: EXIT
      actOfferExit :: ActOffer
      actOfferExit   = ActOffer {  offers = Set.singleton
                                          Offer { chanid = chanIdExit
                                                , chanoffers = []
                                          }
                              , hiddenvars = Set.empty
                              , constraint = cstrConst (Cbool True)
                  }
      
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





-- P[A]() := EXIT !1 >>> ACCEPT ?id IN A!id >-> STOP NI
-- with procInst = P[A]()
-- 
-- becomes
-- LPE_P[A](pc$P, P$pre1$enable$A$id) :=
--                        {}    [pc$P == 0, EXIT$1 == 1] {EXIT$1}               >-> LPE_P[A](1, EXIT$1)
--                    ##  A?A$1 [pc$P == 1, A$1 == P$pre1$enable$A$id] {}       >-> LPE_P[A](-1, ANY)
-- with procInst = LPE_P[A](0, ANY)
testEnable2 :: Test
testEnable2 = TestCase $
      -- trace ("\n\n expected:" ++ show (Just (procInstlpe, procDefPlpe)) ++ "\ngot: " ++ show res) $ 
      assertBool "simple EXIT" (eqProcDef (Just (procInstlpe, procDefPlpe)) res)
      where
      res = lpeTransformFunc procInst' procDefs'
      procInst' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      

      -- action: EXIT !1
      actOfferExit :: ActOffer
      actOfferExit   = ActOffer {  offers = Set.singleton
                                          Offer { chanid = chanIdExit
                                                , chanoffers = [Exclam int1]
                                          }
                              , hiddenvars = Set.empty
                              , constraint = cstrConst (Cbool True)
                  }

      varIdid = VarId (T.pack "id") 33 intSort
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

      varIdPenableId = VarId (T.pack "P$pre1$enable$A$id") 123 intSort
      vexprPenableId = cstrVar varIdPenableId

      procDefP = ProcDef [chanIdA] [] (enable (actionPref actOfferExit stop)
                                                [Quest varIdid]
                                                (actionPref actOfferAid stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]



      procIdPlpe = procIdGen "LPE_P" [chanIdA] [varIdPcP, varIdPenableId]
      procDefPlpe = ProcDef [chanIdA] [varIdPcP, varIdPenableId] 
                                                (choice (Set.fromList [
                                                            actionPref ActOffer {    offers = Set.empty
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

                        , TestLabel "lpeHide integration" testLPEHide1
                        , TestLabel "lpeHide integration" testLPEHide2
                        , TestLabel "lpeEnable integration" testEnable1
                        , TestLabel "lpeEnable integration 2" testEnable2

                        --, TestLabel "multi chanoffer translation" testMultiChanOffer
                        ]
