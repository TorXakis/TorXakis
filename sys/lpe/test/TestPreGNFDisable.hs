{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE ViewPatterns        #-}

module TestPreGNFDisable
(
      testPreGNFDisableList
)
where

import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text         as T
import           Data.Maybe


import TxsDefs
import VarId
import Constant
import ValExpr
import TranslatedProcDefs
import LPEfunc
import TestDefinitions

-- import Debug.Trace
-- import TxsShow


--type ProcDefs = Map.Map ProcId ProcDef

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------


-- P[A]() := A >-> STOP [>> A >-> STOP
-- with procInst = P[A]()
-- becomes:
--          first make a new ProcDef of left side of DISABLE: 
--                P$lhs[A]() := A >-> STOP 
--                with procInst: P$lhs[A]()
-- 
--                then translate to LPE: 
--                P$lhs[A](pc$P$lhs) := A [pc$P$lhs == 0] >-> P$lhs[A](-1) 
--                with procInst: P$lhs[A](0)
-- 
--          then make a new ProcDef of right side of DISABLE: 
--                P$rhs[A]() := A >-> STOP 
--                with procInst: P$rhs[A]()
-- 
--                then translate to LPE: 
--                P$rhs[A](pc$P$rhs) := A [pc$P$rhs == 0] >-> P$rhs[A](-1) 
--                with procInst: P$rhs[A](0)
--          
--          make a new ProcDef for P
--                P[A](P$disable$lhs, P$lhs$pc$P$lhs, P$rhs$pc$P$rhs) :=
--                      A [P$disable$lhs == 0, P$lhs$pc$P$lhs == 0] >-> P[A](0, -1, P$rhs$pc$P$rhs) 
--                 ##   A [P$rhs$pc$P$rhs == 0]                     >-> P[A](1, P$lhs$pc$P$lhs, -1)   
--          with ProcInst: P[A](0,0,0)
testDisable1 :: Test
testDisable1 = TestCase $
--    trace ("\ntestDisable1:\n expected:" ++  pshow (procInst', DefProc procDefExpected)  ++ 
--             "\ngot: " ++ pshow (res_procInst, DefProc res_procDef) ++ 
--             "\n res_procDefs': " ++ pshow_procDefs res_procDefs') $
      assertBool "ActionPref, ActionPref" $ eqProcDef (Just (procInst', procDefExpected)) ((Just (res_procInst, res_procDef)))--((eqProcDef procDefExpected res_procDef)  && (procInst' ~~ res_procInst))
   where
      (res_procInst@(TxsDefs.view -> ProcInst res_procId _ _), res_procDefs') = preGNFDisableFunc procInst'' chanOffers emptyTranslatedProcDefs procDefs'
      -- extract expected ProcDef from all results:
      res_procDef = fromMaybe (error "could not find the given procId") (Map.lookup res_procId res_procDefs')

      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] [ ]
      procDefP = ProcDef [chanIdA] [] (disable (actionPref actOfferA stop)
                                                (actionPref actOfferA stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdP' = procIdGen "P" [chanIdA] [varIdPdisable, varIdPpcLHS, varIdPpcRHS]
      procInst' = procInst procIdP' [chanIdA] [int0, int0, int0]
      procDefExpected = ProcDef [chanIdA] [varIdPdisable, varIdPpcLHS, varIdPpcRHS]
                                                (choice $ Set.fromList [

                                                      (actionPref 
                                                            actOfferA { constraint = cstrAnd (Set.fromList [ 
                                                                                                cstrITE (cstrEqual vexprPdisable int0)
                                                                                                      (cstrEqual vexprPpcLHS int0)
                                                                                                      (cstrConst (Cbool False))
                                                                                          ])
                                                                        } 
                                                            (procInst procIdP' [chanIdA] [int0, intMin1, vexprPpcRHS])),

                                                      (actionPref 
                                                            actOfferA { constraint = (cstrEqual vexprPpcRHS int0)
                                                                        } 
                                                            (procInst procIdP' [chanIdA] [int1, vexprPpcLHS, intMin1])) 


                                                ])





-- P[A]() := EXIT >-> STOP [>> A >-> STOP
-- with procInst = P[A]()
-- becomes:
--          first make a new ProcDef of left side of DISABLE: 
--                P$lhs[A]() := EXIT >-> STOP 
--                with procInst: P$lhs[A]()
-- 
--                then translate to LPE: 
--                P$lhs[A](pc$P$lhs) := EXIT [pc$P$lhs == 0] >-> P$lhs[A](-1) 
--                with procInst: P$lhs[A](0)
-- 
--          then make a new ProcDef of right side of DISABLE: 
--                P$rhs[A]() := A >-> STOP 
--                with procInst: P$rhs[A]()
-- 
--                then translate to LPE: 
--                P$rhs[A](pc$P$rhs) := A [pc$P$rhs == 0] >-> P$rhs[A](-1) 
--                with procInst: P$rhs[A](0)
--          
--          make a new ProcDef for P
--                P[A](P$disable$lhs, P$lhs$pc$P$lhs, P$rhs$pc$P$rhs) :=
--                      EXIT [P$disable$lhs == 0, P$lhs$pc$P$lhs == 0] >-> P[A](0, -1, -1)                       -- NOTICE that RHS has been disabled by setting pc$P$rhs to -1! 
--                 ##   A [P$rhs$pc$P$rhs == 0]                        >-> P[A](1, P$lhs$pc$P$lhs, -1)   
--          with ProcInst: P[A](0,0,0)

testDisable2 :: Test
testDisable2 = TestCase $
--    trace ("\ntestDisable2:\n expected:" ++  pshow (procInst', DefProc procDefExpected)  ++ 
--             "\ngot: " ++ pshow (res_procInst, DefProc res_procDef) ++ 
--             "\n res_procDefs': " ++ pshow_procDefs res_procDefs') $
      assertBool "EXIT, ActionPref" $ eqProcDef (Just (procInst', procDefExpected)) ((Just (res_procInst, res_procDef)))--((eqProcDef procDefExpected res_procDef)  && (procInst' ~~ res_procInst))
   where
      (res_procInst@(TxsDefs.view -> ProcInst res_procId _ _), res_procDefs') = preGNFDisableFunc procInst'' chanOffers emptyTranslatedProcDefs procDefs'
      -- extract expected ProcDef from all results:
      res_procDef = fromMaybe (error "could not find the given procId") (Map.lookup res_procId res_procDefs')

      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] [ ]
      procDefP = ProcDef [chanIdA] [] (disable (actionPref actOfferExit stop)
                                                (actionPref actOfferA stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdP' = procIdGen "P" [chanIdA] [varIdPdisable, varIdPpcLHS, varIdPpcRHS]
      procInst' = procInst procIdP' [chanIdA] [int0, int0, int0]
      procDefExpected = ProcDef [chanIdA] [varIdPdisable, varIdPpcLHS, varIdPpcRHS]
                                                (choice $ Set.fromList [
                                                      (actionPref 
                                                            actOfferExit { constraint = cstrAnd (Set.fromList [ 
                                                                                                cstrITE (cstrEqual vexprPdisable int0)
                                                                                                      (cstrEqual vexprPpcLHS int0)
                                                                                                      (cstrConst (Cbool False))
                                                                                          ])
                                                                        } 
                                                            (procInst procIdP' [chanIdA] [int0, intMin1, intMin1])),

                                                      (actionPref 
                                                            actOfferA { constraint = (cstrEqual vexprPpcRHS int0)
                                                                        } 
                                                            (procInst procIdP' [chanIdA] [int1, vexprPpcLHS, intMin1])) 


                                                ])

        



-- P[A]() := A | EXIT >-> STOP [>> A >-> STOP
-- with procInst = P[A]()
-- becomes:
--          first make a new ProcDef of left side of DISABLE: 
--                P$lhs[A]() := A | EXIT >-> STOP 
--                with procInst: P$lhs[A]()

--                then translate to LPE: 
--                P$lhs[A](pc$P$lhs) := A | EXIT [pc$P$lhs == 0] >-> P$lhs[A](-1) 
--                with procInst: P$lhs[A](0)

--          then make a new ProcDef of right side of DISABLE: 
--                P$rhs[A]() := A >-> STOP 
--                with procInst: P$rhs[A]()

--                then translate to LPE: 
--                P$rhs[A](pc$P$rhs) := A [pc$P$rhs == 0] >-> P$rhs[A](-1) 
--                with procInst: P$rhs[A](0)
         
--          make a new ProcDef for P
--                P[A](P$disable$lhs, P$lhs$pc$P$lhs, P$rhs$pc$P$rhs) :=
--                      A | EXIT [P$disable$lhs == 0, P$lhs$pc$P$lhs == 0] >-> P[A](0, -1, -1) 
--                 ##   A [P$rhs$pc$P$rhs == 0]                            >-> P[A](1, P$lhs$pc$P$lhs, -1)   
--          with ProcInst: P[A](0,0,0)

testDisable3 :: Test
testDisable3 = TestCase $
--    trace ("\ntestDisable3:\n expected:" ++  pshow (procInst', DefProc procDefExpected)  ++ 
--             "\ngot: " ++ pshow (res_procInst, DefProc res_procDef) ++ 
--             "\n res_procDefs': " ++ pshow_procDefs res_procDefs') $
      assertBool "ActionPref | EXIT, ActionPref" $ eqProcDef (Just (procInst', procDefExpected)) ((Just (res_procInst, res_procDef)))--((eqProcDef procDefExpected res_procDef)  && (procInst' ~~ res_procInst))
   where
      (res_procInst@(TxsDefs.view -> ProcInst res_procId _ _), res_procDefs') = preGNFDisableFunc procInst'' chanOffers emptyTranslatedProcDefs procDefs'
      -- extract expected ProcDef from all results:
      res_procDef = fromMaybe (error "could not find the given procId") (Map.lookup res_procId res_procDefs')

      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] [ ]
      procDefP = ProcDef [chanIdA] [] (disable (actionPref actOfferAExit stop)
                                                (actionPref actOfferA stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]


      -- action: A | EXIT
      actOfferAExit :: ActOffer
      actOfferAExit   = ActOffer {  offers = Set.fromList [
                                          Offer { chanid = chanIdA0
                                                , chanoffers = []
                                                },
                                          Offer { chanid = chanIdExit
                                                , chanoffers = []
                                                }]
                                    , hiddenvars = Set.empty
                                    , constraint = cstrConst (Cbool True)
                  }


      procIdP' = procIdGen "P" [chanIdA] [varIdPdisable, varIdPpcLHS, varIdPpcRHS]
      procInst' = procInst procIdP' [chanIdA] [int0, int0, int0]
      procDefExpected = ProcDef [chanIdA] [varIdPdisable, varIdPpcLHS, varIdPpcRHS]
                                                (choice $ Set.fromList [

                                                      (actionPref 
                                                            actOfferAExit { constraint = cstrAnd (Set.fromList [ 
                                                                                                cstrITE (cstrEqual vexprPdisable int0)
                                                                                                      (cstrEqual vexprPpcLHS int0)
                                                                                                      (cstrConst (Cbool False))
                                                                                          ])
                                                                        } 
                                                            (procInst procIdP' [chanIdA] [int0, intMin1, intMin1])),

                                                      (actionPref 
                                                            actOfferA { constraint = (cstrEqual vexprPpcRHS int0)
                                                                        } 
                                                            (procInst procIdP' [chanIdA] [int1, vexprPpcLHS, intMin1])) 


                                                ])

        
  

-- P[A]() := P1[A](0) [>> A?x >-> STOP
-- P1[A](y) :=    A?x >-> STOP
--             ## A?x >-> P1[A](2)
-- with procInst = P[A]()
-- becomes:
--          first make a new ProcDef of left side of DISABLE: 
--                P$lhs[A]() :=     A?x >-> STOP
--                               ## A?x >-> P1[A]()
--                with procInst: P$lhs[A]()
-- 
--                then translate to LPE: 
--                P$lhs[A](pc$P$lhs) :=     A?A$1 [pc$P$lhs == 0] >-> P$lhs[A](-1, ANY) 
--                                       ## A?A$1 [pc$P$lhs == 0] >-> P$lhs[A](1, 2)
--                                       ## A?A$1 [pc$P$lhs == 1] >-> P$lhs[A](-1, ANY)        -- created because P1 is unfolded inside the new P$lhs
--                                       ## A?A$1 [pc$P$lhs == 1] >-> P$lhs[A](1, 2)           -- "
--                with procInst: P$lhs[A](0, ANY)
-- 
--          then make a new ProcDef of right side of DISABLE: 
--                P$rhs[A]() := A?x >-> STOP 
--                with procInst: P$rhs[A]()
-- 
--                then translate to LPE: 
--                P$rhs[A](pc$P$rhs) := A?A$1 [pc$P$rhs == 0] >-> P$rhs[A](-1) 
--                with procInst: P$rhs[A](0, ANY)
--          
--          make a new ProcDef for P
--                P[A](P$disable$lhs, P$lhs$pc$P$lhs, P$lhs$P1$A$y, P$rhs$pc$P$rhs) :=
--                    A$A$1 [P$rhs$pc$P$rhs == 0]                     >-> P[A](1, P$lhs$pc$P$lhs, P$lhs$P1$A$y, -1)   
--                 ## A?A$1 [P$disable$lhs == 0, P$lhs$pc$P$lhs == 0] >-> P[A](0, -1, ANY, P$rhs$pc$P$rhs) 
--                 ## A?A$1 [P$disable$lhs == 0, P$lhs$pc$P$lhs == 0] >-> P[A](0, 1, 2, P$rhs$pc$P$rhs)
--                 ## A?A$1 [P$disable$lhs == 0, P$lhs$pc$P$lhs == 1] >-> P[A](0, -1, ANY, P$rhs$pc$P$rhs)
--                 ## A?A$1 [P$disable$lhs == 0, P$lhs$pc$P$lhs == 1] >-> P[A](0, 1, 2, P$rhs$pc$P$rhs) 
--          with ProcInst: P[A](0, 0, ANY, 0)
testDisable4 :: Test
testDisable4 = TestCase $
--    trace ("\ntestDisable4:\n expected:" ++  pshow (procInst', DefProc procDefExpected)  ++ 
--             "\ngot: " ++ pshow (res_procInst, DefProc res_procDef) ++ 
--             "\n res_procDefs': " ++ pshow_procDefs res_procDefs') $
      assertBool "ProcInst, ActionPref" $ eqProcDef (Just (procInst', procDefExpected)) ((Just (res_procInst, res_procDef)))--((eqProcDef procDefExpected res_procDef)  && (procInst' ~~ res_procInst))
   where
      (res_procInst@(TxsDefs.view -> ProcInst res_procId _ _), res_procDefs') = preGNFDisableFunc procInst'' chanOffers emptyTranslatedProcDefs procDefs'
      -- extract expected ProcDef from all results:
      res_procDef = fromMaybe (error "could not find the given procId") (Map.lookup res_procId res_procDefs')

      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] [ ]
      procDefP = ProcDef [chanIdA] [] (disable (procInst procIdP1 [chanIdA] [int0])
                                                (actionPref actOfferAx stop))
      procIdP1 = procIdGen "P1" [chanIdA] [varIdY]
      procDefP1 = ProcDef [chanIdA] [varIdY] (choice $ Set.fromList [
                                                (actionPref actOfferAx stop)
                                          ,     (actionPref actOfferAx (procInst procIdP1 [chanIdA] [int2]))
                                          ])
      procDefs' = Map.fromList [  (procIdP, procDefP)
                               ,  (procIdP1, procDefP1)]


      varIdPlhsP1y :: VarId
      varIdPlhsP1y = VarId (T.pack "P$lhs$P1$A$y") 33 intSort
      vexprPlhsP1y :: VExpr
      vexprPlhsP1y = cstrVar varIdPlhsP1y
--          
--          make a new ProcDef for P
--                P[A](P$disable$lhs, P$lhs$pc$P$lhs, P$lhs$P1$A$y, P$rhs$pc$P$rhs) :=
--                    A$A$1 [P$rhs$pc$P$rhs == 0]                     >-> P[A](1, P$lhs$pc$P$lhs, P$lhs$P1$A$y, -1)   
--                 ## A?A$1 [P$disable$lhs == 0, P$lhs$pc$P$lhs == 0] >-> P[A](0, -1, ANY, P$rhs$pc$P$rhs) 
--                 ## A?A$1 [P$disable$lhs == 0, P$lhs$pc$P$lhs == 0] >-> P[A](0, 1, 2, P$rhs$pc$P$rhs)
--                 ## A?A$1 [P$disable$lhs == 0, P$lhs$pc$P$lhs == 1] >-> P[A](0, -1, ANY, P$rhs$pc$P$rhs)
--                 ## A?A$1 [P$disable$lhs == 0, P$lhs$pc$P$lhs == 1] >-> P[A](0, 1, 2, P$rhs$pc$P$rhs) 
--          with ProcInst: P[A](0, 0, ANY, 0)
      procIdP' = procIdGen "P" [chanIdA] [varIdPdisable, varIdPpcLHS, varIdPlhsP1y, varIdPpcRHS]
      procInst' = procInst procIdP' [chanIdA] [int0, int0, anyInt, int0]
      procDefExpected = ProcDef [chanIdA] [varIdPdisable, varIdPpcLHS, varIdPlhsP1y, varIdPpcRHS]
                                                (choice $ Set.fromList [
                                                      --                    A$A$1 [P$rhs$pc$P$rhs == 0]                     >-> P[A](1, P$lhs$pc$P$lhs, P$lhs$P1$A$y, -1)   
                                                      (actionPref 
                                                            actOfferA { offers = Set.singleton
                                                                              Offer { chanid = chanIdA
                                                                                    , chanoffers = [Quest varIdA1]
                                                                              },
                                                                        constraint = (cstrEqual vexprPpcRHS int0)
                                                                        } 
                                                            (procInst procIdP' [chanIdA] [int1, vexprPpcLHS, vexprPlhsP1y, intMin1])),

                                                      --                 ## A?A$1 [P$disable$lhs == 0, P$lhs$pc$P$lhs == 0] >-> P[A](0, -1, ANY, P$rhs$pc$P$rhs) 
                                                      (actionPref 
                                                            actOfferAx {offers = Set.singleton
                                                                              Offer { chanid = chanIdA
                                                                                    , chanoffers = [Quest varIdA1]
                                                                              },
                                                                        constraint = cstrAnd (Set.fromList [ 
                                                                                          cstrITE (cstrEqual vexprPdisable int0)
                                                                                                (cstrEqual vexprPpcLHS int0)
                                                                                                (cstrConst (Cbool False))
                                                                                    ])
                                                                        } 
                                                            (procInst procIdP' [chanIdA] [int0, intMin1, anyInt, vexprPpcRHS])),

                                                      --                 ## A?A$1 [P$disable$lhs == 0, P$lhs$pc$P$lhs == 0] >-> P[A](0, 1, 2, P$rhs$pc$P$rhs)
                                                      (actionPref 
                                                            actOfferAx {offers = Set.singleton
                                                                              Offer { chanid = chanIdA
                                                                                    , chanoffers = [Quest varIdA1]
                                                                              },
                                                                        constraint = cstrAnd (Set.fromList [ 
                                                                                          cstrITE (cstrEqual vexprPdisable int0)
                                                                                                (cstrEqual vexprPpcLHS int0)
                                                                                                (cstrConst (Cbool False))
                                                                                    ])
                                                                        } 
                                                            (procInst procIdP' [chanIdA] [int0, int1, int2, vexprPpcRHS])),

                                                      --                 ## A?A$1 [P$disable$lhs == 0, P$lhs$pc$P$lhs == 1] >-> P[A](0, -1, ANY, P$rhs$pc$P$rhs)
                                                      (actionPref 
                                                            actOfferAx {offers = Set.singleton
                                                                              Offer { chanid = chanIdA
                                                                                    , chanoffers = [Quest varIdA1]
                                                                              },
                                                                        constraint = cstrAnd (Set.fromList [ 
                                                                                          cstrITE (cstrEqual vexprPdisable int0)
                                                                                                (cstrEqual vexprPpcLHS int1)
                                                                                                (cstrConst (Cbool False))
                                                                                    ])
                                                                        } 
                                                            (procInst procIdP' [chanIdA] [int0, intMin1, anyInt, vexprPpcRHS])),

                                                      --                 ## A?A$1 [P$disable$lhs == 0, P$lhs$pc$P$lhs == 1] >-> P[A](0, 1, 2, P$rhs$pc$P$rhs) 
                                                      (actionPref 
                                                      actOfferAx {offers = Set.singleton
                                                                        Offer { chanid = chanIdA
                                                                              , chanoffers = [Quest varIdA1]
                                                                        },
                                                                  constraint = cstrAnd (Set.fromList [ 
                                                                                    cstrITE (cstrEqual vexprPdisable int0)
                                                                                          (cstrEqual vexprPpcLHS int1)
                                                                                          (cstrConst (Cbool False))
                                                                              ])
                                                                  } 
                                                      (procInst procIdP' [chanIdA] [int0, int1, int2, vexprPpcRHS]))
                                                ])

        

-- P[A](x) := P1[A](x) [>> P1[A](x)
-- P1[A](y) :=     A >-> STOP
-- with procInst = P[A](2)
-- becomes:
--          first make a new ProcDef of left side of DISABLE: 
--                then translate to LPE: 
--                P$lhs[A](pc$P$lhs, P$lhs$A$x) := A [pc$P$lhs == 0] >-> P$lhs[A](-1, ANY) 
--                with procInst: P$lhs[A](0, ANY)
-- 
--          then make a new ProcDef of right side of DISABLE: 
--                then translate to LPE: 
--                P$rhs[A](pc$P$rhs, P$rhs$A$x) := A [pc$P$rhs == 0] >-> P$rhs[A](-1, ANY) 
--                with procInst: P$rhs[A](0, ANY)
--          
--          make a new ProcDef for P
--                P[A](P$disable$lhs, x, P$lhs$pc$P$lhs, P$lhs$P$lhs$A$x, P$rhs$pc$P$rhs, P$rhs$P$rhs$A$x) :=
--                      A [P$disable$lhs == 0, pc$P$lhs == 0]   >-> P[A](0, x, -1, ANY, pc$P$rhs, P$rhs$A$x) 
--                 ##   A [pc$P$rhs == 0]                       >-> P[A](1, x, pc$P$lhs, P$lhs$A$x, -1, ANY)   
--          with ProcInst: P[A](0, 2, 0, ANY, 0, ANY)
testDisable5 :: Test
testDisable5 = TestCase $
--    trace ("\ntestDisable5:\n expected:" ++  pshow (procInst', DefProc procDefExpected)  ++ 
--             "\ngot: " ++ pshow (res_procInst, DefProc res_procDef) ++ 
--             "\n res_procDefs': " ++ pshow_procDefs res_procDefs') $
      assertBool "ProcInst, ActionPref" $ eqProcDef (Just (procInst', procDefExpected)) ((Just (res_procInst, res_procDef)))--((eqProcDef procDefExpected res_procDef)  && (procInst' ~~ res_procInst))
   where
      (res_procInst@(TxsDefs.view -> ProcInst res_procId _ _), res_procDefs') = preGNFDisableFunc procInst'' chanOffers emptyTranslatedProcDefs procDefs'
      -- extract expected ProcDef from all results:
      res_procDef = fromMaybe (error "could not find the given procId") (Map.lookup res_procId res_procDefs')

      procInst'' = procInst procIdP [chanIdA] [int2]
      procIdP = procIdGen "P" [chanIdA] [varIdX]
      procDefP = ProcDef [chanIdA] [varIdX] (disable  (procInst procIdP1 [chanIdA] [vexprX])
                                                      (procInst procIdP1 [chanIdA] [vexprX]))
      procIdP1 = procIdGen "P1" [chanIdA] [varIdY]
      procDefP1 = ProcDef [chanIdA] [varIdY] (actionPref actOfferA stop)

      procDefs' = Map.fromList [  (procIdP, procDefP)
                               ,  (procIdP1, procDefP1)]


      varIdPlhsX :: VarId
      varIdPlhsX = VarId (T.pack "P$lhs$P$lhs$A$x") 33 intSort
      vexprPlhsX :: VExpr
      vexprPlhsX = cstrVar varIdPlhsX

      varIdPrhsX :: VarId
      varIdPrhsX = VarId (T.pack "P$rhs$P$rhs$A$x") 33 intSort
      vexprPrhsX :: VExpr
      vexprPrhsX = cstrVar varIdPrhsX

      procIdP' = procIdGen "P" [chanIdA] [varIdPdisable, varIdX, varIdPpcLHS, varIdPlhsX, varIdPpcRHS, varIdPrhsX]
      procInst' = procInst procIdP' [chanIdA] [int0, int2, int0, anyInt, int0, anyInt]
      procDefExpected = ProcDef [chanIdA] [varIdPdisable, varIdX, varIdPpcLHS, varIdPlhsX, varIdPpcRHS, varIdPrhsX]
                                                (choice $ Set.fromList [
                                                      (actionPref 
                                                            actOfferA { constraint = cstrAnd (Set.fromList [ 
                                                                                                cstrITE (cstrEqual vexprPdisable int0)
                                                                                                      (cstrEqual vexprPpcLHS int0)
                                                                                                      (cstrConst (Cbool False))
                                                                                          ])
                                                                        } 
                                                            (procInst procIdP' [chanIdA] [int0, vexprX, intMin1, anyInt, vexprPpcRHS, vexprPrhsX])),

                                                      (actionPref 
                                                            actOfferA { constraint = (cstrEqual vexprPpcRHS int0)
                                                                        } 
                                                            (procInst procIdP' [chanIdA] [int1, vexprX, vexprPpcLHS, vexprPlhsX, intMin1, anyInt])) 
                                                ])

        


----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testPreGNFDisableList :: Test
testPreGNFDisableList = TestList [  
                             TestLabel "ActionPref, ActionPref" testDisable1
                        ,    TestLabel "EXIT , ActionPref" testDisable2
                        ,    TestLabel "EXIT | A , ActionPref" testDisable3
                        ,    TestLabel "ProcInst , ActionPref" testDisable4
                        ,    TestLabel "ProcInst , ActionPref" testDisable5

                        ]
