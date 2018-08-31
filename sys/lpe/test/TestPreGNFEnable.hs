{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module TestPreGNFEnable
(
testPreGNFEnableList
)
where


import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text         as T

import TxsDefs
import VarId
import Constant
import ValExpr
import TranslatedProcDefs
import LPEfunc
import TestDefinitions

-- import Debug.Trace

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------


-- P[A]() := EXIT >>> STOP
-- with procInst = P[A]()
-- becomes:
--    preGNFEnable of   P[A]()
--                      P[A]() := EXIT >>> STOP
--          first LPE of left side of ENABLE:
--                P[A](pc$P) := EXIT [pc$P == 0] >-> STOP 
--                with procInst = P[A](0)
--          make a new ProcDef P$enable of the right hand side of ENABLE operator
--                P$enable[A]() := STOP
--          then replace each occurrence of EXIT >-> BExpr with an empty ActionPrefix and a ProcInst to P$enable
--                P[A](pc$P) := {} [pc$P == 0] >-> P$enable[A]()           
--          then run preGNF on P$enable
--                extension of scope of variables from the lhs to rhs?
--                just by explicit communication via EXIT/ACCEPT
-- becomes
-- P[A](pc$P) := {} [pc$P == 0] >-> P$enable[A]()
-- with procInst = P[A](0)                // just the procInst of the LPE of lhs

testExit1 :: Test
testExit1 = TestCase $
   --trace ("\n\ntestExit1:\n expected:" ++ show (procInst', procDefs''') ++ "\ngot: " ++ show  (res_procInst, res_procDefs'')) $ 
      assertBool "simple EXIT" (eqProcDefs procDefs''' res_procDefs'' && (procInst' ~~ res_procInst))
   where
      res_procDefs'' = Map.delete procIdP res_procDefs'  -- remove the original ProcId for the comparison with the expected result
      (res_procInst, res_procDefs') = preGNFEnableFunc procInst'' chanOffers emptyTranslatedProcDefs procDefs'
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] [ ]
            
      procDefP = ProcDef [chanIdA] [] (enable (actionPref actOfferExit stop)
                                              [] 
                                              stop)
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdP' = procIdGen "P" [chanIdA] [varIdPcP]
      procIDPenable = procIdGen "P$enable" [chanIdA] []

      procInst' = procInst procIdP' [chanIdA] [int0]
      procDefs''' = Map.fromList [(procIdP', ProcDef [chanIdA] [varIdPcP] 
                                                (actionPref 
                                                      ActOffer {    offers = Set.empty
                                                                  , hiddenvars = Set.empty
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (procInst procIDPenable [chanIdA] []))),
                                    (procIDPenable, ProcDef [chanIdA] [] stop)
                        ]




-- P[A]() := EXIT >>> A >-> STOP
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) := {} [pc$P == 0] >-> P$enable[A]()
-- P$enable[A]() := A >-> STOP
-- with procInst = P[A](0)                // just the procInst of the LPE of lhs
testExit2 :: Test
testExit2 = TestCase $
   --trace ("\n\ntestExit2:\n expected:" ++ show (procInst', procDefs''') ++ "\ngot: " ++ show  (res_procInst, res_procDefs'')) $ 
       assertBool "EXIT ActionPref" (eqProcDefs procDefs''' res_procDefs'' && (procInst' ~~ res_procInst))
   where
      res_procDefs'' = Map.delete procIdP res_procDefs'  -- remove the original ProcId for the comparison with the expected result
      (res_procInst, res_procDefs') = preGNFEnableFunc procInst'' chanOffers emptyTranslatedProcDefs procDefs'
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] [ ]
          
      procDefP = ProcDef [chanIdA] [] (enable (actionPref actOfferExit stop)
                                              [] 
                                              (actionPref actOfferA stop))
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdP' = procIdGen "P" [chanIdA] [varIdPcP]
      procIDPenable = procIdGen "P$enable" [chanIdA] []

      procInst' = procInst procIdP' [chanIdA] [int0]
      procDefs''' = Map.fromList [   (procIdP', ProcDef [chanIdA] [varIdPcP] 
                                                (actionPref 
                                                      ActOffer {    offers = Set.empty
                                                                  , hiddenvars = Set.empty
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (procInst procIDPenable [chanIdA] []))),
                                    (procIDPenable, ProcDef [chanIdA] [] (actionPref actOfferA stop))
                        ]



-- P[A]() := EXIT ?x >>> ACCEPT ?id IN STOP NI
-- with procInst = P[A]()
-- becomes
-- P[A](pc$P) := {} [pc$P == 0] {EXIT$1} >-> P$enable[A](EXIT$1)
-- P$enable[A](id) := STOP
-- with procInst = P[A](0)
testExit3 :: Test
testExit3 = TestCase $
   -- trace ("\n\nACCEPT:\n expected:" ++ show (procInst', procDefs''') ++ "\ngot: " ++ show  (res_procInst, res_procDefs'')) $ 
       assertBool "ACCEPT" (eqProcDefs procDefs''' res_procDefs'' && (procInst' ~~ res_procInst))
   where
      res_procDefs'' = Map.delete procIdP res_procDefs'  -- remove the original ProcId for the comparison with the expected result
      (res_procInst, res_procDefs') = preGNFEnableFunc procInst'' chanOffers emptyTranslatedProcDefs procDefs'
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] [ ]
      
      -- action: EXIT ?x
      actOfferExitX :: ActOffer
      actOfferExitX   = ActOffer {  offers = Set.singleton
                                          Offer { chanid = chanIdExit
                                                , chanoffers = [Quest varIdX]
                                          }
                              , hiddenvars = Set.empty
                              , constraint = cstrConst (Cbool True)
                  }
            
      varIdid = VarId (T.pack "id") 33 intSort

      varIdExit1 = VarId (T.pack "EXIT$1") 122 intSort
      vexprExit1 = cstrVar varIdExit1

      procDefP = ProcDef [chanIdA] [] (enable (actionPref actOfferExitX stop)
                                              [Quest varIdid]
                                              stop)
      procDefs' = Map.fromList  [  (procIdP, procDefP)]

      procIdP' = procIdGen "P" [chanIdA] [varIdPcP]
      procIDPenable = procIdGen "P$enable" [chanIdA] [varIdid]

      procInst' = procInst procIdP' [chanIdA] [int0]
      procDefs''' = Map.fromList [   (procIdP', ProcDef [chanIdA] [varIdPcP] 
                                                (actionPref 
                                                      ActOffer {    offers = Set.empty
                                                                  , hiddenvars = Set.fromList [varIdExit1]
                                                                  , constraint = cstrEqual vexprPcP int0
                                                                  } 
                                                      (procInst procIDPenable [chanIdA] [vexprExit1]))),
                                    (procIDPenable, ProcDef [chanIdA] [varIdid] stop)
                        ]


        
        
----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testPreGNFEnableList :: Test
testPreGNFEnableList = TestList [  
                              TestLabel "simple EXIT" testExit1
                            , TestLabel "EXIT ActionPref" testExit2
                            , TestLabel "ACCEPT" testExit3

                        ]
