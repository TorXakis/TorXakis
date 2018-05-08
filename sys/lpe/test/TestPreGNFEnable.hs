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

module TestPreGNFEnable
(
testPreGNFEnableList
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

import StdTDefs (chanIdExit)
import Debug.Trace

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

-- runs preGNFEnable, but returns only the relevant translated ProcDef
preGNFEnableTestWrapper :: BExpr -> TranslatedProcDefs -> ProcDefs -> Maybe (BExpr, ProcDef)
preGNFEnableTestWrapper procInst'' translatedProcDefs procDefs =
  let (procInst'@(TxsDefs.view -> ProcInst procId' _ _), procDefs') = preGNFEnableFunc procInst'' chanOffers translatedProcDefs procDefs
      procDef' = case Map.lookup procId' procDefs' of
                    Just procDef   -> procDef
                    Nothing        -> error "preGNFEnableTestWrapper: could not find the procId" in
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
   trace ("\n\ntestExit1:\n expected:" ++ show (procInst', procDefs'') ++ "\ngot: " ++ show  (res_procInst, res_procDefs')) $ 
      assertBool "simple EXIT" ((eqProcDefs procDefs'' res_procDefs') && (procInst' ~~ res_procInst))
   where
      res_procDefs' = Map.delete procIdP res_procDefs  -- remove the original ProcId for the comparison with the expected result
      (res_procInst, res_procDefs) = (preGNFEnableFunc procInst'' chanOffers emptyTranslatedProcDefs procDefs)
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] [ ]
      
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
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      procIdP' = procIdGen "P" [chanIdA] [varIdPcP]
      procIDPenable = procIdGen "P$enable" [chanIdA] []

      procInst' = procInst procIdP' [chanIdA] [int0]
      procDefs'' = Map.fromList [(procIdP', ProcDef [chanIdA] [varIdPcP] 
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
   trace ("\n\ntestExit2:\n expected:" ++ show (procInst', procDefs'') ++ "\ngot: " ++ show  (res_procInst, res_procDefs')) $ 
       assertBool "EXIT ActionPref" ((eqProcDefs procDefs'' res_procDefs') && (procInst' ~~ res_procInst))
   where
      res_procDefs' = Map.delete procIdP res_procDefs  -- remove the original ProcId for the comparison with the expected result
      (res_procInst, res_procDefs) = (preGNFEnableFunc procInst'' chanOffers emptyTranslatedProcDefs procDefs)
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] [ ]
      
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
                                              (actionPref actOfferA stop))
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      procIdP' = procIdGen "P" [chanIdA] [varIdPcP]
      procIDPenable = procIdGen "P$enable" [chanIdA] []

      procInst' = procInst procIdP' [chanIdA] [int0]
      procDefs'' = Map.fromList [   (procIdP', ProcDef [chanIdA] [varIdPcP] 
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
   trace ("\n\nACCEPT:\n expected:" ++ show (procInst', procDefs'') ++ "\ngot: " ++ show  (res_procInst, res_procDefs')) $ 
       assertBool "ACCEPT" ((eqProcDefs procDefs'' res_procDefs') && (procInst' ~~ res_procInst))
   where
      res_procDefs' = Map.delete procIdP res_procDefs  -- remove the original ProcId for the comparison with the expected result
      (res_procInst, res_procDefs) = (preGNFEnableFunc procInst'' chanOffers emptyTranslatedProcDefs procDefs)
      procInst'' = procInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] [ ]
      
      -- action: EXIT ?x
      actOfferExit :: ActOffer
      actOfferExit   = ActOffer {  offers = Set.singleton
                                          Offer { chanid = chanIdExit
                                                , chanoffers = [Quest varIdX]
                                          }
                              , hiddenvars = Set.empty
                              , constraint = cstrConst (Cbool True)
                  }
            
      varIdid = VarId (T.pack "id") 33 intSort
      vexprid = cstrVar varIdid

      varIdExit1 = VarId (T.pack "EXIT$1") 122 intSort
      vexprExit1 = cstrVar varIdExit1

      procDefP = ProcDef [chanIdA] [] (enable (actionPref actOfferExit stop)
                                              [Quest varIdid]
                                              stop)
      procDefs = Map.fromList  [  (procIdP, procDefP)]

      procIdP' = procIdGen "P" [chanIdA] [varIdPcP]
      procIDPenable = procIdGen "P$enable" [chanIdA] [varIdid]

      procDefs3 = Map.fromList [(ProcId {ProcId.name = (T.pack "P"), 
                                    ProcId.unid = 111, 
                                    ProcId.procchans = [], 
                                    ProcId.procvars = [], 
                                    ProcId.procexit = NoExit},
                              (ProcDef [] [] stop))]
      
      procInst' = trace ("\n\n test result: " ++ show (eqProcDefs procDefs3 (Map.empty))) $ procInst procIdP' [chanIdA] [int0]
      procDefs'' = Map.fromList [   (procIdP', ProcDef [chanIdA] [varIdPcP] 
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
