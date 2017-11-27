{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestLPEHelpers
(
testLPEHelpersList
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
import VarId
import ConstDefs
import ValExpr

import qualified Data.Text         as T
import LPEHelpers
import TranslatedProcDefs

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

procIdGen :: String -> [ChanId] -> [VarId] -> ProcId
procIdGen name chans vars = ProcId   {    ProcId.name       = T.pack name
                                        , ProcId.unid       = 111
                                        , ProcId.procchans  = chans
                                        , ProcId.procvars   = vars
                                        , ProcId.procexit   = NoExit
                                    }

varIdX = VarId (T.pack "x") 33 intSort
varIdY = VarId (T.pack "y") 34 intSort
-- vexprX :: VExpr
vexprX = cstrVar varIdX
vexprY = cstrVar varIdY
vexpr1 = cstrConst (Cint 1)


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

-- action: A!x
actOfferAx   = ActOffer {  offers = Set.singleton(
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Quest varIdX]
                                        })
                        , constraint = cstrConst (Cbool True)
            }
-- action: A?xy
actOfferAxy   = ActOffer {  offers = Set.singleton(
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Quest varIdX, Quest varIdY]
                                        })
                        , constraint = cstrConst (Cbool True)
            }

-- action: A?xy1
actOfferAxy1   = ActOffer {  offers = Set.singleton(
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Quest varIdX, Exclam vexpr1, Quest varIdY, Exclam vexprX]
                                        })
                        , constraint = cstrConst (Cbool True)
            }

testExtractVars1 :: Test
testExtractVars1 = TestCase $
   assertEqual "extractVars" [varIdX] (extractVars actOfferAx)


testExtractVars2 :: Test
testExtractVars2 = TestCase $
   assertEqual "extractVars" [varIdX, varIdY] (extractVars actOfferAxy)

testExtractVars3 :: Test
testExtractVars3 = TestCase $
   assertEqual "extractVars" [varIdX, varIdY] (extractVars actOfferAxy1)



----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testLPEHelpersList :: Test
testLPEHelpersList = TestList [ TestLabel "extractVars " testExtractVars1
                          , TestLabel "extractVars 2" testExtractVars2
                          , TestLabel "extractVars 3" testExtractVars3
                         ]
