{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TestConstElm
(
testConstElmBasic,
testConstElmXYX,
)
where

import Test.HUnit
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import TxsDefs
import ProcId
import ChanId
import SortId
import qualified Data.Text         as T
import VarId
import Constant
import ValExpr

import StdTDefs (stdSortTable)

import LPEConstElm
import TestUtils

{-# ANN module "HLint: ignore Reduce duplication" #-}

testConstElmBasic :: Test
testConstElmBasic = TestCase $ tryLPEOperation constElm model1 model2
  where
    summand1_1 :: LPESummand
    summand1_1 = newLPESummand -- A ? z [z==0] >-> P(1, 0)
        chanIdA [varIdZ]
        (cstrEqual vexprZ vexpr0)
        [(varIdX, vexpr1), (varIdY, vexpr0)]
    summand1_2 :: LPESummand
    summand1_2 = newLPESummand -- A ? z [x==1 && y==z] >-> P(0, y)
        chanIdA [varIdZ]
        (cstrAnd (Set.fromList [cstrEqual vexprX vexpr1, cstrEqual vexprY vexprZ]))
        [(varIdX, vexpr0), (varIdY, vexprY)]
    model1 :: LPE
    model1 = newLPE ([chanIdA], [(varIdX, vexpr0), (varIdY, vexpr0)], [summand1_1, summand1_2])
    
    summand2_1 :: LPESummand
    summand2_1 = newLPESummand -- A ? z [z==0] >-> P(1)
        chanIdA [varIdZ]
        (cstrEqual vexprZ vexpr0)
        [(varIdX, vexpr1)]
    summand2_2 :: LPESummand
    summand2_2 = newLPESummand -- A ? z [0==0 && 0==z] >-> P(0)
        chanIdA [varIdZ]
        (cstrAnd (Set.fromList [cstrEqual vexprX vexpr1, cstrEqual vexpr0 vexprZ]))
        [(varIdX, vexpr0)]
    model2 :: LPE
    model2 = newLPE ([chanIdA], [(varIdX, vexpr0)], [summand2_1, summand2_2])
-- testConstElmBasic

testConstElmXYX :: Test
testConstElmXYX = TestCase $ tryLPEOperation constElm model1 model2
  where
    summand1_1 :: LPESummand
    summand1_1 = newLPESummand -- A ? __FV1 [__FV1==z] >-> P(x, 1, z)
        chanIdA [varIdFV1]
        (cstrEqual vexprFV1 vexprZ)
        [(varIdX, vexprX), (varIdY, vexpr1), (varIdZ, vexprZ)]
    summand1_2 :: LPESummand
    summand1_2 = newLPESummand -- A ? __FV2 [__FV2==z] >-> P(y, x, __FV2)
        chanIdA [varIdFV2]
        (cstrEqual vexprFV2 vexprZ)
        [(varIdX, vexprY), (varIdY, vexprX), (varIdZ, vexprFV2)]
    summand1_3 :: LPESummand
    summand1_3 = newLPESummand -- A ? __FV3 [__FV3==z] >-> P(1, x, 2)
        chanIdA [varIdFV3]
        (cstrEqual vexprFV3 vexprZ)
        [(varIdX, vexpr1), (varIdY, vexprX), (varIdZ, vexpr2)]
    model1 :: LPE
    model1 = newLPE ([chanIdA], [(varIdX, vexpr1), (varIdY, vexpr1), (varIdZ, vexpr2)], [summand1_1, summand1_2, summand1_3])
    
    summand2_1 :: LPESummand
    summand2_1 = newLPESummand -- A ? __FV1 [__FV1==2] >-> P()
        chanIdA [varIdFV1]
        (cstrEqual vexprFV1 vexpr2)
        []
    summand2_2 :: LPESummand
    summand2_2 = newLPESummand -- A ? __FV2 [__FV2==2] >-> P()
        chanIdA [varIdFV2]
        (cstrEqual vexprFV2 vexpr2)
        []
    summand2_3 :: LPESummand
    summand2_3 = newLPESummand -- A ? __FV3 [__FV3==2] >-> P()
        chanIdA [varIdFV3]
        (cstrEqual vexprFV3 vexpr2)
        []
    model2 :: LPE
    model2 = newLPE ([chanIdA], [], [summand2_1, summand2_2, summand2_3])
-- testConstElmXYX

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

procIdGen :: String -> [ChanId] -> [VarId] -> ProcId
procIdGen name' chans vars' = ProcId   {  ProcId.name       = T.pack name'
                                        , ProcId.unid       = 111
                                        , ProcId.procchans  = toChanSort <$> chans
                                        , ProcId.procvars   = varsort <$> vars'
                                        , ProcId.procexit   = NoExit
                                    }
varIdX :: VarId
varIdX = VarId (T.pack "x") 33 intSort
varIdY :: VarId
varIdY = VarId (T.pack "y") 36 intSort
varIdZ :: VarId
varIdZ = VarId (T.pack "z") 36 intSort
varIdA1 :: VarId
varIdA1 = VarId (T.pack "A$1") 34 intSort
varIdB1 :: VarId
varIdB1 = VarId (T.pack "B$1") 34 intSort
varIdFV1 :: VarId
varIdFV1 = VarId (T.pack "__FV1") (-1) intSort
varIdFV2 :: VarId
varIdFV2 = VarId (T.pack "__FV2") (-2) intSort
varIdFV3 :: VarId
varIdFV3 = VarId (T.pack "__FV3") (-3) intSort

vexprX :: VExpr
vexprX = cstrVar varIdX
vexprY :: VExpr
vexprY = cstrVar varIdY
vexprZ :: VExpr
vexprZ = cstrVar varIdZ
vexprA1 :: VExpr
vexprA1 = cstrVar varIdA1
vexprB1 :: VExpr
vexprB1 = cstrVar varIdB1
vexprFV1 :: VExpr
vexprFV1 = cstrVar varIdFV1
vexprFV2 :: VExpr
vexprFV2 = cstrVar varIdFV2
vexprFV3 :: VExpr
vexprFV3 = cstrVar varIdFV3

vexpr0 :: VExpr
vexpr0 = cstrConst (Cint 0)
vexpr1 :: VExpr
vexpr1 = cstrConst (Cint 1)
vexpr2 :: VExpr
vexpr2 = cstrConst (Cint 2)
vexprMin1 :: VExpr
vexprMin1 = cstrConst (Cint (-1))
vexprTrue :: VExpr
vexprTrue = cstrConst (Cbool True)
vexprFalse :: VExpr
vexprFalse = cstrConst (Cbool False)

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

-- action: A?y
actOfferAy :: ActOffer
actOfferAy   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Quest varIdY]
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

-- action: B!x
actOfferBExclamX :: ActOffer
actOfferBExclamX   = ActOffer {  offers = Set.singleton
                              Offer { chanid = chanIdB
                                    , chanoffers = [Exclam vexprX]
                              }
                              , hiddenvars = Set.empty
                              , constraint = cstrConst (Cbool True)
            }

-- sorts, chanIds
intSort :: SortId
intSort = Maybe.fromMaybe (error "LPE module: could not find standard IntSort") (Map.lookup (T.pack "Int") stdSortTable)

chanIdA :: ChanId
chanIdA = ChanId    { ChanId.name = T.pack "A"
                    , ChanId.unid = 2
                    , ChanId.chansorts = [intSort]
                    }

chanIdA0 :: ChanId
chanIdA0 = ChanId    { ChanId.name = T.pack "A"
                        , ChanId.unid = 2
                        , ChanId.chansorts = []
}      

chanIdB :: ChanId
chanIdB = ChanId    { ChanId.name = T.pack "B"
                    , ChanId.unid = 3
                    , ChanId.chansorts = [intSort]
                    }
                  
anyInt :: VExpr
anyInt = cstrConst $ Cany intSort

