{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestDefinitions
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text         as T

import TxsDefs
import ProcId
import ChanId
import SortId
import VarId
import Constant
import ValExpr

import TxsShow

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


pshowProcDefs :: Map.Map ProcId ProcDef -> String
pshowProcDefs procDefs' = pshowProcDefs' $ Map.toList procDefs'
  where
      pshowProcDefs' :: [(ProcId, ProcDef)] -> String
      pshowProcDefs' [] = ""
      pshowProcDefs' ((procId, procDef'):rest) = "\n ** " ++ pshow procId ++ "\n" ++ pshow (DefProc procDef') ++ pshowProcDefs' rest


---------------------------------------------------------------------------
-- Predefined values
---------------------------------------------------------------------------
varIdX :: VarId
varIdX = VarId (T.pack "x") 33 sortIdInt
varIdY :: VarId
varIdY = VarId (T.pack "y") 34 sortIdInt
varIdZ :: VarId
varIdZ = VarId (T.pack "z") 35 sortIdInt
varIdS :: VarId
varIdS = VarId (T.pack "s") 36 sortIdInt
varIdA1 :: VarId
varIdA1 = VarId (T.pack "A$1") 40 sortIdInt
varIdB1 :: VarId
varIdB1 = VarId (T.pack "B$1") 41 sortIdInt

vexprX :: VExpr
vexprX = cstrVar varIdX
vexprY :: VExpr
vexprY = cstrVar varIdY
vexprZ :: VExpr
vexprZ = cstrVar varIdZ
vexprS :: VExpr
vexprS = cstrVar varIdS
vexprA1 :: VExpr
vexprA1 = cstrVar varIdA1
vexprB1 :: VExpr
vexprB1 = cstrVar varIdB1




varIdPpre1X :: VarId
varIdPpre1X = VarId (T.pack "P$pre1$x") 43 sortIdInt
varIdPpre1Y :: VarId
varIdPpre1Y = VarId (T.pack "P$pre1$y") 44 sortIdInt
varIdPpre1Z :: VarId
varIdPpre1Z = VarId (T.pack "P$pre1$z") 45 sortIdInt
varIdPgnf1X :: VarId
varIdPgnf1X = VarId (T.pack "P$gnf1$x") 46 sortIdInt
varIdPgnf1Y :: VarId
varIdPgnf1Y = VarId (T.pack "P$gnf1$y") 47 sortIdInt
varIdPgnf1Z :: VarId
varIdPgnf1Z = VarId (T.pack "P$gnf1$z") 48 sortIdInt

vexprPpre1X :: VExpr
vexprPpre1X = cstrVar varIdPpre1X
vexprPpre1Y :: VExpr
vexprPpre1Y = cstrVar varIdPpre1Y
vexprPpre1Z :: VExpr
vexprPpre1Z = cstrVar varIdPpre1Z
vexprPgnf1X :: VExpr
vexprPgnf1X = cstrVar varIdPgnf1X
vexprPgnf1Y :: VExpr
vexprPgnf1Y = cstrVar varIdPgnf1Y
vexprPgnf1Z :: VExpr
vexprPgnf1Z = cstrVar varIdPgnf1Z

int0 :: VExpr
int0 = cstrConst (Cint 0)
int1 :: VExpr
int1 = cstrConst (Cint 1)
int2 :: VExpr
int2 = cstrConst (Cint 2)
intMin1 :: VExpr
intMin1 = cstrConst (Cint (-1))

varIdPcP :: VarId
varIdPcP = VarId (T.pack "pc$P") 50 sortIdInt
vexprPcP :: VExpr
vexprPcP = cstrVar varIdPcP



varIdPdisable :: VarId
varIdPdisable = VarId (T.pack "P$disable$lhs") 53 sortIdInt
varIdPpcLHS :: VarId
varIdPpcLHS = VarId (T.pack "P$lhs$pc$P$lhs") 54 sortIdInt
varIdPpcRHS :: VarId
varIdPpcRHS = VarId (T.pack "P$rhs$pc$P$rhs") 55 sortIdInt
varIdPpcInterruptLHS :: VarId
varIdPpcInterruptLHS = VarId (T.pack "P$lhs$pc$P$interrupt$lhs") 56 sortIdInt
varIdPpcInterruptRHS :: VarId
varIdPpcInterruptRHS = VarId (T.pack "P$rhs$pc$P$interrupt$rhs") 57 sortIdInt

vexprPdisable :: VExpr
vexprPdisable = cstrVar varIdPdisable
vexprPpcLHS :: VExpr
vexprPpcLHS = cstrVar varIdPpcLHS
vexprPpcRHS :: VExpr
vexprPpcRHS = cstrVar varIdPpcRHS
vexprPpcInterruptLHS :: VExpr
vexprPpcInterruptLHS = cstrVar varIdPpcInterruptLHS
vexprPpcInterruptRHS :: VExpr
vexprPpcInterruptRHS = cstrVar varIdPpcInterruptRHS




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
                                              , chanoffers = [Exclam int1]
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

-- action: A?A$1
actOfferAA1 :: ActOffer
actOfferAA1   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Quest varIdA1]
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

-- action: B
actOfferB :: ActOffer
actOfferB = ActOffer {  offers = Set.singleton
                                    Offer { chanid = chanIdB0
                                          , chanoffers = []
                                    }
                     , hiddenvars = Set.empty
                     , constraint = cstrConst (Cbool True)
                     }

-- action: B!1
actOfferB1 :: ActOffer
actOfferB1 = ActOffer {  offers = Set.singleton
                                    Offer { chanid = chanIdB
                                          , chanoffers = [Exclam int1]
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

-- action: B?B$1
actOfferBB1 :: ActOffer
actOfferBB1  = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Quest varIdB1]
                                        }
                        , hiddenvars = Set.empty
                        , constraint = cstrConst (Cbool True)
            }

-- action: B?y
actOfferBy :: ActOffer
actOfferBy   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdB
                                              , chanoffers = [Quest varIdY]
                                        }
                        , hiddenvars = Set.empty
                        , constraint = cstrConst (Cbool True)
            }

-- action: A|B    // no chanoffers!
actOfferAB :: ActOffer
actOfferAB   = ActOffer {  offers = Set.fromList [
                                      Offer { chanid = chanIdA0
                                            , chanoffers = []
                                      },
                                      Offer { chanid = chanIdB0
                                            , chanoffers = []
                                      }
                                    ]
                        , hiddenvars = Set.empty
                        , constraint = cstrConst (Cbool True)
                        }

-- action: C
actOfferC :: ActOffer
actOfferC   = ActOffer {  offers = Set.singleton
                                        Offer { chanid = chanIdC0
                                              , chanoffers = []
                                        }
                        , hiddenvars = Set.empty
                        , constraint = cstrConst (Cbool True)
            }


-- action: EXIT
actOfferExit :: ActOffer
actOfferExit   = ActOffer {  offers = Set.singleton
                                    Offer { chanid = chanIdExit
                                          , chanoffers = []
                                    }
                        , hiddenvars = Set.empty
                        , constraint = cstrConst (Cbool True)
            }

-- sorts, chanIds
chanIdA0 :: ChanId
chanIdA0 = ChanId    { ChanId.name = T.pack "A"
                     , ChanId.unid = 2
                     , ChanId.chansorts = []
                     }

chanIdA :: ChanId
chanIdA = ChanId    { ChanId.name = T.pack "A"
                    , ChanId.unid = 2
                    , ChanId.chansorts = [sortIdInt]
                    }

chanIdB0 :: ChanId
chanIdB0 = ChanId    { ChanId.name = T.pack "B"
                     , ChanId.unid = 3
                     , ChanId.chansorts = []
                     }

chanIdB :: ChanId
chanIdB = ChanId    { ChanId.name = T.pack "B"
                    , ChanId.unid = 4
                    , ChanId.chansorts = [sortIdInt]
                    }

chanIdC0 :: ChanId
chanIdC0 = ChanId   { ChanId.name = T.pack "C"
                    , ChanId.unid = 5
                    , ChanId.chansorts = []
                    }

chanIdC :: ChanId
chanIdC = ChanId    { ChanId.name = T.pack "C"
                    , ChanId.unid = 6
                    , ChanId.chansorts = [sortIdInt]
                    }

anyInt :: VExpr
anyInt = cstrConst $ Cany sortIdInt

chanOffers :: Map.Map (T.Text, Int) VarId
chanOffers = Map.fromList [ ((T.pack "A", 1), varIdA1)
                          , ((T.pack "B", 1), varIdB1)
                          ]
