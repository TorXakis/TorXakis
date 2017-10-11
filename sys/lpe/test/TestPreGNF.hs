{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestPreGNF
(
testPreGNFList
)
where
import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map

-- import Debug.Trace as Trace

import TxsDefs
import TxsShow
import ProcId
import ChanId
import SortId
import qualified Data.Text         as T
import ValExprDefs

import PreGNF

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------
procIdP =  ProcId   {     ProcId.name       = T.pack "P"
                        , ProcId.unid       = 111
                        , ProcId.procchans  = []
                        , ProcId.procvars   = []
                        , ProcId.procexit   = NoExit
                    }
procIdP0 =  ProcId   {     ProcId.name       = T.pack "P$0"
                        , ProcId.unid       = 111
                        , ProcId.procchans  = []
                        , ProcId.procvars   = []
                        , ProcId.procexit   = NoExit
                    }
procIdP1 =  ProcId   {     ProcId.name       = T.pack "P$1"
                        , ProcId.unid       = 111
                        , ProcId.procchans  = []
                        , ProcId.procvars   = []
                        , ProcId.procexit   = NoExit
                    }
procIdP2 =  ProcId   {     ProcId.name       = T.pack "P$2"
    , ProcId.unid       = 111
    , ProcId.procchans  = []
    , ProcId.procvars   = []
    , ProcId.procexit   = NoExit
}
procIdP11 =  ProcId   {     ProcId.name       = T.pack "P$1$1"
                        , ProcId.unid       = 111
                        , ProcId.procchans  = []
                        , ProcId.procvars   = []
                        , ProcId.procexit   = NoExit
                    }
procIdQ =  ProcId   {     ProcId.name       = T.pack "Q"
                        , ProcId.unid       = 111
                        , ProcId.procchans  = []
                        , ProcId.procvars   = []
                        , ProcId.procexit   = NoExit
                    }
procIdQ0 =  ProcId   {    ProcId.name       = T.pack "Q$0"
                        , ProcId.unid       = 111
                        , ProcId.procchans  = []
                        , ProcId.procvars   = []
                        , ProcId.procexit   = NoExit
                    }
procIdQ1 =  ProcId   {    ProcId.name       = T.pack "Q$1"
    , ProcId.unid       = 111
    , ProcId.procchans  = []
    , ProcId.procvars   = []
    , ProcId.procexit   = NoExit
}

-- process instances P
procInstP = ProcInst 
                procIdP
                [chanIdA] -- [ChanId] channels
                [] -- [VExpr] arguments
     
-- process instances P0
procInstP0 = ProcInst 
                procIdP0
                [chanIdA] -- [ChanId] channels
                [] -- [VExpr] arguments
-- process instances P1
procInstP1 = ProcInst 
                procIdP1
                [chanIdA] -- [ChanId] channels
                [] -- [VExpr] arguments
-- process instances P1x
procInstP1x = ProcInst 
                procIdP1
                [chanIdA] -- [ChanId] channels
                [vexprX] -- [VExpr] arguments

-- process instances P1
procInstP2 = ProcInst 
                procIdP2
                [chanIdA] -- [ChanId] channels
                [] -- [VExpr] arguments
-- process instances P11
procInstP11 = ProcInst 
                procIdP11
                [chanIdA] -- [ChanId] channels
                [] -- [VExpr] arguments
-- process instances P11
procInstP11x = ProcInst 
                procIdP11
                [chanIdA] -- [ChanId] channels
                [vexprX] -- [VExpr] arguments
                                        
-- process instances Q
procInstQ = ProcInst 
                procIdQ
                [chanIdA] -- [ChanId] channels
                [] -- [VExpr] arguments
-- process instances Q0
procInstQ0 = ProcInst 
                procIdQ0
                [chanIdA] -- [ChanId] channels
                [] -- [VExpr] arguments
-- process instances Q1
procInstQ1 = ProcInst 
                procIdQ1
                [chanIdA] -- [ChanId] channels
                [] -- [VExpr] arguments

-- process instances Q1
procInstQ1x = ProcInst 
                procIdQ1
                [chanIdA] -- [ChanId] channels
                [vexprX] -- [VExpr] arguments

varIdX = VarId (T.pack "x") 33 intSort
varIdY = VarId (T.pack "y") 34 intSort
-- vexprX :: VExpr
vexprX = ValExpr $ Vvar varIdX
vexpr1 = cstrConst (Cint 1)


-- action: A
actOfferA   = ActOffer {  offers = Set.singleton( 
                                        Offer { chanid = chanIdA
                                              , chanoffers = []
                                        })
                        , constraint = cstrConst (Cbool True)
            }  
-- action: A1
actOfferA1   = ActOffer {  offers = Set.singleton( 
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Exclam vexpr1]
                                        })
                        , constraint = cstrConst (Cbool True)
            }  
-- action: A
actOfferAx   = ActOffer {  offers = Set.singleton( 
                                        Offer { chanid = chanIdA
                                              , chanoffers = [Quest varIdX]
                                        })
                        , constraint = cstrConst (Cbool True)
            }
           

-- action: B
actOfferB   = ActOffer {  offers = Set.singleton( 
                                        Offer { chanid = chanIdB
                                              , chanoffers = []
                                        })
                        , constraint = cstrConst (Cbool True)
            }  
            
-- BExpr:   
aSeqStop = ActionPref actOfferA Stop
aSeqBSeqStop = ActionPref actOfferA (ActionPref actOfferB Stop)
choice2 = Choice[Stop, Stop]
choice3 = Choice[Stop, Stop, Stop]

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
---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
-- Stop
testStop :: Test
testStop = TestCase $
    let procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [] Stop)] 
    in  assertEqual "STOP" procDefs (preGNF procIdP [] procDefs)

-- action prefix remains unchanged
testActPref :: Test
testActPref = TestCase $
   let procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [] aSeqStop)] 
   in  assertEqual "A >-> STOP" procDefs (preGNF procIdP [] procDefs)

testActPref2 :: Test
testActPref2 = TestCase $
   let procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [] aSeqBSeqStop)] 
   in  assertEqual "A >-> B >-> STOP" procDefs (preGNF procIdP [] procDefs)

-- action prefix is translated recursively
-- P[A]() = A >-> Q[]()
-- Q[A]() = A?x >-> (STOP ## STOP)
-- becomes
-- P[A]() = A >-> Q[A]()
-- Q[A]() = A?x >-> Q$1[A](x)
-- Q$1[A](x) = STOP ## STOP
testActPref3 :: Test
testActPref3 = TestCase $
   assertEqual "ActionPref is translated recursively" procDefs' (preGNF procIdP [] procDefs)
   where   
      bexprP = ActionPref actOfferA procInstQ
      bexprQ = ActionPref actOfferAx (Choice [Stop, Stop])
      
      bexprQ' = ActionPref actOfferAx procInstQ1x
      bexprQ1 = choice2

      procDefs = Map.fromList  [ (procIdP, ProcDef [chanIdA] [] bexprP) 
                               , (procIdQ, ProcDef [chanIdA] [] bexprQ) ]
      procDefs' = Map.fromList  [ (procIdP, ProcDef [chanIdA] [] bexprP)
                                , (procIdQ, ProcDef [chanIdA] [] bexprQ') 
                                , (procIdQ1, ProcDef [chanIdA] [varIdX] bexprQ1) ]
                          

-- process instance remains unchanged in preGNF                         
testProcInst :: Test
testProcInst = TestCase $
   let procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [] procInstP)] 
   in  assertEqual "P[]()" procDefs (preGNF procIdP [] procDefs)

-- choices at top-level remain unchanged                                  
testChoice1 :: Test
testChoice1 = TestCase $
   let procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [] choice2)] 
   in  assertEqual "P[]()" procDefs (preGNF procIdP [] procDefs)
            
-- choices at top-level remain unchanged                                  
testChoice2 :: Test
testChoice2 = TestCase $
   let procDefs = Map.fromList [(procIdP, ProcDef [chanIdA] [] choice3)] 
   in  assertEqual "P[]()" procDefs (preGNF procIdP [] procDefs)


-- choices at a lower level are substituted with a process instance to a 
-- process definition that is created for exactly the substituted term  
-- P[A]() = A >-> (STOP ## STOP) 
-- becomes
  -- P[A]()  = A >-> P$1[]()
  -- P$1[A]() = STOP ## STOP        
testChoice3 :: Test
testChoice3 = TestCase $
   assertEqual "choice (on lower level) is substituted" procDefs' (preGNF procIdP [] procDefs)
   where   
      bexpr = ActionPref actOfferA choice2
      bexpr' = ActionPref actOfferA procInstP1
      bexpr'' = choice2

      procDefs = Map.fromList  [ (procIdP, ProcDef [chanIdA] [] bexpr) ]
      procDefs' = Map.fromList  [ (procIdP, ProcDef [chanIdA] [] bexpr')
                                , (procIdP1, ProcDef [chanIdA] [] bexpr'') ]
       

-- choices at a lower level are substituted with a process instance to a 
-- process definition that is created for exactly the substituted term  
-- SAME AS ABOVE, only nested choice is second expression of a choice
-- P[A]() =     STOP
--      ## A >-> (STOP ## STOP) 
-- becomes
  -- P[A]() =     STOP 
  --      ## A >-> P$2[]()
  -- P$2[A]() = STOP ## STOP        
testChoice4 :: Test
testChoice4 = TestCase $
   assertEqual "choice (on lower level) is substituted 2" procDefs' (preGNF procIdP [] procDefs)
   where   
      bexpr = Choice [Stop, ActionPref actOfferA choice2]
      bexpr' = Choice [Stop, ActionPref actOfferA procInstP2]
      bexpr'' = choice2

      procDefs = Map.fromList  [ (procIdP, ProcDef [chanIdA] [] bexpr) ]
      procDefs' = Map.fromList  [ (procIdP, ProcDef [chanIdA] [] bexpr')
                                , (procIdP2, ProcDef [chanIdA] [] bexpr'') ]
       
 
-- -- choices nested two levels deep
-- -- P = (A >-> ((A >-> STOP ## STOP) ## STOP)) ## STOP 
-- -- or maybe more readable:
-- --   P =           
-- --                A
-- --          >->
-- --                          A
-- --                     >->
-- --                              STOP
-- --                          ##
-- --                              STOP
-- --                ##
-- --                     STOP
-- --    ##
-- --          STOP

-- -- becomes:
-- --  P[A]() = ( A >-> P$1[A]()  ) ## STOP
-- --  P$1[A]() = (A >-> P$1$1[A]()) ## STOP
-- -- P$1$1[A]() = STOP ## STOP
testChoice5 :: Test
testChoice5 = TestCase $
   assertEqual "choice (on lower level) is substituted 2" procDefs' (preGNF procIdP [] procDefs)
   where   
      bexpr = Choice [ActionPref actOfferA (
                          Choice [ ActionPref actOfferA (Choice [Stop,Stop]) , 
                                    Stop]), 
                      Stop]
      bexpr' = Choice [ActionPref actOfferA procInstP1,
                       Stop]
      bexpr'' = Choice [ActionPref actOfferA procInstP11, 
                        Stop]
      bexpr''' = Choice [Stop, Stop]

      procDefs = Map.fromList  [ (procIdP, ProcDef [chanIdA] [] bexpr) ]
      procDefs' = Map.fromList  [ (procIdP, ProcDef [chanIdA] [] bexpr')
                                , (procIdP1, ProcDef [chanIdA] [] bexpr'') 
                                , (procIdP11, ProcDef [chanIdA] [] bexpr''') ]



-- -- choices nested two levels deep WITH DATA
-- -- P = (A?x >-> ((A!1 >-> STOP ## STOP) ## STOP)) ## STOP 
-- -- or maybe more readable:
-- --   P =           
-- --                A
-- --          >->
-- --                          A
-- --                     >->
-- --                              STOP
-- --                          ##
-- --                              STOP
-- --                ##
-- --                     STOP
-- --    ##
-- --          STOP

-- -- becomes:
-- --  P[A]() = ( A?x >-> P$1[A](x)  ) ## STOP
-- --  P$1[A](x) = (A!1 >-> P$1$1[A](x)) ## STOP
-- -- P$1$1[A](x) = STOP ## STOP
testChoice5withData :: Test
testChoice5withData = TestCase $
   assertEqual "choice (on lower level) is substituted 2 with data" procDefs' (preGNF procIdP [] procDefs)
   where   
      bexpr = Choice [ActionPref actOfferAx (
                          Choice [ ActionPref actOfferA1 (Choice [Stop,Stop]) , 
                                    Stop]), 
                      Stop]
      bexpr' = Choice [ActionPref actOfferAx procInstP1x,
                       Stop]
      bexpr'' = Choice [ActionPref actOfferA1 procInstP11x, 
                        Stop]
      bexpr''' = Choice [Stop, Stop]

      procDefs = Map.fromList  [ (procIdP, ProcDef [chanIdA] [] bexpr) ]
      procDefs' = Map.fromList  [ (procIdP, ProcDef [chanIdA] [] bexpr')
                                , (procIdP1, ProcDef [chanIdA] [varIdX] bexpr'') 
                                , (procIdP11, ProcDef [chanIdA] [varIdX] bexpr''') ]
                          
                          
-- temporary tests

---- action: A
--actOfferAxy   = ActOffer {  offers = Set.singleton( 
--                                        Offer { chanid = chanIdA
--                                              , chanoffers = [Quest varIdX, Quest varIdY]
--                                        })
--                        , constraint = cstrConst (Cbool True)
--            }  

---- action: A
--actOfferAxy1   = ActOffer {  offers = Set.singleton( 
--                                        Offer { chanid = chanIdA
--                                              , chanoffers = [Quest varIdX, Exclam vexpr1, Quest varIdY, Exclam vexprX]
--                                        })
--                        , constraint = cstrConst (Cbool True)
--            }   

--testExtractVars :: Test
--testExtractVars = TestCase $
--   --assertEqual "extractVars" [varIdX] (extractVars actOfferAx)
--   --assertEqual "extractVars" [varIdX, varIdY] (extractVars actOfferAxy)
--   assertEqual "extractVars" [varIdX, varIdY] (extractVars actOfferAxy1)



----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testPreGNFList :: Test
testPreGNFList = TestList [ TestLabel "Stop is unchanged" testStop
                          , TestLabel "Single ActionPref is unchanged " testActPref
                          , TestLabel "Multi ActionPref is unchanged" testActPref2
                          , TestLabel "ActionPref is translated recursively" testActPref3
                          , TestLabel "ProcInst is unchanged" testProcInst
                          , TestLabel "Choice2 at top-level is unchanged" testChoice1
                          , TestLabel "Choice3 at top-level is unchanged" testChoice2
                          , TestLabel "choice at lower level is substituted" testChoice3
                          , TestLabel "choice at lower level is substituted 2" testChoice4
                          , TestLabel "choice at lower level is substituted (recursively)" testChoice5
                          , TestLabel "choice at lower level is substituted (recursively) with data" testChoice5withData
                          --, TestLabel "extractVars " testExtractVars
                         ]