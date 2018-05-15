{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestBExpr
(
testBExprList
)
where
import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map

-- import Debug.Trace as Trace

import ConstDefs
import StdTDefs
import TxsDefs
import TxsShow
import ValExpr

import TestHelperFuncContent

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

definedChannel1 :: String
definedChannel1 = "Aap"

definedChannel1SortName :: String
definedChannel1SortName = intSortName

definedChannel2 :: String
definedChannel2 = "Noot"

definedChannel2SortName :: String
definedChannel2SortName = intSortName

definedChannel3 :: String
definedChannel3 = "Mies"

definedChannel3SortName :: String
definedChannel3SortName = stringSortName

definedChannels :: TypedElements
definedChannels = [([definedChannel1SortName], definedChannel1),
                   ([definedChannel2SortName], definedChannel2),
                   ([definedChannel3SortName], definedChannel3)]
         
                
definedInt1 :: String
definedInt1 = "x"

definedInt2 :: String
definedInt2 = "y"

definedInt3 :: String
definedInt3 = "z"

definedString1 :: String
definedString1 = "k"

definedString2 :: String
definedString2 = "l"

definedString3 :: String
definedString3 = "m"

definedBool1 :: String
definedBool1 = "a"

definedVars :: TypedElements
definedVars = [ ([definedInt1, definedInt2, definedInt3], intSortName)
              , ([definedString1, definedString2, definedString3], stringSortName)
              , ([definedBool1], boolSortName)
              ]


definedProcDef :: String
definedProcDef = "proc"

parseBexpr :: Maybe [String] -> BExpr -> BExpr
parseBexpr definedExits content =
    let
        file :: String
        file = createProcDef definedProcDef definedChannels definedVars definedExits content
        pid :: ProcId
        pid = expectProcId definedProcDef definedChannels definedVars definedExits
      in
        -- Trace.trace ("file = \n" ++ file) $
                    case findValueOfGenericKey (IdProc pid) ( parseTorXakis file ) of
                         Just (DefProc (ProcDef _definedChannels' _definedVars' bexp)) -> bexp
                         Nothing                                                       -> error $ "Test Error: process\n" ++ show pid ++ "\nnot found in \n" ++ show ( Map.keys (procDefs (parseTorXakis file) ) )
                         Just _another                                                 -> error "Test Error: process not a ProcDef"
                    
---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
    -- can ALLGATES be entered as Channel name? If so, how to prevent this ; how to test it is ok?

-- Stop
testStop :: Test
testStop = TestCase $
    let bexpr :: BExpr
        bexpr = stop
        actual :: BExpr
        actual = parseBexpr Nothing bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual

-- Exit
testExit :: Test
testExit = TestCase $
    let bexpr :: BExpr
        bexpr = actionPref (ActOffer (Set.singleton(Offer (expectChanId "EXIT" []) [])) Set.empty (cstrConst (Cbool True)) ) stop 
        actual :: BExpr
        actual = parseBexpr (Just []) bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual

-- ExitValue
testExitValue :: Test
testExitValue = TestCase $
    let bexpr :: BExpr
        bexpr = actionPref (ActOffer (Set.singleton(Offer (expectChanId "EXIT" [intSortName]) [Exclam (cstrConst (Cint 8978))])) Set.empty (cstrConst (Cbool True)) ) stop 
        actual :: BExpr
        actual = parseBexpr (Just [intSortName]) bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual
           
-- ActionPref 
testExclam :: Test
testExclam = TestCase $
    let value = 10
        bexpr :: BExpr
        bexpr = actionPref (ActOffer (Set.singleton(Offer (expectChanId definedChannel1 [definedChannel1SortName]) [Exclam (cstrConst (Cint value))])) Set.empty (cstrConst (Cbool True))) stop
        actual :: BExpr
        actual = parseBexpr Nothing bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual

testExclamArgument :: Test
testExclamArgument = TestCase $
    let bexpr :: BExpr
        bexpr = actionPref (ActOffer (Set.singleton(Offer (expectChanId definedChannel1 [definedChannel1SortName]) [Exclam (cstrVar (expectVarId definedInt1 intSortName))])) Set.empty (cstrConst (Cbool True))) stop
        actual :: BExpr
        actual = parseBexpr Nothing bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual

testQuest :: Test
testQuest = TestCase $
    let varName = "v"
        varSortName = definedChannel3SortName
        bexpr :: BExpr
        bexpr = actionPref (ActOffer (Set.singleton (Offer (expectChanId definedChannel3 [definedChannel3SortName]) [Quest (expectVarId varName varSortName)])) Set.empty (cstrConst (Cbool True))) stop
        actual :: BExpr
        actual = parseBexpr Nothing bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual  
        
testQuestScope :: Test
testQuestScope = TestCase $
    let bexpr :: BExpr
        bexpr = actionPref (ActOffer (Set.singleton (Offer (expectChanId definedChannel2 [definedChannel2SortName]) [Quest (expectVarId definedInt1 intSortName)])) Set.empty (cstrConst (Cbool True))) stop
        actual :: BExpr
        actual = parseBexpr Nothing bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual
        
-- Exit & Processes to use
aDefinedExit :: Maybe [String]
aDefinedExit = Just [intSortName, stringSortName]
        
aBExpr :: BExpr
aBExpr = actionPref (ActOffer (Set.singleton(Offer (expectChanId definedChannel1 [definedChannel1SortName]) [Exclam (cstrConst (Cint 123))])) Set.empty (cstrConst (Cbool True)) )
          (actionPref (ActOffer (Set.singleton(Offer (expectChanId "EXIT" [ intSortName, stringSortName]) 
                                                                          [ Exclam (cstrVar (expectVarId definedInt1 intSortName))
                                                                          , Exclam (cstrVar (expectVarId definedString3 stringSortName))]) ) Set.empty (cstrConst (Cbool True)) ) 
           stop)

anotherBExpr :: BExpr
anotherBExpr = guard (cstrVar (expectVarId definedBool1 boolSortName))
                     (actionPref (ActOffer (Set.singleton(Offer (expectChanId "EXIT"  [ intSortName, stringSortName])
                                                                                      [ Exclam (cstrVar (expectVarId definedInt2 intSortName))
                                                                                      , Exclam (cstrVar (expectVarId definedString2 stringSortName))]) ) Set.empty (cstrConst (Cbool True)) ) 
                      stop)


-- Guard
testGuard :: Test
testGuard = TestCase $
    let bexpr :: BExpr
        bexpr = guard (cstrVar (expectVarId definedBool1 boolSortName)) aBExpr
        actual :: BExpr
        actual = parseBexpr aDefinedExit bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual

-- Choice
testChoice :: Test
testChoice = TestCase $
    let bexpr :: BExpr
        bexpr = choice [aBExpr, anotherBExpr]
        actual :: BExpr
        actual = parseBexpr aDefinedExit bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual
        
--Parallel
testSynchronization :: Test
testSynchronization = TestCase $
    let chans :: TypedElements
        chans = definedChannels
        bexpr :: BExpr
        bexpr = parallel (chanIdExit:map (\(s,n) -> expectChanId n s) chans) [aBExpr, anotherBExpr]
        actual :: BExpr
        actual = parseBexpr aDefinedExit bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual

testInterleaving :: Test
testInterleaving = TestCase $
    let chans :: TypedElements
        chans = []
        bexpr :: BExpr
        bexpr = parallel (chanIdExit:map (\(s,n) -> expectChanId n s) chans) [aBExpr, anotherBExpr]
        actual :: BExpr
        actual = parseBexpr aDefinedExit bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual

testCommunicate :: Test
testCommunicate = TestCase $
    let chans :: TypedElements
        chans = [([definedChannel1SortName], definedChannel1)]
        bexpr :: BExpr
        bexpr = parallel (chanIdExit:map (\(s,n) -> expectChanId n s) chans) [aBExpr, anotherBExpr]
        actual :: BExpr
        actual = parseBexpr aDefinedExit bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual

-- Enable
testEnable :: Test
testEnable = TestCase $
    let bexpr :: BExpr
        bexpr = enable (actionPref (ActOffer (Set.singleton(Offer (expectChanId "EXIT" []) [])) Set.empty (cstrConst (Cbool True)) ) stop) 
                       [] 
                       anotherBExpr
        actual :: BExpr
        actual = parseBexpr aDefinedExit bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual

testEnableCommunicate :: Test
testEnableCommunicate = TestCase $
    let bexpr :: BExpr
        bexpr = enable aBExpr 
                       [ Quest (expectVarId "t1" intSortName)
                       , Quest (expectVarId "t2" stringSortName)
                       ] 
                       anotherBExpr
        actual :: BExpr
        actual = parseBexpr aDefinedExit bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual

-- Disable
testDisable :: Test
testDisable = TestCase $
    let bexpr :: BExpr
        bexpr = disable aBExpr anotherBExpr
        actual :: BExpr
        actual = parseBexpr aDefinedExit bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual

-- ProcInst
testProcInst :: Test
testProcInst = TestCase $
    let pid :: ProcId
        pid = expectProcId definedProcDef definedChannels definedVars Nothing

        bexpr :: BExpr
        bexpr = procInst pid (fromTypedElementsToChanIds definedChannels) (fromTypedElementsToVExprs definedVars)
        actual :: BExpr
        actual = parseBexpr Nothing bexpr
      in
        assertBool ("expected =\n" ++ pshow bexpr ++ "\nactual =\n" ++ pshow actual) $ identicalBExpr bexpr actual

----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testBExprList :: Test
testBExprList = TestList [ TestLabel "Stop" testStop,
                           TestLabel "Exit" testExit,
                           TestLabel "ExitValue" testExitValue,
                           TestLabel "ActionPref Exclam" testExclam,
                           TestLabel "ActionPref Exclam Argument" testExclamArgument,
                           TestLabel "ActionPref Quest" testQuest,
                           TestLabel "ActionPref Quest Scope" testQuestScope,
                           TestLabel "Guard" testGuard,
                           TestLabel "Choice" testChoice,
                           TestLabel "Parallel Synchronization" testSynchronization,
                           TestLabel "Parallel Interleaving" testInterleaving,
                           TestLabel "Parallel Communicate" testCommunicate,
                           TestLabel "Enable" testEnable,
                           TestLabel "Enable Communicate" testEnableCommunicate,
                           TestLabel "Disable" testDisable,
                           TestLabel "ProcInst" testProcInst   
                         ]
