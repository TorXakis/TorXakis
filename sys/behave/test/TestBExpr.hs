{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

module TestBExpr
(
testBExprList
)
where
import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map

import Debug.Trace as Trace

import TxsAlex
import TxsHappy
import TxsDefs
import TxsShow

import TestHelper

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
		 
chanExit :: TypedElement
chanExit = ([], "EXIT")
				 
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

definedVars :: TypedElements
definedVars = [([definedInt1, definedInt2, definedInt3], intSortName),
			   ([definedString1, definedString2, definedString3], stringSortName)]


definedProcDef :: String
definedProcDef = "proc"

definedExits :: Maybe [String]
definedExits = Just []

parseBexpr :: BExpr -> BExpr
parseBexpr content =
	let
		file :: String
		file = createProcDef definedProcDef definedChannels definedVars definedExits content
		id :: ProcId
		id = expectProcId definedProcDef definedChannels definedVars definedExits
	 in do
	 {
		Trace.trace ("file = \n" ++ file) 
					(case findValueOfGenericKey id ( parseTorXakis file ) of		-- to do: idMap / sig 
						Just (ProcDef definedChannels definedVars bexp) 	-> bexp
						Nothing             								-> error ("Test Error: process name not found" ++ (show ( Map.keys (parseTorXakis file ))))
						Just another        								-> error "Test Error: process name not a ProcDef" -- ++ (show ( parseTorXakis file )))
					)
	 }
---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
	-- can ALLGATES be entered as Channel name? If so, how to prevent this ; how to test it is ok?

aBExpr :: BExpr
aBExpr = (ActionPref (ActOffer (Set.singleton(Offer (expectChanId definedChannel1 [definedChannel1SortName]) [Exclam (Vconst (Cint 123))])) [] )
          (ActionPref (ActOffer (Set.singleton(Offer (expectChanId "EXIT" []) [])) [] ) 
           Stop)
          )   

anotherBExpr :: BExpr
anotherBExpr = (Guard [Vvar (expectVarId definedString1 stringSortName)] Stop) 

-- Stop
testStop :: Test
testStop = TestCase $ do
	let
		bexpr :: BExpr
		bexpr = Stop
	 in do
		assertEqual ("equal\n" ++ (pshow bexpr))  bexpr (parseBexpr bexpr)

-- ActionPref 
testExclam :: Test
testExclam = TestCase $ do
	let
		value = 10
		bexpr :: BExpr
		bexpr = (ActionPref (ActOffer (Set.singleton(Offer (expectChanId definedChannel1 [definedChannel1SortName]) [Exclam (Vconst (Cint value))])) Set.empty Set.empty) Stop) 
	  in do
		assertEqual ("equal\n" ++ (pshow bexpr))  bexpr (parseBexpr bexpr)

testExclamArgument :: Test
testExclamArgument = TestCase $ do
	let
		bexpr :: BExpr
		bexpr = (ActionPref (ActOffer (Set.singleton(Offer (expectChanId definedChannel1 [definedChannel1SortName]) [Exclam (Vvar (expectVarId definedInt1 intSortName))])) Set.empty Set.empty) Stop) 
		
	  in do
		assertEqual ("equal\n" ++ (pshow bexpr))  bexpr (parseBexpr bexpr)

testQuest :: Test
testQuest = TestCase $ do
	let
		varName = "v"
		varSortName = definedChannel3SortName
		bexpr :: BExpr
		bexpr = (ActionPref (ActOffer (Set.singleton(Offer (expectChanId definedChannel3 [definedChannel3SortName]) [Quest (expectVarId varName varSortName)])) Set.empty Set.empty) Stop) 
  	  in do
		assertEqual ("equal\n" ++ (pshow bexpr))  bexpr (parseBexpr bexpr)	
		
testQuestScope :: Test
testQuestScope = TestCase $ do
	let
		bexpr :: BExpr
		bexpr = (ActionPref (ActOffer (Set.singleton(Offer (expectChanId definedChannel2 [definedChannel2SortName]) [Quest (expectVarId definedInt1 intSortName)])) Set.empty Set.empty) Stop) 
	  in do
		assertEqual ("equal\n" ++ (pshow bexpr))  bexpr (parseBexpr bexpr)
		-- TODO: is x really overwritten/shadowed?

-- Guard
testGuard :: Test
testGuard = TestCase $ do
	let 
		bexpr :: BExpr
		bexpr = (Guard (Set.fromList [Vvar (expectVarId definedString1 stringSortName)]) Stop) 
	  in do
		assertEqual ("equal\n" ++ (pshow bexpr))  bexpr (parseBexpr bexpr)

-- Choice
testChoice :: Test
testChoice = TestCase $ do
	let
		bexpr :: BExpr
		bexpr = Choice (Set.fromList [aBExpr, anotherBExpr])
	  in do
		assertEqual ("equal\n" ++ (pshow bexpr)) bexpr (parseBexpr bexpr)
		
--Parallel
testSynchronization :: Test
testSynchronization = TestCase $ do
	let 
		chans = definedChannels
		
		bexpr :: BExpr
		bexpr = (Parallel (Set.fromList (map (\(s,n) -> expectChanId n s) chans)) aBExpr anotherBExpr )

		-- Add EXIT to expected structure
		bexprExpect :: BExpr
		bexprExpect = (Parallel (Set.fromList (map (\(s,n) -> expectChanId n s) (chans++[chanExit]))) aBExpr anotherBExpr )
	  in do
		assertEqual ("equal\n" ++ (pshow bexpr))  bexprExpect (parseBexpr bexpr)
			
testInterleaving :: Test
testInterleaving = TestCase $ do
	let 
		chans = [] 
		
		bexpr :: BExpr
		bexpr = (Parallel (Set.fromList (map (\(s,n) -> expectChanId n s) chans)) aBExpr anotherBExpr )

		-- Add EXIT to expected structure
		bexprExpect :: BExpr
		bexprExpect = (Parallel (Set.fromList (map (\(s,n) -> expectChanId n s) (chans++[chanExit]))) aBExpr anotherBExpr )
	  in do
		assertEqual ("equal\n" ++ (pshow bexpr))  bexprExpect (parseBexpr bexpr)

testCommunicate :: Test
testCommunicate = TestCase $ do
	let 
		chans = [([definedChannel1SortName], definedChannel1)] 
		
		bexpr :: BExpr
		bexpr = (Parallel (Set.fromList (map (\(s,n) -> expectChanId n s) chans)) aBExpr anotherBExpr )

		-- Add EXIT to expected structure
		bexprExpect :: BExpr
		bexprExpect = (Parallel (Set.fromList (map (\(s,n) -> expectChanId n s) (chans++[chanExit]))) aBExpr anotherBExpr )
	  in do
		assertEqual ("equal\n" ++ (pshow bexpr))  bexprExpect (parseBexpr bexpr)

-- Enable
testEnable :: Test
testEnable = TestCase $ do
	let 
		bexpr :: BExpr
		bexpr = (Enable aBExpr [] anotherBExpr)
	  in do
		assertEqual ("equal\n" ++ (pshow bexpr))   bexpr (parseBexpr bexpr)

testEnableCommunicate :: Test
testEnableCommunicate = TestCase $ do
	let 
		bexpr :: BExpr
		bexpr = (Enable aBExpr [Exclam (Vvar (expectVarId definedString2 stringSortName))] anotherBExpr)
	  in do
		assertEqual ("equal\n" ++ (pshow bexpr)) bexpr (parseBexpr bexpr)

-- Disable
testDisable :: Test
testDisable = TestCase $ do
	let 
		bexpr :: BExpr
		bexpr = (Disable aBExpr anotherBExpr)
	  in do
		assertEqual ("equal\n" ++ (pshow bexpr))   bexpr (parseBexpr bexpr)

-- ProcInst
testProcInst :: Test
testProcInst = TestCase $ do
	let 
		id :: ProcId
		id = expectProcId definedProcDef definedChannels definedVars Nothing

		bexpr :: BExpr
		bexpr = ProcInst id (fromTypedElementsToChanIds definedChannels) (fromTypedElementsToVExprs definedVars)  
	  in do
		assertEqual ("equal\n" ++ (pshow bexpr))   bexpr (parseBexpr bexpr)

----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testBExprList = TestList [	TestLabel "Stop" testStop,
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
							TestLabel "Enable Communicate" testEnableCommunicate, -- TODO: Jan please fix this failing test
							TestLabel "Disable" testDisable,
							TestLabel "ProcInst" testProcInst	
						 ]
