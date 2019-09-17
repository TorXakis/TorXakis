{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SortContextSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'SortContext' elements.
-----------------------------------------------------------------------------
module TorXakis.SortContextSpec
(spec
)
where
import           Control.Monad.Except
import           Control.Monad.State
import           Data.HashMap
import           Data.Text
import           Debug.Trace
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           TorXakis.ContextTestSort
import           TorXakis.Name
import           TorXakis.ProblemSolver
--import           TorXakis.RandomSolver
import           TorXakis.SmtM
import           TorXakis.Sort
--import           TorXakis.SymbolicSolver
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.Var

import           TorXakis.SortGenContext
import           TorXakis.TestSolvers

constructableADTs :: ProblemSolver p => [ADTDef] -> p Bool
constructableADTs ads = do
    TorXakis.ProblemSolver.addADTs ads
    bs <- mapM constructableADT ads
    return $ and bs

constructableADT :: ProblemSolver p => ADTDef -> p Bool
constructableADT ad = do
    _ <- push
    ctx <- toValExprContext
    let ar = RefByName (adtName ad)
        Right nm = mkName (Data.Text.pack "var")
        Right vd = mkVarDef ctx nm (SortADT ar)
    declareVariables [vd]
    bs <- mapM (constructableADTByConstructor (RefByName (TorXakis.Var.name vd)) ar . RefByName . constructorName) (elemsConstructor ad)
    _ <- pop
    return ( and bs )

constructableADTByConstructor :: ProblemSolver p => RefByName VarDef -> RefByName ADTDef -> RefByName ConstructorDef -> p Bool
constructableADTByConstructor vr ar cr = do
    i <- info
    ctx <- toValExprContext
    _ <- push
    let Right varExpr = mkVar ctx vr
        Right expr = mkIsCstr ctx ar cr varExpr
    addAssertions [expr]
    resp <- solve
    _ <- pop
    let Solved (Solution solution) = resp
        Right answer = subst ctx (Data.HashMap.map (\v -> case mkConst ctx v of
                                                               Right ve -> ve
                                                               Left e   -> error ("mkConst failed unexpectedly on constant " ++ show v ++ " with error " ++ show e)
                                                   )
                                                   solution
                                  )
                                  expr
    case view answer of
        Vconst (Cbool b) -> return ( b || trace (i ++ "\nUnexpectedly it is impossible to make an instance for the ADT (" ++ show ar ++ ") with constructor (" ++ show cr ++ ")") False)
        _                -> error ("Answer of and is unexpectedly not a constant boolean, but " ++ show answer)

-- | Data types are constructable
prop_Constructable :: Property
prop_Constructable = monadicIO $ do
        bs <- mapM ( forAllM (arbitraryADTDefs TorXakis.ContextTestSort.empty) . prop_Constructable_Solver )
                   [cmdZ3]  -- CVC4 too slow for unit tests, see https://github.com/CVC4/CVC4/issues/3266
        return (and bs)
    where
        prop_Constructable_Solver :: (FilePath,[String]) -> [ADTDef] -> PropertyM IO Bool
        prop_Constructable_Solver (fp,as) ads = do
            es <- liftIO $ mkSmtState fp as True
            case es of
                Left err -> error (show err)
                Right ss -> do
                            r <- liftIO $ runExceptT $ -- smt Solver
                                              runStateT (TorXakis.SmtM.toStateT
                                                                                 -- random Solver
                                                                                 -- (evalStateT (TorXakis.RandomSolver.toStateT 
                                                                                                    -- symbolic solver
                                                                                                    -- (evalStateT (TorXakis.SymbolicSolver.toStateT 
                                                                                                                    (constructableADTs ads)
                                                                                                    --             )
                                                                                                    --             mkSymbolicState
                                                                                                    -- )
                                                                                             -- )
                                                                                             -- (mkRandomState 3 10 Factor)
                                                                                 -- )
                                                         )
                                                         ss
                            case r of
                                Left err  -> error (show err)
                                Right (b,ss') -> do
                                                me <- liftIO $ destroySmtState ss'
                                                case me of
                                                    Just err -> error (show err)
                                                    Nothing  -> return b


spec :: Spec
spec =
  describe "All data types of a sort context" $
           modifyMaxSuccess (const 1000000) $  it "are constructable" $ property prop_Constructable
