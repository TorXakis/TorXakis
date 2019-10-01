{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RegexSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'Regex'.
-----------------------------------------------------------------------------
module TorXakis.RegexSpec
(spec
)
where
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Char
import qualified Data.Text
import           Debug.Trace
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Name
import           TorXakis.ProblemSolver
import           TorXakis.Regex
import           TorXakis.RegexGen
import           TorXakis.SmtM
import           TorXakis.Sort
import           TorXakis.StringFromRegex
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.Var

import           TorXakis.TestSolvers

-- | run All solvers
runSolvers :: SmtM Bool -> Expectation
runSolvers exec = do
        bs <- liftIO $ mapM (runSolver exec) defaultSMTProcs
        bs `shouldSatisfy` and

-- | run specific solver
runSolver :: SmtM Bool -> (FilePath,[String]) -> IO Bool
runSolver exec (fp,as) = do
    es <- liftIO $ mkSmtState fp as False
    case es of
        Left err -> error (show err)
        Right ss -> do
                    res <- liftIO $ runExceptT $ -- smt Solver
                                      runStateT (TorXakis.SmtM.toStateT exec)
                                                 ss
                    case res of
                        Left err  -> error (show err)
                        Right (b,ss') -> do
                                        me <- liftIO $ destroySmtState ss'
                                        case me of
                                            Just err -> error (show err)
                                            Nothing  -> return b

-- | all Char in Regex Range
allChars :: String
allChars = [regexRangeLow..regexRangeHigh]

-- | Are all chars correctly handled (by escaping when needed)
expectationChars :: Expectation
expectationChars = runSolvers instanceChars
    where
        instanceChars :: ProblemSolver p => p Bool
        instanceChars = and <$> mapM instanceChar allChars
            
        instanceChar :: ProblemSolver p => Char -> p Bool
        instanceChar c = do
            ctx <- toValExprContext
            _ <- push
            let Right nm = mkName (Data.Text.pack "var")
                Right varDecl = mkVarDef ctx nm SortString
                Right val = mkConst ctx (mkString (Data.Text.singleton c))
              in do
                declareVariables[varDecl]
                ctx' <- toValExprContext
                let Right var = mkVar ctx' (RefByName nm)
                    Right varIsVal = mkEqual ctx' var val
                    Right r = mkRegexCharLiteral c
                    Right varInRe = mkStrInRe ctx' var r
                  in do
                    addAssertions [varIsVal, varInRe]
                    res <- solvable
                    _ <- pop
                    case toMaybeBool res of
                        Nothing -> error ("Problem solver can determine " ++ show c ++ " is in regex of " ++ show c)
                        Just b  -> return b

-- | stringFromRegex
prop_StringFromRegex :: RegexGen -> Expectation
prop_StringFromRegex (RegexGen r) = runSolvers instanceStringFromRegex
    where
        instanceStringFromRegex :: ProblemSolver p => p Bool
        instanceStringFromRegex = do
            s <- liftIO $ stringFromRegex r
            _ <- push
            ctx <- toValExprContext
            let Right nm = mkName (Data.Text.pack "var")
                Right varDecl = mkVarDef ctx nm SortString
                Right val = mkConst ctx (mkString s)
              in do
                declareVariables[varDecl]
                ctx' <- toValExprContext
                let Right var = mkVar ctx' (RefByName nm)
                    Right varIsVal = mkEqual ctx' var val
                    Right varInRe = mkStrInRe ctx' var r
                  in do
                    addAssertions [varIsVal, varInRe]
                    res <- solvable
                    _ <- pop
                    case toMaybeBool res of
                        Nothing -> error ("Problem solver can determine " ++ show s ++ " is in regex of " ++ show r)
                        Just b  -> return b

-- | Is Char in range of low and high
inRange :: ProblemSolver p => Char -> Char -> Char -> p Bool
inRange l h x = do
        ctx <- toValExprContext
        _ <- push
        let Right nm = mkName (Data.Text.pack "var")
            Right varDecl = mkVarDef ctx nm SortString
            Right val = mkConst ctx (mkString (Data.Text.singleton x))
          in do
            declareVariables[varDecl]
            ctx' <- toValExprContext
            let Right var = mkVar ctx' (RefByName nm)
                Right varIsVal = mkEqual ctx' var val
                Right r = mkRegexRange l h
                Right varInRe = mkStrInRe ctx' var r
              in do
                addAssertions [varIsVal, varInRe]       -- two step approach to prevent optimizations by TorXakis
                res <- solvable
                _ <- pop
                case toMaybeBool res of
                    Nothing -> error ("Problem solver can determine " ++ show x ++ " is in range of [" ++ show l ++ "-" ++ show h)
                    Just b  -> return b

-- | Are all chars correctly handled (by escaping when needed) in the lowerbound Position of a range
-- In posix, the character at the position of u at [l - u]
expectationCharsLowRange :: Expectation
expectationCharsLowRange = runSolvers allMatchRanges
    where
        allMatchRanges :: ProblemSolver p => p Bool
        allMatchRanges = and <$> mapM matchRanges allChars

        matchRanges :: ProblemSolver p => Char -> p Bool
        matchRanges c = let (invalidRange, validRange) = splitAt (Data.Char.ord c - Data.Char.ord regexRangeLow) allChars
                            in do
                                bInRanges <- mapM (inRange c regexRangeHigh) validRange
                                bOutRanges <- mapM (inRange c regexRangeHigh) invalidRange
                                return $    and (bInRanges ++ map not bOutRanges) 
                                         || trace ("Char " ++ show c ++ " fails") False

-- | Are all chars correctly handled (by escaping when needed) in the Upperbound Position of a range
-- In posix, the character at the position of u at [l - u]
expectationCharsHighRange :: Expectation
expectationCharsHighRange = runSolvers allMatchRanges
    where
        allMatchRanges :: ProblemSolver p => p Bool
        allMatchRanges = and <$> mapM matchRanges allChars

        matchRanges :: ProblemSolver p => Char -> p Bool
        matchRanges c = let (validRange, invalidRange) = splitAt (Data.Char.ord c - Data.Char.ord regexRangeLow + 1) allChars
                            in do
                                bInRanges <- mapM (inRange regexRangeLow c) validRange
                                bOutRanges <- mapM (inRange regexRangeLow c) invalidRange
                                return $    and (bInRanges ++ map not bOutRanges) 
                                         || trace ("Char " ++ show c ++ " fails") False

spec :: Spec
spec =
  describe "All Smt Solvers" $ do
        it "handle chars correctly" expectationChars
        it "handle chars correctly in lowerbound position of a range" expectationCharsLowRange
        it "handle chars correctly in upperbound position of a range" expectationCharsHighRange
        it "match string generated from regex" $ property prop_StringFromRegex
