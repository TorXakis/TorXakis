-- random solve
randSolve :: (Variable v) => SolveRandParam -> [v] -> Assertions v -> SMT (SolveProblem v)
randSolve _ _  (Assertions AssertFalse)                  = return Unsolvable
randSolve _ [] (Assertions (AssertSet s))   | Set.null s = return $ Solved Map.empty
randSolve p vs (Assertions (AssertSet s))                = 
    let vexps = Set.toList s in
        let vs' = List.nub $ vs ++ concatMap freeVars vexps in do
            sp    <- case p of
                        RandNo                  -> valExprsSolve vs' vexps                          -- allow easy comparison of difference in performance of randomization algorithm
                        RandPartition r         -> randValExprsSolvePartition r vs' vexps
                        RandTrueBins r          -> randValExprsSolveTrueBins r vs' vexps
                        RandIncrementChoice r   -> randValExprsSolveIncrementChoice r vs' vexps
                        RandIncrementBins r     -> randValExprsSolveIncrementBins r vs' vexps
            return $ case sp of 
                    Solved sol    -> Solved $ Map.filterWithKey (\k _ -> k `elem` vs) sol
                    Unsolvable    -> Unsolvable
                    UnableToSolve -> UnableToSolve 
