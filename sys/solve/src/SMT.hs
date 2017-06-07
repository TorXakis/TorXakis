{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --
module SMT
-- ----------------------------------------------------------------------------------------- --
--
-- SMT: open a connection to SMT, send constraints, and retrieve a model
-- 
-- ----------------------------------------------------------------------------------------- --
-- export

( openSolver        -- :: SMT String
, close             -- :: SMT ()
, push              -- :: SMT ()
, pop               -- :: SMT ()
, createSMTEnv      -- :: SMTEnv
, addDefinitions    -- :: SMT ()
, addDeclarations   -- :: (Variable v) => [v] -> SMT()
, addAssertions     -- :: (Variable v) => [ValExpr v] -> SMT ()
, getSolvable       -- :: SMT SolvableProblem
, getSolution       -- :: (Variable v) => [v] -> SMT (Solution v)

, cmdCVC4
, cmdZ3
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import SMTInternal