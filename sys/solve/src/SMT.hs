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

( createSMTEnv
, openSolver
, close
, addDefinitions
, addDeclarations
, addAssertions
, getSolvable
, getSolution
, push
, pop
, put
, valExprToString

, cmdCVC4
, cmdZ3
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import SMTInternal