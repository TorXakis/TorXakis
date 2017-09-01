{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
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
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           SMTInternal
