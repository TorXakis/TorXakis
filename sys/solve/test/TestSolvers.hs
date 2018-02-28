{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- | The tests in this module should be re-written to make use of a TorXakis
-- configuration file (`.torxakis.yaml`) which specifies the paths of the
-- solvers in the user' system.
--
-- See: https://github.com/TorXakis/TorXakis/issues/197
module TestSolvers where

import           System.Process

-- | The  should be re-written to avoid this hardcoded SMT processes.
defaultSMTProcs :: [CreateProcess]
defaultSMTProcs =
  [ cmdCVC4
  , cmdZ3
  , cmdZ3Str3
  ]

cmdCVC4 :: CreateProcess
cmdCVC4 = proc "cvc4"
                [ "--lang=smt"
                , "--incremental"
                , "--strings-exp"
                , "--fmf-fun-rlv"
                , "--uf-ss-fair"
                , "--no-strings-std-ascii"
                ]

cmdZ3 :: CreateProcess
cmdZ3 = proc    "z3"
                [ "-smt2"
                , "-in"
                ]

cmdZ3Str3 :: CreateProcess
cmdZ3Str3 = proc "z3"
                   [ "-smt2"
                   , "-in"
                   , "smt.string_solver=z3str3"
                   ]

