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

-- | The tests should be re-written to avoid this hardcoded SMT processes.
-- a raw command with arguments: see http://hackage.haskell.org/package/process/docs/System-Process.html#v:proc for more info
defaultSMTProcs :: [(FilePath,[String])]
defaultSMTProcs =
  [ cmdCVC4
  , cmdZ3
  ]

cmdCVC4 :: (FilePath,[String])
cmdCVC4 = ("cvc4", [ "--lang=smt2.5"
                 , "--incremental"
                 , "--strings-exp"
                 , "--fmf-fun-rlv"
                 , "--uf-ss-fair"
                 , "--no-strings-print-ascii"
                 ]
          )

cmdZ3 :: (FilePath,[String])
cmdZ3 = ("z3", [ "-smt2"
               , "-in"
               ]
        )

{- See https://github.com/Z3Prover/z3/issues/2071
cmdZ3Str3 :: (FilePath,[String])
cmdZ3Str3 = ("z3", [ "-smt2"
                   , "-in"
                   , "smt.string_solver=z3str3"
                   ]
            )
-}

{- Yices doesn't support declare-datatypes
cmdYices :: (FilePath,[String])
cmdYices = ("yices-smt2", [ "--incremental"
                          ]
            )
-}

{- MathSat occasionally unexplainably hangs: https://github.com/TorXakis/TorXakis/issues/419
cmdMathSAT :: (FilePath,[String])
cmdMathSAT = ("mathsat", [
                         ]
            )
-}