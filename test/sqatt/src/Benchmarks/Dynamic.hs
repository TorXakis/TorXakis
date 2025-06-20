{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Benchmarks.Dynamic
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Benchmarks in which processes are dynamically created/instantiated.
-- Note: these benchmarks thus can NOT be transformed by LPE.
-----------------------------------------------------------------------------
module Benchmarks.Dynamic (benchmarksSet) where

import           Benchmarks.Common
import           Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Dynamic"

nonDistinguishing :: TxsExample
nonDistinguishing = emptyExample
    { exampleName = "non distinguishing output"
    , txsModelFiles = [insqatt $ txsFilePath BenchTest benchDir "nonDistinguishing" ]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "nonDistinguishing"
                     ]
    , expectedResult = Pass
    }

distinguishByValue :: TxsExample
distinguishByValue = emptyExample
    { exampleName = "distinguishing output by Value"
    , txsModelFiles = [insqatt $ txsFilePath BenchTest benchDir "distinguishByValue" ]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "distinguishByValue"
                     ]
    , expectedResult = Pass
    }

distinguishByOrder :: TxsExample
distinguishByOrder = emptyExample
    { exampleName = "distinguishing output by Order"
    , txsModelFiles = [ insqatt $ txsFilePath BenchTest benchDir "distinguishByOrder" ]
    , txsCmdsFiles = [ seedSetupCmdFile
                     , insqatt $ txsCmdPath BenchTest benchDir "distinguishByOrder"
                     ]
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Dynamic" [ nonDistinguishing
                                        , distinguishByValue
                                        , distinguishByOrder
                                        ]
