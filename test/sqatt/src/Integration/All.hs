{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Integration.All (allTests) where

import qualified Integration.Any as Any
import           Sqatt

allTests :: [TxsExampleSet]
allTests = [ Any.testSet ]
