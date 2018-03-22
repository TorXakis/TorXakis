{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module Integration.All (allTests) where

import qualified Integration.Any        as Any
import qualified Integration.ConfigFile as CF
import           Sqatt

allTests :: [TxsExampleSet]
allTests = [ Any.testSet, CF.testSet ]
