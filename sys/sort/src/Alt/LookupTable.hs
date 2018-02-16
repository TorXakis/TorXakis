{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Alt.LookupTable where

import           Data.Text       (Text)
import           Data.Map.Strict (Map)

type LookupTable v = Map Text v
