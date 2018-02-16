{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Alt.Cstr where

import           GHC.Generics

import           Alt.Name
import           Alt.Field
import           Alt.LookupTable

-- | Constructor declaration.
data CstrD = CstrD
    { cdName :: Name
    , cdFields :: [FieldD]
    } deriving (Show, Eq, Generic)

instance HasName CstrD

data Cstr = Cstr
    { cName   :: Name
    , cFields :: LookupTable Field
    }

