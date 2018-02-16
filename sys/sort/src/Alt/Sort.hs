{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module Alt.Sort where

import           Data.Text       (Text)

import           Alt.Name
import           GHC.Generics

newtype Sort = Sort { sName :: Name } deriving (Show, Eq, Generic)

instance HasName Sort

fromText :: Text -> Sort
fromText = Sort . Name

-- | Predefined sorts.
predefSorts :: [Sort]
predefSorts =
    [ Sort "Bool"
    , Sort "Int"
    , Sort "Char"
    , Sort "Regex"
    , Sort "String"
    ]
