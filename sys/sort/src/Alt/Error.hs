{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Alt.Error where

import           Data.Text       (Text)

data Error = Error
    { eType :: ErrorType
    , eMsg  :: Text
    } deriving (Show)

data ErrorType = EmptyName 
               | NotUniqueADTs
               | NotUniqueCtrs
               | NotUniqueFields
               | UndefinedRefs
               | NonCtrADT
               | EmptyCtrDefs
               deriving (Show)
