{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Alt.Error where

import           Data.Text       (Text)
import qualified Data.Text        as T

data Error = Error
    { eType :: ErrorType
    , eMsg  :: Text
    } deriving (Show)

data ErrorType = EmptyName 
               | NotUniqueADTs
               | NotUniqueCtrs
               | NotUniqueFields
               | NotDefinedRefs
               | NonCstrADT
               | NoCstr
               | EmptyCstrDefs
               deriving (Show)


errorWithTexts :: ErrorType -> [Text] -> Either Error a
errorWithTexts e ts = Left $ Error e (T.intercalate " " ts)
