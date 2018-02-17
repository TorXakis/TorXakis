{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Alt.ADT where

import           GHC.Generics
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.List.Unique

import           Alt.Error
import           Alt.Name
import           Alt.Cstr
import           Alt.LookupTable

data ADTD = ADTD
    { adName :: Name
    , adCstrs :: [CstrD]
    } deriving (Show, Eq, Generic)

-- | Smart constructor for an ADT declaration:
--
-- Preconditions:
--
-- * The ADT name should be non-empty.
--
-- * All the constructor names should be unique.
--
-- * All the field names should be unique across all constructor declarations.
--
mkADTD :: Text -- ^ Constructor name
       -> [CstrD]
       -> Either Error ADTD
mkADTD n cs
    | T.null n        = Left $ Error EmptyName "ADT name cannot be empty"
    | null cs         = Left $ Error NoCstr "ADT must have at least one constructor"
    | not (null nuCs) = Left $ Error NotUniqueCtrs (T.intercalate " " nuCs)
    | not (null nuFs) = Left $ Error NotUniqueFields (T.intercalate " " nuFs)
    | otherwise       = Right $ ADTD (Name n) cs
    where
      nuCs = repeated $ name <$> cs
      nuFs = repeated $ name <$> concatMap cdFields cs

-- | Like 'mkADTD' but take a list of @Either Error CstrD@, which allow to
-- compose user defined constructor declarations with ADT declarations.
(.::=) :: Text -> [Either Error CstrD] -> Either Error ADTD
n .::= ecs = sequence ecs >>= mkADTD n

data ADT = ADT
    { aName :: Name
    , aCstrs :: LookupTable Cstr
    } deriving (Show, Eq, Generic)
