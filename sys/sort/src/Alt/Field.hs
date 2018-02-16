{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module Alt.Field where

import           GHC.Generics
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Alt.Name
import           Alt.Sort
import           Alt.Error

-- | Declaration of a field.
data FieldD = FieldD
    { fdName :: Name
    , fdSort :: Text
    } deriving (Show, Eq, Generic)

instance HasName FieldD

-- | Smart constructor for a field declaration:
--
-- Preconditions:
--
-- * The field name should be non-empty.
--
-- * The field type should be non-empty.
--
mkFieldDecl :: Text -- ^ Field name.
            -> Text -- ^ Field type.
            -> Either Error FieldD
mkFieldDecl fName fType
    | T.null fName = Left $ Error EmptyName "Field name cannot be empty"
    | T.null fType = Left $ Error EmptyName "Field type cannot be empty"
    | otherwise    = Right $ FieldD (Name fName) fType

-- | Type-checked field.
data Field = Field
    { fName :: Name
    , fSort :: Sort
    } deriving (Show, Eq)
