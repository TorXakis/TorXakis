{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Alt.Cstr where

import           GHC.Generics
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.List.Unique

import           Alt.Error
import           Alt.Name
import           Alt.Field
import           Alt.LookupTable

-- | Constructor declaration.
data CstrD = CstrD
    { cdName :: Name
    , cdFields :: [FieldD]
    } deriving (Show, Eq, Generic)

instance HasName CstrD

-- | Smart constructor for a constructor declaration.
--
-- Preconditions:
--
-- * The constructor name should be non-empty
--
-- * All the field names should be unique
--
mkCstrD :: Text        -- ^ Constructor name.
        -> [FieldD] -- ^ Fields
        -> Either Error CstrD
mkCstrD n fs
    | T.null n = Left $ Error EmptyName "Constructor name cannot be empty"
    | not (null nuFs) = Left $ Error NotUniqueFields (T.intercalate " " nuFs)
    | otherwise = Right $ CstrD (Name n) fs
    where
      nuFs = repeated $ name <$> fs

-- | Like 'mkCstrD' but take a list of @Either Error FieldDecl@, which allow to
-- compose user defined field declarations with constructors declaration in a
-- convenient way.
(.=) :: Text -> [Either Error FieldD] -> Either Error CstrD
cName .= efs = sequence efs >>= mkCstrD cName

data Cstr = Cstr
    { cName   :: Name
    , cFields :: LookupTable Field
    } deriving (Show, Eq, Generic)
