{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
module Alt.Name where

import           GHC.Generics
import           Data.Text       (Text)
import           Data.String

newtype Name = Name Text deriving (Show, Eq, Generic)

instance IsString Name

class HasName a where
    name :: a -> Text

    default name :: (Generic a, GHasName (Rep a)) => a -> Text
    name a = gName (from a)

instance HasName Name where
    name (Name n) = n

-- It seems I cannot make this generic ...
--
-- I need to define a GHasName for U1, K1, etc, which I think I cannot do ...
--
class GHasName f where
    gName :: f a -> Text

instance GHasName a => GHasName (M1 i c a) where
    gName (M1 a) = gName a

instance HasName a => GHasName (K1 i a) where
    gName (K1 a) = name a

-- The name is expected always in the first position of the constructor.
instance GHasName a => GHasName (a :*: b) where
    gName (a :*: b) = gName a


