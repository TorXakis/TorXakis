{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module TorXakis.Compiler.Data where

import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State  (State, MonadState)
import           Data.Map             (Map)
import qualified Data.Map as Map
import           Data.Text            (Text)
import           Data.Semigroup ((<>))
import           Data.Either.Utils (maybeToEither)

import           Id                            (Id (Id))
import           SortId                        (SortId (SortId))
import           CstrId                        (CstrId (CstrId))
import           TorXakis.Sort.ADTDefs         (Sort)
import           TorXakis.Sort.ConstructorDefs (ConstructorDef)

import           TorXakis.Compiler.Error

data Env = Env
    { sortsMap :: Map Text SortId
    , cstrsMap :: Map Text CstrId
    }

findSort :: Env -> Text -> Either Error SortId
findSort e s = maybeToEither err $
    Map.lookup s (sortsMap e)
    where err = "Could not find sort " <> s

findCstr :: Env -> Text -> Either Error CstrId
findCstr e c = maybeToEither err $
    Map.lookup c (cstrsMap e)
    where err = "Could not find constructor " <> c
