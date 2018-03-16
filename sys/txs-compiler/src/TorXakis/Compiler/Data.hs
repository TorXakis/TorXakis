{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module TorXakis.Compiler.Data where

import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State  (State, MonadState, StateT)
import           Data.Map             (Map)
import qualified Data.Map as Map
import           Data.Text            (Text)
import           Data.Semigroup ((<>))
import           Data.Either.Utils (maybeToEither)
import           Control.Monad.State (StateT, put, get)
import           Control.Monad.Error.Class (MonadError, throwError)

import           Id                            (Id (Id))
import           SortId                        (SortId (SortId))
import           CstrId                        (CstrId (CstrId))
import           TorXakis.Sort.ADTDefs         (Sort)
import           TorXakis.Sort.ConstructorDefs (ConstructorDef)

import           TorXakis.Parser.Data (CstrDecl, uid, nodeMdata, nodeName)
import           TorXakis.Compiler.Error

data Env = Env
    { sortsMap :: Map Text SortId
    , cstrsMap :: Map Int  CstrId
    }

findSort :: Env -> Text -> Either Error SortId
findSort e s = maybeToEither err $
    Map.lookup s (sortsMap e)
    where err = "Could not find sort " <> s

findSortM :: Env -> Text -> CompilerM SortId
findSortM e s =
    case Map.lookup s (sortsMap e) of
        Nothing  -> throwError $ "Could not find sort " <> s
        Just sId -> return sId

findCstr :: Env -> CstrDecl -> Either Error CstrId
findCstr e c = maybeToEither err $
    Map.lookup (uid . nodeMdata $ c) (cstrsMap e)
    where err = "Could not find constructor " <> nodeName c

findCstrM :: Env -> CstrDecl -> CompilerM CstrId
findCstrM e c =
    case Map.lookup (uid . nodeMdata $ c) (cstrsMap e) of
        Nothing  -> throwError $ "Could not find constructor " <> nodeName c
        Just sId -> return sId

newtype St = St { nextId :: Int } deriving (Eq, Show)

newtype CompilerM a = CompilerM { runCompiler :: StateT St (Either Error) a }
    deriving (Functor, Applicative, Monad, MonadState St, MonadError Error)

getNextId :: CompilerM Int
getNextId = do
    i <- nextId <$> get
    put (St $ i + 1)
    return i
