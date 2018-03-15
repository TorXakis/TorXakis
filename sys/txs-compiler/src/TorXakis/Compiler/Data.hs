{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TorXakis.Compiler.Data where

import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State  (State, MonadState)
import           Data.Map             (Map)

import           Id                            (Id (Id))
import           SortId                        (SortId (SortId))
import           CstrId                        (CstrId (CstrId))
import           TorXakis.Sort.ADTDefs         (Sort)
import           TorXakis.Sort.ConstructorDefs (ConstructorDef)

data Env = Env
    { sortsMap :: Map Sort SortId
    , cstrsMap :: Map Id   CstrId
    }

newtype St = St { nextId :: Int }

newtype CompilerM a = CompilerM 
    { runCompiler :: ReaderT Env (State St) a
    } deriving (Functor, Applicative, Monad, MonadState St)
