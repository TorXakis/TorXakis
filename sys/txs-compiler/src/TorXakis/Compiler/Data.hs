{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module TorXakis.Compiler.Data where

import           Control.Arrow             (left, (|||))
import           Control.Lens              ((&), (.~))
import           Control.Monad.Error.Class (MonadError, catchError, liftEither)
import           Control.Monad.State       (MonadState, StateT, get, put)
import           Data.Either               (partitionEithers)
import           Data.Either.Utils         (maybeToEither)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (catMaybes, fromMaybe)
import           Data.Monoid               (Monoid, (<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Prelude                   hiding (lookup)

import           CstrId                    (CstrId)
import           FuncDef                   (FuncDef)
import           FuncId                    (FuncId, funcargs, funcsort)
import           SortId                    (SortId)
import           VarId                     (VarId)

import           TorXakis.Compiler.Error
import           TorXakis.Parser.Data      hiding (St, nextId)

newtype St = St { nextId :: Int } deriving (Eq, Show)

newState :: St
-- '1000' was chosen for compatibility with the current TorXakis compiler.
newState = St 1000

newtype CompilerM a = CompilerM { runCompiler :: StateT St (Either Error) a }
    deriving (Functor, Applicative, Monad, MonadState St, MonadError Error)

getNextId :: CompilerM Int
getNextId = do
    i <- nextId <$> get
    put (St $ i + 1)
    return i

-- | Like traverse, but catch and error and continue if any `CompilerM` action
-- throws an error in the process.
traverseCatch :: (a -> CompilerM b) -> [a] -> CompilerM ([(a, Error)], [b])
traverseCatch f as = partitionEithers <$>
    traverse (\a -> (`catchError` hdlr a) (fmap Right (f a)) ) as
    where
      hdlr a e = return $ Left (a, e)

-- | Flipped version of traverseCatch
forCatch :: [a] -> (a -> CompilerM b) -> CompilerM ([(a, Error)], [b])
forCatch = flip traverseCatch
