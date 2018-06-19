{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Data
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
--  Data used for the 'TorXakis' compiler.
--------------------------------------------------------------------------------
module TorXakis.Compiler.Data
    ( CompilerM
    , runCompiler
    -- * State query and manipulation
    , St
    , newState
    , getUnid
    , setUnid
    , getNextId
    -- * Traversals using the @CompilerM@ monad
    , traverseCatch
    , forCatch
    )
where

import           Control.Arrow             (left, (|||))
import           Control.Lens              ((&), (.~))
import           Control.Monad.Error.Class (MonadError, catchError, liftEither)
import           Control.Monad.State       (MonadState, StateT, get, put)
import           Data.Either               (partitionEithers)
import           Data.Either.Utils         (maybeToEither)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (catMaybes, fromMaybe)
import           Data.Monoid               (Monoid)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           CstrId                    (CstrId)
import           FuncDef                   (FuncDef)
import           FuncId                    (FuncId, funcargs, funcsort)
import           SortId                    (SortId)
import           VarId                     (VarId)

import           TorXakis.Compiler.Error   (Error)

-- | State of the compiler.
newtype St = St { nextId :: Int } deriving (Eq, Show)

newState :: St
-- '1000' was chosen for compatibility with the current TorXakis compiler.
newState = St 1000

newtype CompilerM a = CompilerM { runCompiler :: StateT St (Either Error) a }
    deriving (Functor, Applicative, Monad, MonadState St, MonadError Error)

-- | Get the next unique id, and increment the unique id counter in the
-- compiler state.
getNextId :: CompilerM Int
getNextId = do
    i <- nextId <$> get
    put (St $ i + 1)
    return i

-- | Set the unique id counter to the given value.
setUnid :: Int -> CompilerM ()
setUnid unid = put (St unid)

-- | Get the value of the unique id counter.
getUnid :: CompilerM Int
getUnid = nextId <$> get

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
