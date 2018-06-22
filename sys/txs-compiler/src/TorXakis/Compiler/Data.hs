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

import           Control.Monad.Error.Class (MonadError, catchError)
import           Control.Monad.State       (MonadState, StateT, gets, put)
import           Data.Either               (partitionEithers)

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
    i <- gets nextId
    put (St $ i + 1)
    return i

-- | Set the unique id counter to the given value.
setUnid :: Int -> CompilerM ()
setUnid unid = put (St unid)

-- | Get the value of the unique id counter.
getUnid :: CompilerM Int
getUnid = gets nextId

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
