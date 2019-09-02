{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


{-# LANGUAGE FlexibleInstances #-}
-- | TorXakis Core Environment (Internal State) Data Type Definitions.
module TorXakis.CoreState
  ( IOC -- IOC = StateT EnvC IO
                  -- torxakis core main state monad transformer
  , EnvC(..)
  , CoreState(..)
  -- * Operation on core-state
  , modifyCS
  , putCS
  )
where

import           Control.Monad.State hiding (state)

--import qualified Data.Map            as Map

import TorXakis.ContextValExpr


-- ----------------------------------------------------------------------------------------- --
-- IOC :  torxakis core state monad transformer

type  IOC  = StateT EnvC IO

newtype EnvC = EnvC
                { state  :: CoreState        -- ^ State specific information.
                }

data CoreState = Noning
             | Initing  { tdefs   :: ContextValExpr                 -- TorXakis definitions
                        }


modifyCS :: (CoreState -> CoreState) -> IOC ()
modifyCS f  = modify $ \env -> env { state = f (state env) }

putCS :: CoreState -> IOC ()
putCS newSt = modify $ \env -> env { state = newSt }


-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

