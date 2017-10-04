{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module EWorld

-- ----------------------------------------------------------------------------------------- --
--
-- Interface for Connections, External World
--
-- ----------------------------------------------------------------------------------------- --
-- export

( EWorld (..)
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

-- import Control.Monad.State

-- import qualified Data.Char as Char
-- import qualified Data.List as List
-- import qualified Data.Set  as Set
-- import qualified Data.Map  as Map
-- import qualified Data.String.Utils as Utils

-- import from coreenv
-- import qualified EnvCore  as IOC
-- import qualified EnvData
-- import qualified ParamCore

-- import from defs
-- import qualified TxsDefs
-- import qualified Sigs
-- import VarId
-- 
-- import from solve
-- import qualified SMTData


-- ----------------------------------------------------------------------------------------- --
-- IOW :


class EWorld w
  where
     startW  :: w -> IOC.IOC w
     stopW   :: w -> IOC.IOC w 
     putToW  :: w -> TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
     getFroW :: w -> IOC TxsDDefs.Action


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

