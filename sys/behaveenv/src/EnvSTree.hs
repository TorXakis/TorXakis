{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module EnvSTree

-- ----------------------------------------------------------------------------------------- --
--
-- Interaction Variables, Communication Tree, Behaviour Tree, Behaviour Node
--
-- ----------------------------------------------------------------------------------------- --
-- export

( SEE
, EnvE (..)
, putMsgs
, getFuncDefs
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State
import qualified Data.Map            as Map
import qualified EnvData
import qualified ParamCore

-- import from defs
import           FuncDef
import           FuncId
import qualified Sigs
import           TxsDefs
import           VarId

-- | SEE : symbolic expansion environment state monad, necessary in STree manipulation
type SEE a = State EnvE a

data EnvE = EnvE { tdefs   :: TxsDefs.TxsDefs               -- ^ TorXakis definitions
                 , sigs    :: Sigs.Sigs VarId
                 , stateid :: EnvData.StateNr               -- ^ Current beh statenr
                 , params  :: ParamCore.Params              -- ^ Parameters
                 , msgs    :: [EnvData.Msg]                 -- ^ (error) reporting
                 }

-- | Add messages to SEE Monad.
putMsgs :: [EnvData.Msg] -> SEE ()
putMsgs mess  =  do
     msgs' <- gets msgs
     modify $ \env -> env { msgs = msgs' ++ mess }

-- | Get the function defined at in the current state.
getFuncDefs :: SEE (Map.Map FuncId (FuncDef VarId))
getFuncDefs = funcDefs . tdefs <$> get
