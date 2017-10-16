{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
module EnDecode

-- ----------------------------------------------------------------------------------------- --
--
-- En- and Decoding between abstract model actions and
-- concrete representations of actions on connections
--
-- ----------------------------------------------------------------------------------------- --
-- export

( encode    -- encode :: Action -> IOC SAction
            -- mapping from Abstract to Representation
, decode    -- decode :: SAction -> IOC Action
            -- mapping from Representation to Abstract
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State

import qualified Data.Map  as Map
import qualified Data.Set  as Set

-- import from serverenv
import qualified EnvServer as IOS

-- import from core
import qualified EnvCore   as IOC
import qualified TxsCore

--import from defs
import           TxsDDefs
import           TxsDefs
import           TxsShow


-- ----------------------------------------------------------------------------------------- --
-- encode :  encoding from Abstract to Concrete (String)


encode :: IOS.EnvS -> Action -> IOC.IOC SAction

encode envs (Act offs)  =  do
     let ( _, _, towhdls ) = IOS.tow envs
     let ss = [ tow
              | tow@(ConnHtoW chan' _h _vars _vexp) <- towhdls
              , Set.singleton chan' == Set.map fst offs
              ]
     let ConnHtoW _chan h vars' vexp =
                             case ss of
                               [ tow ] -> tow
                               _       -> error $ "Encode 1: No (unique) action: " ++ fshow ss
     let walues = case Set.toList offs of
                        [ ( _chanid, wals ) ] -> wals
                        _                     -> error $ "Encode 2: No (unique) action: " ++ fshow offs
     let wenv = Map.fromList $ zip vars' walues
     st <- gets IOC.state
     sval     <- TxsCore.txsEval $ subst (Map.map cstrConst wenv) (funcDefs (IOC.tdefs st)) vexp
     return $ case sval of
                Cstring s -> SAct h s
                _         -> error "Encode 3: No encoding to String\n"

encode _envs ActQui  =
     return SActQui


-- ----------------------------------------------------------------------------------------- --
-- decode :  decoding from Concrete (String) to Abstract


decode :: IOS.EnvS -> SAction -> IOC.IOC Action

decode envs (SAct hdl sval)  =
     let ( _, _, frowhdls )         = IOS.frow envs
         ConnHfroW chan' _h var' vexps = case [ frow
                                             | frow@(ConnHfroW _ h _ _) <- frowhdls
                                             , h == hdl
                                             ] of
                                          { [ frow ] -> frow
                                          ; _        -> error "Decode: No (unique) handle\n"
                                          }
      in do let senv = Map.fromList [ (var', cstrConst (Cstring sval)) ]
            st <- gets IOC.state
            wals     <- mapM (TxsCore.txsEval . subst senv (funcDefs (IOC.tdefs st))) vexps
            return $ Act ( Set.singleton (chan',wals) )

decode _envs SActQui  =
     return ActQui


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
