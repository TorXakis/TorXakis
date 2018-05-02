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

( encode    -- :: [DD.ConnHandle] -> DD.Action -> IOC.IOC DD.SAction
            -- mapping from Abstract to Representation
, decode    -- :: [DD.ConnHandle] -> DD.SAction -> IOC.IOC DD.Action
            -- mapping from Representation to Abstract
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State

import           Data.Either         (partitionEithers)
import qualified Data.Map            as Map
import qualified Data.Set            as Set

-- import from serverenv
-- import qualified EnvServer    as IOS

-- import from core
import qualified EnvCore             as IOC
import qualified TxsCore

--import from defs
import qualified TxsDDefs            as DD
import qualified TxsDefs             as D
import           TxsShow

-- import from valexpr
import           ConstDefs
import           ValExpr

-- ----------------------------------------------------------------------------------------- --
-- encode :  encoding from Abstract to Concrete (String)


encode :: [DD.ConnHandle] -> DD.Action -> IOC.IOC DD.SAction
encode towhdls (DD.Act offs)
  =  let ss     = [ tow
                  | tow@(DD.ConnHtoW chan' _h _vars _vexp) <- towhdls
                  , Set.singleton chan' == Set.map fst offs
                  ]
         DD.ConnHtoW _chan h vars' vexp
                = case ss of
                    [ tow ] -> tow
                    _       -> error $ "Encode 1: No (unique) action: " ++ fshow ss
         walues = case Set.toList offs of
                    [ ( _chanid, wals ) ] -> wals
                    _                     -> error $ "Encode 2: " ++
                                                     "No (unique) action: " ++ fshow offs
         wenv   = Map.fromList $ zip vars' walues
      in do st   <- gets IOC.state
            sval <- TxsCore.txsEval $
                      subst (Map.map cstrConst wenv) (D.funcDefs (IOC.tdefs st)) vexp
            return $ case sval of
                       Right (Cstring s) -> DD.SAct h s
                       x -> error $ "Encode 3: No encoding to String\n" ++ show x

encode _towhdls DD.ActQui
  =  return DD.SActQui


-- ----------------------------------------------------------------------------------------- --
-- decode :  decoding from Concrete (String) to Abstract


decode :: [DD.ConnHandle] -> DD.SAction -> IOC.IOC DD.Action
decode frowhdls (DD.SAct hdl sval)
  =  let DD.ConnHfroW chan' _h var' vexps
              = case [ frow | frow@(DD.ConnHfroW _ h _ _) <- frowhdls, h == hdl ] of
                  [ frow ] -> frow
                  _        -> error "Decode: No (unique) handle\n"
         senv = Map.fromList [ (var', cstrConst (Cstring sval)) ]
      in do st   <- gets IOC.state
            eWals <- mapM (TxsCore.txsEval . subst senv (D.funcDefs (IOC.tdefs st))) vexps
            case partitionEithers eWals of
                ([], wals) -> return $ DD.Act ( Set.singleton (chan',wals) )
                (errs, _)  -> error $ "Decode: " ++ show errs

decode _frowhdls DD.SActQui
  =  return DD.ActQui


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

