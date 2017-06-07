{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module EnDecode

-- ----------------------------------------------------------------------------------------- --
--
-- En- and Decoding between abstract model actions and
-- concrete representations of actions on connections
--  
-- ----------------------------------------------------------------------------------------- --
-- Exporting:

( encode    -- encode :: Action -> IOE SAction
            -- mapping from Abstract to Representation
, decode    -- decode :: SAction -> IOE Action
            -- mapping from Representation to Abstract
)

-- ----------------------------------------------------------------------------------------- --
where

import System.IO
import Control.Monad.State
import Debug.Trace

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import TxsDefs
import TxsDDefs
import TxsUtils

import TxsEnv
import Eval


-- ----------------------------------------------------------------------------------------- --
-- encode :  encoding from Abstract to Concrete (String)


encode :: Action -> IOE SAction

encode (Act offs)  =  do
     ( _, _, towhdls ) <- gets envtow
     ConnHtoW chan h vars vexp <- return $
                                    case [ tow
                                         | tow@(ConnHtoW chan h vars vexp) <- towhdls 
                                         , Set.singleton (sig (IdChan chan)) == Set.map (sig . IdChan . fst) offs
                                         ] of
                                    { [ tow ] -> tow
                                    ; _       -> error $ "TXS Encode: No (unique) action\n"
                                    }
     walues   <- return $ case Set.toList offs of
                          { [ ( chanid, wals ) ] -> wals
                          ; _                    -> error $ "TXS Encode: No (unique) action\n"
                          }
     wenv     <- return $ Map.fromList $ zip vars walues
     sval     <- eval $ cstrEnv (Map.map cstrConst wenv) vexp
     return $ case sval of
              { Cstring s   -> SAct h s
              ; _           -> error "TXS Encode: No encoding to String\n"
              }

encode (ActQui)  =  do
     return $ SActQui


-- ----------------------------------------------------------------------------------------- --
-- decode :  decoding from Concrete (String) to Abstract


decode :: SAction -> IOE Action 

decode (SAct hdl sval)  =  do
     ( _, _, frowhdls ) <- gets envfrow
     ConnHfroW chan h var vexps <- return $
                                     case [ frow
                                          | frow@(ConnHfroW _ h _ _) <- frowhdls
                                          , h == hdl
                                          ] of
                                     { [ frow ] -> frow
                                     ; _        -> error $ "TXS Decode: No (unique) handle\n"
                                     }
     senv     <- return $ Map.fromList $ [ (var, cstrConst (Cstring sval)) ]
     wals     <- mapM (eval.(cstrEnv senv)) vexps
     return $ Act ( Set.singleton (chan,wals) )

decode (SActQui)  =  do
     return $ ActQui


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
