{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module EnvServer

-- ----------------------------------------------------------------------------------------- --
--
-- TorXakis Server Environment (Internal State) Data Type Definitions
--
-- ----------------------------------------------------------------------------------------- --
-- export

( IOS             -- type IOS a = StateT EnvS IOC a
                  -- torxakis server main state monad transformer
, EnvS     (..)   -- torxakis server state
, envsNone        -- torxakis server initial state
, TxsModus (..)   -- torxakis server modus
, isNoned         -- isXX :: TxsModus -> Bool
, isIdled         -- check whether torxakis modus is XX
, isInited        --
, isTested        --
, isSimuled       --
, isStepped       --
, isGtNoned       -- isGtXX :: TxsModus -> Bool
, isGtIdled       -- check whether torxakis modus is greater (further) than XX
, isGtInited      --
, getParams       -- :: [String] -> IOS [(String,String)]
, setParams       -- :: [(String,String)] -> IOS [(String,String)]
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Concurrent
import           Control.Monad.State
import           Network.Socket
import           System.IO

import qualified Data.Map            as Map

-- import from local
import           ParamServer

-- import from coreenv
import qualified EnvCore             as IOC

-- import from defs
import qualified Sigs
import qualified TxsDDefs
import qualified TxsDefs

-- import from valexpr
import           Id
import qualified VarId
-- ----------------------------------------------------------------------------------------- --
-- IOS :  torxakis server main state monad transformer


type IOS a = StateT EnvS IOC.IOC a


-- ----------------------------------------------------------------------------------------- --
-- torxakis server state type definitions


data EnvS  = EnvS { host    :: String                    -- ^ host of server client
                    , portNr  :: PortNumber                -- ^ port number of server client
                    , servhs  :: Handle                    -- ^ server socket handle
                    , modus   :: TxsModus                  -- ^ current modus of TXS operation
                    , uid     :: Id                       -- ^ last used unique id number
                    , sigs    :: Sigs.Sigs VarId.VarId   -- ^ Signatures contained in TorXakis files
                    , locvars :: [VarId.VarId]           -- ^ local free variables
                    , locvals :: TxsDefs.VEnv              -- ^ local value environment
                    , tow     :: ( Maybe (Chan TxsDDefs.SAction)
                                 , Maybe ThreadId
                                 , [TxsDDefs.ConnHandle]
                                 )                         -- ^ connections to world
                    , frow    :: ( Maybe (Chan TxsDDefs.SAction)
                                 , [ThreadId]
                                 , [TxsDDefs.ConnHandle]
                                 )                         -- ^ connections from world
                    , params  :: Params                    -- ^ TorXakis parameters with checks
                    }


envsNone    :: EnvS
envsNone   = EnvS { host      = ""
                    , portNr    = 0
                    , servhs    = stderr
                    , modus     = Noned
                    , uid       = 1000
                    , sigs      = Sigs.empty
                    , locvars   = []
                    , locvals   = Map.empty
                    , tow       = ( Nothing, Nothing, [] )
                    , frow      = ( Nothing, [],      [] )
                    , params    = initParams
                    }


-- ----------------------------------------------------------------------------------------- --
-- Txs Modus


data  TxsModus = Noned
                 | Idled
                 | Inited
                 | Tested  TxsDefs.CnectDef
                 | Simuled TxsDefs.CnectDef
                 | Stepped

isNoned, isIdled, isInited        :: TxsModus -> Bool
isTested, isSimuled, isStepped    :: TxsModus -> Bool
isGtNoned, isGtIdled, isGtInited  :: TxsModus -> Bool

isNoned Noned = True
isNoned _     = False
isIdled Idled = True
isIdled _     = False
isInited Inited = True
isInited _      = False
isTested  (Tested _) = True
isTested  _          = False
isSimuled (Simuled _) = True
isSimuled _           = False
isStepped Stepped = True
isStepped _       = False

isGtNoned  m             = not (isNoned m)
isGtIdled  m             = isGtNoned m && not (isIdled m)
isGtInited m             = isGtIdled m && not (isInited m)


-- ----------------------------------------------------------------------------------------- --
-- Params :  getParams, setParams


getParams :: [String] -> IOS [(String,String)]
getParams prms =
     case prms of
       [] -> map (\(nm,(val,_))->(nm,val)) . Map.toList <$> gets params
       _  -> concat <$> mapM getParam prms

getParam :: String -> IOS [(String,String)]
getParam prm = do
     params' <- gets params
     case Map.lookup prm params' of
       Nothing      -> return []
       Just (val,_) -> return [(prm,val)]


setParams :: [(String,String)] -> IOS [(String,String)]
setParams parvals = concat <$> mapM setParam parvals

setParam :: (String,String) -> IOS [(String,String)]
setParam (prm,val) = do
     params' <- gets params
     case Map.lookup prm params' of
       Nothing           -> return []
       Just (_,check) -> if check val
                            then let  newParams = Map.insert prm (val,check) params'
                                  in do
                                    modify $ \env -> env { params = newParams }
                                    return [(prm,val)]
                            else return []

-- ----------------------------------------------------------------------------------------- --
-- Msg :  (Error) Messages

{-

data Msg    =  TXS_SERV_SYSTEM_ERROR     { s :: String }
               | TXS_SERV_MODEL_ERROR      { s :: String }
               | TXS_SERV_USER_ERROR       { s :: String }
               | TXS_SERV_RUNTIME_ERROR    { s :: String }
               | TXS_SERV_SYSTEM_WARNING   { s :: String }
               | TXS_SERV_MODEL_WARNING    { s :: String }
               | TXS_SERV_USER_WARNING     { s :: String }
               | TXS_SERV_RUNTIME_WARNING  { s :: String }
               | TXS_SERV_SYSTEM_INFO      { s :: String }
               | TXS_SERV_MODEL_INFO       { s :: String }
               | TXS_SERV_USER_INFO        { s :: String }
               | TXS_SERV_RUNTIME_INFO     { s :: String }
               | TXS_SERV_RESPONSE         { s :: String }
               | TXS_SERV_OK               { s :: String }
               | TXS_SERV_NOK              { s :: String }
               | TXS_SERV_ANY              { s :: String }
     deriving (Eq,Ord,Read,Show)

instance TxsShow.PShow Msg
  where
     pshow msg = s msg


-- | Add messages to IOS Monad.
putMsgs :: [Msg] -> IOS ()
putMsgs mess = do
     msgs' <- gets msgs
     modify $ \env -> env { msgs = msgs' ++ mess }


-- | Take messages from Monad, and reset message list .
takeMsgs :: IOS [String]
takeMsgs = do
     msgs' <- gets msgs
     modify $ \env -> env { msgs = [] }
     return $ map TxsShow.pshow msgs'

-}
