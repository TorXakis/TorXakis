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
, isLearned       --
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
import           Network
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


data EnvS = EnvS { host    :: String                    -- ^ host of server client
                 , portNr  :: PortNumber                -- ^ port number of server client
                 , servhs  :: Handle                    -- ^ server socket handle
                 , modus   :: TxsModus                  -- ^ current modus of TXS operation
                 , uid     :: Id                        -- ^ last used unique id number
                 , tdefs   :: TxsDefs.TxsDefs           -- ^ TorXakis definitions from file
                 , sigs    :: Sigs.Sigs VarId.VarId     -- ^ signatures contained in TXS files
                 , locvars :: [VarId.VarId]             -- ^ local free variables
                 , locvals :: TxsDefs.VEnv              -- ^ local value environment
                 , params  :: Params                    -- ^ TorXakis parameters with checks
                 }

envsNone :: EnvS
envsNone  = EnvS { host      = ""
                 , portNr    = 0
                 , servhs    = stderr
                 , modus     = Noned
                 , uid       = 1000
                 , tdefs     = TxsDefs.empty
                 , sigs      = Sigs.empty
                 , locvars   = []
                 , locvals   = Map.empty
                 , params    = initParams
                 }


-- ----------------------------------------------------------------------------------------- --
-- Txs Modus

data  TxsModus =   Noned
                 | Idled
                 | Inited
                 | Manualed SockWorld                 -- ^ connections to external world
                 | Tested   SockWorld                 -- ^ connections to external world
                 | Simuled  SockWorld                 -- ^ connections to external world
                 | Learned  SockWorld                 -- ^ connections to external world
                 | Stepped

isNoned, isIdled, isInited                            :: TxsModus -> Bool
isManualed, isTested, isSimuled, isStepped, isLearned :: TxsModus -> Bool
isGtNoned, isGtIdled, isGtInited                      :: TxsModus -> Bool
isNoned _             = False
isIdled Idled         = True
isIdled _             = False
isInited Inited       = True
isInited _            = False
isTested  (Tested _)  = True
isTested  _           = False
isSimuled (Simuled _) = True
isSimuled _           = False
isStepped Stepped     = True
isStepped _           = False
isLearned (Learned _) = True
isLearned _           = False

isGtNoned  m          = not (isNoned m)
isGtIdled  m          = isGtNoned m && not (isIdled m)
isGtInited m          = isGtIdled m && not (isInited m)


-- ----------------------------------------------------------------------------------------- --
-- Params :  getParams, setParams


getParams :: [String] -> IOS [(String,String)]
getParams prms =
     case prms of
       [] -> do parammap <- gets params
                return $ map (\(nm,(val,_))->(nm,val)) (Map.toList parammap)
       _  -> do params' <- mapM getParam prms
                return $ concat params'

getParam :: String -> IOS [(String,String)]
getParam prm = do
     params' <- gets params
     case Map.lookup prm params' of
       Nothing      -> return []
       Just (val,_) -> return [(prm,val)]


setParams :: [(String,String)] -> IOS [(String,String)]
setParams parvals = do
     params' <- mapM setParam parvals
     return $ concat params'

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
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

