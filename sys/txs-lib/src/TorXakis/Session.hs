-- | 
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
module TorXakis.Session where

import           Control.Concurrent.STM.TVar   (TVar)
import           Control.DeepSeq               (NFData)
import           GHC.Generics                  (Generic)
import           Lens.Micro                    (Lens')
import           Control.Concurrent.STM.TQueue (TQueue)
import           Control.Concurrent.MVar       (MVar)
import qualified Data.Map                   as Map

import           Sigs     (Sigs, empty)
import           Id       (Id)
import           VarId    (VarId)
import qualified TxsDefs                  as D
import           EnvCore  (EnvC, initEnvC)
import           EnvData  (Msg)
import           TxsDDefs (Verdict)
import           ParamServer

-- | The session, which maintains the state of a TorXakis model.
data Session = Session
    { _sessionState :: TVar SessionSt
    , _sessionMsgs  :: TQueue Msg
    , _pendingIOC   :: MVar () -- ^ Signal that a pending IOC operation is taking place.
    , _verdicts     :: TQueue Verdict
    }

data SessionSt = SessionSt
    { _uid     :: Id                      -- ^ last used unique id number
    , _tdefs   :: D.TxsDefs               -- ^ TorXakis definitions from file
    , _sigs    :: Sigs VarId              -- ^ signatures contained in TXS files
    , _locvars :: [VarId]                 -- ^ local free variables
    , _locvals :: D.VEnv                  -- ^ local value environment
    , _params  :: Params                  -- ^ TorXakis parameters with checks
    , _modus   :: SessionModus            -- ^ current modus of TXS operation
    , _envCore :: EnvC
    } deriving (Generic, NFData)

data SessionModus =  Idled
                   | Tested   [D.ChanId] [D.ChanId]   -- ^ cnectdef to eworld
                   | Simuled  [D.ChanId] [D.ChanId]   -- ^ cnectdef to eworld
                   | Stepped
                   | Learned  [D.ChanId] [D.ChanId]   -- ^ cnectdef to eworld
                   | Manualed [D.ChanId] [D.ChanId]   -- ^ cnectdef to eworld
    deriving (Show, Generic, NFData)

-- * Session state manipulation
initSessionState :: SessionSt
initSessionState = SessionSt 10000           -- _uid
                             D.empty         -- _tdefs 
                             Sigs.empty      -- _sigs
                             []              -- _locvars
                             Map.empty       -- _locvals
                             initParams      -- _params
                             Idled           -- _modus
                             initEnvC        -- _envCore

-- * Lenses
sessionState :: Lens' Session (TVar SessionSt)
sessionState h (Session s m p v) = (\s' -> Session s' m p v) <$> h s

sessionMsgs :: Lens' Session (TQueue Msg)
sessionMsgs h (Session s m p v) = (\m' -> Session s m' p v) <$> h m

pendingIOC :: Lens' Session (MVar ())
pendingIOC h (Session s m p v) = (\p' -> Session s m p' v) <$> h p

verdicts :: Lens' Session (TQueue Verdict)
verdicts h (Session s m p v) = Session s m p <$> h v

tdefs :: Lens' SessionSt D.TxsDefs
-- Remember:
--
-- > Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
-- > h :: (D.TxsDefs -> f D.TxsDefs)
-- > x :: SessionSt
tdefs h (SessionSt u t s r v p m c) = (\t' -> SessionSt u t' s r v p m c) <$> h t

sigs :: Lens' SessionSt (Sigs VarId)
-- Defining the rest of the lenses is just the same. However we want to avoid
-- depending on template Haskell to scrap the boilerplate.
sigs h (SessionSt u t s r v p m c) = (\s' -> SessionSt u t s' r v p m c) <$> h s

envCore :: Lens' SessionSt EnvC
envCore h (SessionSt u t s r v p m c) = SessionSt u t s r v p m <$> h c

