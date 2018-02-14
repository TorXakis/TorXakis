{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
-- ----------------------------------------------------------------------------------------- --

module TreeVars

-- ----------------------------------------------------------------------------------------- --
--
-- Interaction Variables, Communication Tree, Behaviour Tree, Behaviour Node
--
-- ----------------------------------------------------------------------------------------- --
-- export

( BehAction
, CTOffer (..)
, IVar (..)
, IWals
, Menu
, IVEnv
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Data.Monoid
import qualified Data.Set    as Set
import qualified Data.Text   as T

import           ConstDefs
import           Id
import           Name
import           SortId
import           TxsDefs
import           ValExpr
import           Variable
import           VarId

-- ----------------------------------------------------------------------------------------- --
-- ----------------------------------------------------------------------------------------- --
-- BehAct :  behaviour Action


type BehAction  =  Set.Set (TxsDefs.ChanId,[Const])


-- | IVar     :  interaction variable for behaviour tree
--
-- An interaction variable is used to combine the communication of different
-- processes over the same channel.
--
-- The following channel communication:
--
-- > A ? x ? y ? z
--
-- Is associated to the following `IVar`'s:
--
-- > IVar "A" uid 1 d sortOf(x)
-- > IVar "A" uid 2 d sortOf(y)
-- > IVar "A" uid 3 d sortOf(z)
--
-- These variables allow to translate communications like:
--
-- > A ! 6
--
-- which gets translated to:
--
-- > A ? A1 [[ A1 == 6 ]]
--
-- where A1 is associated to `IVar`:
--
-- > IVar "A" uid 1 d Int
--
data  IVar      =  IVar    { ivname :: Name       -- name of Channel
                           , ivuid  :: Id         -- uid of Channel
                           , ivpos  :: Int        -- 1..length (chansorts chan)
                           , ivstat :: Int        -- depth in the behaviour tree
                           , ivsrt  :: SortId     -- (chansorts chan)!!(pos-1)
                           }
     deriving (Eq,Ord,Read,Show)


instance Variable IVar where
    vname (IVar nm uid pos stat _srt) =
      "$" <> nm <> "$" <> (T.pack . show) uid <> "$" <> (T.pack . show) stat <> "$" <> (T.pack . show) pos <> "$"
    vunid IVar{ ivuid = uid } = _id uid
    vsort IVar{ ivsrt = srt } = srt
    cstrVariable s i = IVar (T.pack s) (Id i) (-1) (-1)           -- PvdL for temporary variable

type  IVEnv = VarEnv VarId IVar

type  IWals = WEnv IVar


data  CTOffer   =  CToffer  { ctchan     :: ChanId
                            , ctchoffers :: [IVar]
                            }
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
-- menu

-- |
--
-- An element of the form (offers, hiddenVars, valexp)
--
-- Example offers G ? x | H ? y ~ Set { G ? x ,  H ? y }
--
-- valexp: = value expression over interaction variables. interaction variables
-- must come from the hidden variables or offers.
type  Menu  =  [ ( Set.Set CTOffer, [IVar], ValExpr IVar ) ]


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
