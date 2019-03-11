{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.PrettyPrint.TorXakis
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- PrettyPrinter for TorXakis output
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.PrettyPrint.TorXakis
( -- * Pretty Print Options
  Options (..)
  -- * Pretty Print class for TorXakis
, PrettyPrint (..)
, PrettyPrintContext (..)
  -- * Pretty Print Output for TorXakis
, TxsString (..)
  -- * Helper Functions
, indent
, separator
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.Text           as T
import           GHC.Generics        (Generic)


-- | The data type that represents the options for pretty printing.
data Options = Options { -- | May a definition cover multiple lines?
                         multiline :: Bool
                         -- | Should a definition be short? E.g. by combining of arguments of the same sort.
                       , short     :: Bool
                       }
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

-- | Enables pretty printing in a common way.
class PrettyPrint c a where
    prettyPrint :: Options -> c -> a -> TxsString

-- | Pretty printing all definitions in a context.
class PrettyPrintContext c where
    prettyPrintContext :: Options -> c -> TxsString

-- | The data type that represents the output for pretty printing in TorXakis format.
newtype TxsString = TxsString { toText :: T.Text }
    deriving (Eq, Ord, Read, Generic, NFData, Data)

instance Show TxsString where
    show (TxsString x) = T.unpack x

-- | indentation
indent :: T.Text -> T.Text -> T.Text
indent i t = T.intercalate (T.append (T.singleton '\n') i) (T.lines t)

-- | separator based on option
separator :: Options -> T.Text
separator Options{multiline = m} = if m then T.singleton '\n'
                                        else T.singleton ' '
