{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Referable
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the referable class.
-- The referable class links a type to its reference type.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
module TorXakis.Referable
( -- * Referable
  Referable(..)
)
where
import           Data.Data            (Data)
import           Data.Hashable

-- | A referable class
-- An object can only have one reference.
-- For example, a function is either referenced by name of by signature but not both.
class ( Eq (Ref a)
      , Ord (Ref a)
      , Hashable (Ref a)
      , Show (Ref a)
      , Read (Ref a)
      , Data (Ref a)
      ) => Referable a where
    type Ref a
    toRef :: a -> Ref a
