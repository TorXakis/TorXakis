{-TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Regex
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for Regular Expressions.
-- We have chosen UTF-8 based on the needs of our current users
-- and the capabilities of the problem solvers.
-- UTF-8 has 256 characters from \x00 till \xFF
-----------------------------------------------------------------------------
module TorXakis.Regex
( module TorXakis.Regex.RegexBasis
, viewCharRepr
, viewStringRepr
)
where

import           TorXakis.Regex.RegexBasis
import           TorXakis.Regex.CharRepr
import           TorXakis.Regex.StringRepr
