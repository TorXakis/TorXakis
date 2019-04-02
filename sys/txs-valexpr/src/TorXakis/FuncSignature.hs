{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.FuncSignature
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Interface for FuncSignature related functionality.
-- Not all function signatures can be defined. 
-- Some function signatures are reserved/predefined functions in TorXakis.
-----------------------------------------------------------------------------
module TorXakis.FuncSignature
( module TorXakis.FuncSignature.FuncSignature
, module TorXakis.FuncSignature.RefByFuncSignature
, module TorXakis.FuncSignature.UsedFuncSignatures
)
where

import TorXakis.FuncSignature.FuncSignature
import TorXakis.FuncSignature.RefByFuncSignature
import TorXakis.FuncSignature.UsedFuncSignatures