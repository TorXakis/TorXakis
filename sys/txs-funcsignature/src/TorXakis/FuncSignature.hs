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
--
-- For round tripping two kind of functions are supported:
-- * Prefix functions
-- * Infix functions, also known as operators.
-- The names of prefix functions are 'TorXakis.Name.Name's, i.e. all names that adhere to the regular expression [A-Z_a-z][A-Z_a-z-0-9]* except the TorXakis reserved words.
-- The names of infix functions are 'TorXakis.OperatorName.OperatorName's, i.e. all names that adhere to the regular expression [-+*=^/\\<>|@&%]+.
-- Not all function signatures can be defined. Some function signatures are reserved/predefined functions in TorXakis.
-----------------------------------------------------------------------------
module TorXakis.FuncSignature
( module TorXakis.FuncSignature.FuncSignature
, module TorXakis.FuncSignature.RefByFuncSignature
)
where

import TorXakis.FuncSignature.FuncSignature
import TorXakis.FuncSignature.RefByFuncSignature