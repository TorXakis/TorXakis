{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
--  Interface for Compilers for the 'TorXakis' MBT tool suite.
--------------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.Compiler
    ( Compiler (..)
    , Error(..)
    )
where

import           TorXakis.Error
import           TorXakis.SortContext

class SortContext ctx => Compiler c ctx where
    compile :: c -> String -> Either Error ctx
    -- compileFile :: c -> FilePath -> IO (Either Error ctx)
