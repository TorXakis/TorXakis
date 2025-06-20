{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Validation
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions to validate 'TorXakis' models
--------------------------------------------------------------------------------
module TorXakis.Compiler.Validation
    ( checkUnique )
where

import           Control.Monad.Error.Class (throwError)
import           Data.List.Unique          (count)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Id                        (Resettable, reset)


import           TorXakis.Compiler.Data    (CompilerM)
import           TorXakis.Compiler.Error

-- | Template to fill in error values.
type ErrorTemplate = (ErrorLoc, Entity, Text)

-- | Check that the entities are unique modulo id's, and use the error template
-- to report an error when duplicated entities are found.
checkUnique :: (Ord e, Resettable e, Show e)
            => ErrorTemplate -> [e] -> CompilerM ()
checkUnique (l, e, what) vs =
    let doubleDefEntities = fmap fst $ filter ((1<) . snd) $ count (reset <$> vs) in
    case doubleDefEntities of
        [] -> return ()
        _  -> throwError Error
            { _errorType = MultipleDefinitions e
            , _errorLoc = l
            , _errorMsg = what <> " double defined: " <> T.pack (show doubleDefEntities)
            }
