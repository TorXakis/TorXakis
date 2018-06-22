{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.MapperDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for mapper declarations.
--------------------------------------------------------------------------------
module TorXakis.Parser.MapperDecl
    (mapperDeclP)
where

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.ChanRef

-- | Parser for mapper declarations.
mapperDeclP :: TxsParser MapperDecl
mapperDeclP =  declP "MAPPERDEF" $ \n l -> do
    is <- chansInDecl
    os <- chansOutDecl
    ys <- chansSyncDecl
    txsSymbol "BEHAVIOUR"
    be <- bexpDeclP
    return $ mkMapperDecl n l is os ys be
