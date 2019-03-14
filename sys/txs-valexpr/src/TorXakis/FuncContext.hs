{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Function Context
-----------------------------------------------------------------------------
module TorXakis.FuncContext
( -- * Context
  -- ** Func Context class
  FuncContext (..)
, prettyPrintFuncContext
  -- dependencies, yet part of interface
, module TorXakis.FuncSignatureContext
, FuncDef
)
where
import qualified Data.Text           as T

import           TorXakis.FuncDef
import           TorXakis.FuncSignatureContext
import           TorXakis.PrettyPrint.TorXakis

-- | A FuncContext Context instance contains all definitions to work with 'TorXakis.FuncDef'.
class FuncSignatureContext c => FuncContext c where
    -- | lookup FuncDef
    lookupFunc :: FuncSignature -> c -> Maybe FuncDef
    -- | All FuncDef elements in the context
    elemsFunc :: c -> [FuncDef]
    -- | Add FuncDefs to func context.
    --   A func context is returned when the following constraints are satisfied:
    --
    --   * All func Signatures are distinct
    --
    --   * All references are known (sort, variables, func signatures)
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addFuncs :: [FuncDef] -> c -> Either Error c

-- | Generic Pretty Printer for all instance of 'TorXakis.FuncContext'.
-- TODO: move to ContextFunc file
prettyPrintFuncContext :: FuncContext c => Options -> c -> TxsString
prettyPrintFuncContext o fc =
    TxsString (T.concat [ T.intercalate (T.pack "\n") (map (TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o fc) (elemsADT fc))
                        , T.pack "\n"
                        , T.intercalate (T.pack "\n") (map (TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o fc) (elemsFunc fc))
                        ])

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
