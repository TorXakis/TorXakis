{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ValExprContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Processes: all defined sorts, functions, and processes.
-----------------------------------------------------------------------------
module TorXakis.ProcContext
( -- * Process Context
  ProcContext(..)
)
where
import           TorXakis.Error
import           TorXakis.FuncContext
import           TorXakis.ProcDef
import           TorXakis.ProcSignature

-- | A ProcContext instance contains all definitions to work with behavioural expressions and references thereof
class FuncContext c => ProcContext c where
    -- | Points the provided ProcSignature to a ProcessDefinition in the context?
    memberProc :: ProcSignature -> c -> Bool
    -- | lookup ProcDef using the provided process signature.
    lookupProc :: ProcSignature -> c -> Maybe ProcDef
    -- | All procSignatures in the context.
    -- 
    -- Since all procSignatures are distinct the following properties hold:
    --
    -- prop> List.nub (procSignatures x) == procSignatures x
    -- 
    -- prop> Set.toList (Set.fromList (procSignatures x)) == procSignatures x
    procSignatures :: c -> [ProcSignature]
    -- | All ProcDef elements in the context
    elemsProc :: c -> [ProcDef]
    -- | Add process definitions to behavioural expression context.
    --   A proc context is returned when the following constraints are satisfied:
    --
    --   * All proc Signatures are distinct
    --
    --   * All references are known (sort, variables, functions, and processes)
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addProcs :: [ProcDef] -> c -> Either Error c
