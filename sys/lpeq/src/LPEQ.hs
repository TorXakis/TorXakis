{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEQ
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEQ (
lpeq
) where

import qualified Data.Map as Map
-- import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Control.Monad.State as MonadState
import qualified EnvCore as IOC
import qualified TxsDefs
import ModelIdFactory

import ChanSearch
import ProcSearch
import UniqueObjects
import ProcDepTree

import ExclamToQuest
import VEnvElim
import FlattenedChannels
import ThreadInst
import SeqProgramCounters
import PrefixResolution
import TExprLinearization

-- Linearizes a model definition and (if successful) saves it to the current context.
lpeq :: TxsDefs.ModelId -> TxsDefs.ModelDef -> String -> IOC.IOC (Either [String] (TxsDefs.ModelId, TxsDefs.ModelDef))
lpeq _modelId (TxsDefs.ModelDef insyncs outsyncs splsyncs bexpr) outputModelName = do
    -- TODO Keep a list of all processes that currently exist, so that
    --      temporary processes can be deleted later when done
    
    -- 1. Create copies of all involved processes (we do not want to affect the original ones):
    bexpr1 <- ensureFreshProcsInBExpr bexpr
    -- printProcsInBExpr "BEXPR1::" bexpr1
    
    -- 2. Convert !x to ?x:
    bexpr2 <- exclamToQuest bexpr1
    -- printProcsInBExpr "BEXPR2::" bexpr2
    
    -- 3. Eliminate variable environments through substitution:
    bexpr3 <- eliminateVEnvs bexpr2
    -- printProcsInBExpr "BEXPR3::" bexpr3
    
    -- TODO Eliminate StAuts
    
    -- 5. Create process instantiations for `threads' (=sub-expressions of Parallel / Enable / Disable / Interrupt):
    -- let allChanIds = concatMap Set.toList (insyncs ++ outsyncs)
    allChanIds <- getChansInBExpr insyncs outsyncs bexpr3
    bexpr4 <- doThreadInst allChanIds bexpr3
    -- printProcsInBExpr "BEXPR4::" bexpr4
    
    -- 6. Create processes for each instantiation where the channels are different.
    --    This makes the channel-related part of process signature redundant.
    bexpr5 <- flattenChannels allChanIds bexpr4
    -- printProcsInBExpr "BEXPR5::" bexpr5
    
    -- Before continuing, validate the process dependency tree.
    -- This must (at least) happen AFTER doThreadInst (since process instantiations are used to check for recurring visits)!
    problems <- getProcDepTreeProblems bexpr5
    if null problems
    then do 
            -- printProcsInBExpr "BEXPR5::" bexpr5
            
            -- 7. Place all steps of a process (including steps inside instantiated processes) on the same level in a Choice expression, but
            --    with different requirements of the value of a program counter.
            --    Each member of the Choice expression is called a `branch' (and could become a summand later).
            --    Exception: branches with thread expressions (Parallel / Enable / Disable / Interrupt) are not visited internally!
            bexpr6 <- addSeqProgramCounters bexpr5
            -- printProcsInBExpr "BEXPR6::" bexpr6
            
            -- 8. Rewrite the branches of the involved processes so that they have exactly one ActOffer.
            --    Exception: branches with thread expressions (Parallel / Enable / Disable / Interrupt) are not rewritten here!
            bexpr7 <- resolvePrefixes bexpr6
            printProcsInBExpr "BEXPR7::" bexpr7
            
            -- 9. Linearize branches with thread expressions (Parallel / Enable / Disable / Interrupt):
            bexpr8 <- linearizeTExprs bexpr7
            printProcsInBExpr "BEXPR8::" bexpr8
            
            -- Save the result as a new model:
            tdefs' <- MonadState.gets (IOC.tdefs . IOC.state)
            newModelId <- getModelIdFromName (Text.pack ("LPEQ_" ++ outputModelName))
            let newModelDef = TxsDefs.ModelDef insyncs outsyncs splsyncs bexpr8
            let tdefs'' = tdefs' { TxsDefs.modelDefs = Map.insert newModelId newModelDef (TxsDefs.modelDefs tdefs') }
            IOC.modifyCS (\st -> st { IOC.tdefs = tdefs'' })
            return (Right (newModelId, newModelDef))
    else return (Left problems)
-- lpeqModelDef













