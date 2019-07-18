{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  MCRL2Env
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module MCRL2Env (
T2MMonad,
T2MRegisteredObject(..),
T2MEnv(..),
emptyEnv,
modifySpec,
registerObject,
getRegisteredSort,
getRegisteredCstr,
getRegisteredVar,
getRegisteredMapping,
getRegisteredAction,
getRegisteredProcess,
getFreshName
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.Text as Text
import Control.Monad.State hiding (state)
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified MCRL2Defs
import qualified SortId
import qualified FuncId
import qualified ChanId
import qualified ProcId
import qualified CstrId
import qualified VarId

type T2MMonad = StateT T2MEnv IOC.IOC

data T2MRegisteredObject = RegSort MCRL2Defs.ObjectId
                         | RegCstr MCRL2Defs.ObjectId MCRL2Defs.ObjectId
                         | RegVar MCRL2Defs.Variable
                         | RegMapping MCRL2Defs.ObjectId
                         | RegAction MCRL2Defs.ObjectId
                         | RegProcess MCRL2Defs.ObjectId
-- T2MRegisteredObject

data T2MEnv = T2MEnv { txsdefs :: TxsDefs.TxsDefs
                     , specification :: MCRL2Defs.Specification
                     , objectMap :: Map.Map TxsDefs.Ident T2MRegisteredObject
                     , usedNames :: Set.Set MCRL2Defs.ObjectId
                     }
-- T2MEnv

emptyEnv :: T2MEnv
emptyEnv = T2MEnv { txsdefs = TxsDefs.empty
                  , specification = MCRL2Defs.emptySpecification
                  , objectMap = Map.empty
                  , usedNames = Set.fromList (map Text.pack mCRL2KeywordList)
                  }
-- emptyEnv

mCRL2KeywordList :: [String]
mCRL2KeywordList = [
    "act", "allow", "block", "comm", "cons", "delay", "div", "end", "eqn", "exists",
    "forall", "glob", "hide", "if", "in", "init", "lambda", "map", "mod", "mu", "nu", "pbes",
    "proc", "rename", "sort", "struct", "sum", "val", "var", "whr", "yaled",
    "Bag", "Bool", "Int", "List", "Nat", "Pos", "Real", "Set",
    "delta", "false", "nil", "tau", "true",
    
    "min", "max", "succ", "pred", "div", "mod", "exp", "abs", "floor", "ceil", "round",
    "Pos2Nat", "Nat2Pos", "Pos2Int", "Int2Pos", "Nat2Int", "Int2Nat",
    "head", "tail", "rhread", "rtail", "count",
    "Set2Bag", "Bag2Set"
  ]
-- mCRL2KeywordList

modifySpec :: (MCRL2Defs.Specification -> MCRL2Defs.Specification) -> T2MMonad ()
modifySpec f = modify (\env -> env { specification = f (specification env) })

registerObject :: TxsDefs.Ident -> T2MRegisteredObject -> T2MMonad ()
registerObject idt reg = modify (\env -> env { objectMap = Map.insert idt reg (objectMap env) })

getRegisteredSort :: SortId.SortId -> T2MMonad (MCRL2Defs.ObjectId, MCRL2Defs.Sort)
getRegisteredSort sortId = do
    oMap <- gets objectMap
    case Map.lookup (TxsDefs.IdSort sortId) oMap of
      Just (RegSort oId) -> do
        sorts <- gets (MCRL2Defs.sorts . specification)
        case Map.lookup oId sorts of
          Just s -> return (oId, s)
          _ -> return (oId, MCRL2Defs.MissingSort)
      _ -> return (Text.pack "SORT_NOT_FOUND", MCRL2Defs.MissingSort)
-- getRegisteredSort

getRegisteredCstr :: CstrId.CstrId -> T2MMonad (MCRL2Defs.ObjectId, MCRL2Defs.Constructor)
getRegisteredCstr cstrId = do
    oMap <- gets objectMap
    case Map.lookup (TxsDefs.IdCstr cstrId) oMap of
      Just (RegCstr sId cId) -> do
        sorts <- gets (MCRL2Defs.sorts . specification)
        case Map.lookup sId sorts of
          Just (MCRL2Defs.StructSort constructors) ->
            case filter (\MCRL2Defs.Constructor { MCRL2Defs.cstrName = cstrName } -> cstrName == cId) constructors of
              [c] -> return (cId, c)
              _ -> return (cId, MCRL2Defs.MissingConstructor)
          _ -> return (Text.pack "CSTR_NOT_FOUND", MCRL2Defs.MissingConstructor)
      _ -> return (Text.pack "CSTR_NOT_FOUND", MCRL2Defs.MissingConstructor)
-- getRegisteredCstr

getRegisteredVar :: VarId.VarId -> T2MMonad (MCRL2Defs.ObjectId, MCRL2Defs.Variable)
getRegisteredVar varId = do
    oMap <- gets objectMap
    case Map.lookup (TxsDefs.IdVar varId) oMap of
      Just (RegVar var) -> return (MCRL2Defs.varName var, var)
      _ -> return (Text.pack "VAR_NOT_FOUND", MCRL2Defs.MissingVariable)
-- getRegisteredVar

getRegisteredMapping :: FuncId.FuncId -> T2MMonad (MCRL2Defs.ObjectId, MCRL2Defs.Sort)
getRegisteredMapping funcId = do
    oMap <- gets objectMap
    case Map.lookup (TxsDefs.IdFunc funcId) oMap of
      Just (RegMapping oId) -> do
        mappings <- gets (MCRL2Defs.mappings . specification)
        case Map.lookup oId mappings of
          Just m -> return (oId, m)
          _ -> return (oId, MCRL2Defs.MissingMapping)
      _ -> return (Text.pack "MAPPING_NOT_FOUND", MCRL2Defs.MissingMapping)
-- getRegisteredMapping

getRegisteredAction :: ChanId.ChanId -> T2MMonad (MCRL2Defs.ObjectId, MCRL2Defs.Action)
getRegisteredAction chanId = do
    oMap <- gets objectMap
    case Map.lookup (TxsDefs.IdChan chanId) oMap of
      Just (RegAction oId) -> do
        actions <- gets (MCRL2Defs.actions . specification)
        case Map.lookup oId actions of
          Just a -> return (oId, a)
          _ -> return (oId, MCRL2Defs.Action MCRL2Defs.MissingSort)
      _ -> return (Text.pack "ACTION_NOT_FOUND", MCRL2Defs.MissingAction)
-- getRegisteredAction

getRegisteredProcess :: ProcId.ProcId -> T2MMonad (MCRL2Defs.ObjectId, MCRL2Defs.Process)
getRegisteredProcess procId = do
    oMap <- gets objectMap
    case Map.lookup (TxsDefs.IdProc procId) oMap of
      Just (RegProcess oId) -> do
        processes <- gets (MCRL2Defs.processes . specification)
        case Map.lookup oId processes of
          Just p -> return (oId, p)
          _ -> return (oId, MCRL2Defs.MissingProcess)
      _ -> return (Text.pack "PROCESS_NOT_FOUND", MCRL2Defs.MissingProcess)
-- getRegisteredProcess

getFreshName :: MCRL2Defs.ObjectId -> T2MMonad MCRL2Defs.ObjectId
getFreshName prefix = do
    let legalizedPrefix = makeLegal prefix
    namesInUse <- gets usedNames
    let freshName = if Set.member legalizedPrefix namesInUse then getFreshNameWithIndex legalizedPrefix namesInUse 1 else legalizedPrefix
    modify (\env -> env { usedNames = Set.insert freshName namesInUse })
    return freshName
-- getFreshName

getFreshNameWithIndex :: MCRL2Defs.ObjectId -> Set.Set MCRL2Defs.ObjectId -> Integer -> MCRL2Defs.ObjectId
getFreshNameWithIndex prefix namesInUse index =
    let freshName = Text.pack (Text.unpack prefix ++ show index) in
      if Set.member freshName namesInUse
      then getFreshNameWithIndex prefix namesInUse (index + 1)
      else freshName
-- getFreshNameWithIndex

makeLegal :: MCRL2Defs.ObjectId -> MCRL2Defs.ObjectId
makeLegal text = Text.pack (map makeCharLegal (Text.unpack text))
  where
    makeCharLegal :: Char -> Char
    makeCharLegal c =
        if Char.isAlphaNum c || (c == '_')
        then c
        else '_'
-- makeLegal



