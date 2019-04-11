{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Function Definition
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.FuncDef
( FuncDef
, TorXakis.FuncDef.funcName
, paramDefs
, body
, mkFuncDef
, undefinedSortsInFuncs
, undefinedVariablesInFuncs
)
where

import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as Set
import           GHC.Generics           (Generic)

import           TorXakis.ContextVar
import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.Var
import           TorXakis.ValExpr.ValExpr

-- | Data structure to store the information of a Function Definition:
-- * A Name
-- * A list of variables
-- * A body (possibly using the variables)
data FuncDef = FuncDef { -- | The name of the function (of type 'TorXakis.Name')
                         funcName :: FunctionName
                         -- | The function parameter definitions
                       , paramDefs :: VarsDecl
                         -- | The body of the function
                       , body :: ValExpression
                       }
     deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

toVarContext :: SortContext c => c -> VarsDecl -> ContextVar
toVarContext ctx vs =
    case addVars (toList vs) (fromSortContext ctx) of
        Left e      -> error ("toVarContext is unable to make new context" ++ show e)
        Right vctx  -> vctx

-- | Helper function for construction: check for undefined sorts per function definition
undefinedSortsInFuncs :: SortContext c => c -> [FuncDef] -> [(FuncSignature, Set.Set Sort)]
undefinedSortsInFuncs ctx = mapMaybe maybeUndefinedSortsInFunc
    where
        maybeUndefinedSortsInFunc :: FuncDef -> Maybe (FuncSignature, Set.Set Sort)
        maybeUndefinedSortsInFunc f = let sortsUndefined = undefinedSorts ctx (paramDefs f) (body f)
                                         in if Set.null sortsUndefined
                                               then Nothing
                                               else Just (getFuncSignature ctx f, sortsUndefined)

undefinedSorts :: SortContext c => c -> VarsDecl -> ValExpression -> Set.Set Sort
undefinedSorts ctx ps b = let sortsDefined   = Set.fromList (elemsSort ctx)
                              sortsUsed      = Set.unions [ usedSorts ctx ps
                                                          , usedSorts (toVarContext ctx ps) b
                                                          ]
                            in sortsUsed `Set.difference` sortsDefined

-- | Helper function for construction: check for undefined variables per function definition
undefinedVariablesInFuncs :: SortContext c => c -> [FuncDef] -> [(FuncSignature, Set.Set (RefByName VarDef))]
undefinedVariablesInFuncs ctx = mapMaybe maybeUndefinedVariablesInFunc
    where
        maybeUndefinedVariablesInFunc :: FuncDef -> Maybe (FuncSignature, Set.Set (RefByName VarDef))
        maybeUndefinedVariablesInFunc f = let variablesUndefined = undefinedVariables (paramDefs f) (body f)
                                            in if Set.null variablesUndefined
                                                  then Nothing
                                                  else Just (getFuncSignature ctx f, variablesUndefined)

undefinedVariables :: VarsDecl -> ValExpression -> Set.Set (RefByName VarDef)
undefinedVariables ps b = let variablesDefined :: Set.Set (RefByName VarDef)
                              variablesDefined   = Set.fromList (map (RefByName . name) (toList ps))
                              variablesUsed      = freeVars b
                                in variablesUsed `Set.difference` variablesDefined

-- | constructor for FuncDef
mkFuncDef :: SortContext c => c -> FunctionName -> VarsDecl -> ValExpression -> Either Error FuncDef
mkFuncDef ctx n ps b | not (Set.null sortsUndefined) = Left $ Error ("Function definition has undefined sorts: " ++ show sortsUndefined)
                     | not (Set.null varsUndefined)  = Left $ Error ("Function definition has undefined variables in body: " ++ show varsUndefined)
                     | otherwise                     = Right $ FuncDef n ps b
    where
        sortsUndefined = undefinedSorts ctx ps b
        varsUndefined  = undefinedVariables ps b

instance UsedNames FuncDef where
    -- Notes:
    -- 1. A FunctionName is NOT a Name.
    -- 2. A valid definition uses only defined names, hence the body will only add duplicates.
    --    However, changes in ValExpr, such as the introduction of `let`, could make this assumption invalid.
    -- TODO: do we add the code? to be robust against this change? and to make ValExpr and BExpr more alike?
    usedNames = usedNames . paramDefs

-- TODO: how to refactor with sortsUsed in undefinedSorts to make this code only once?
instance SortContext c => UsedSorts c FuncDef where
     usedSorts ctx f  = let ps = paramDefs f
                            b  = body f
                          in Set.unions [ usedSorts ctx ps
                                        , usedSorts (toVarContext ctx ps) b
                                        ]

instance UsedFuncSignatures FuncDef where
    usedFuncSignatures = usedFuncSignatures . body

instance SortContext c => HasFuncSignature c FuncDef
    where
        getFuncSignature ctx (FuncDef fn ps bd) =
            case mkFuncSignature ctx fn (map (getSort ctx) (toList ps)) (getSort (toVarContext ctx ps) bd) of
                 Left e -> error ("getFuncSignature is unable to create FuncSignature" ++ show e)
                 Right x -> x

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
