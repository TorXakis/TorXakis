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
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.FuncDef
( FuncDef
, funcName
, paramDefs
, body
, mkFuncDef
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           TorXakis.Error
import           TorXakis.FreeVars
import           TorXakis.FuncSignature (FuncSignature(FuncSignature), HasFuncSignature(..))
import           TorXakis.Name
import           TorXakis.Sort (getSort, elemSort, SortContext)
import           TorXakis.VarContext
import           TorXakis.VarDef
import           TorXakis.VarsDecl
import           TorXakis.ValExpr.ValExpr (ValExpression)

-- | Data structure to store the information of a Function Definition:
-- * A Name
-- * A list of variables
-- * A body (possibly using the variables)
data FuncDef = FuncDef { -- | The name of the function (of type 'TorXakis.Name')
                         funcName :: Name
                         -- | The function parameter definitions
                       , paramDefs :: VarsDecl
                         -- | The body of the function
                       , body :: ValExpression
                       }
     deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

-- constructor for FuncDef
mkFuncDef :: SortContext a => a -> Name -> VarsDecl -> ValExpression -> Either MinError FuncDef
mkFuncDef ctx n ps b | not (Set.null undefinedVars) = Left $ MinError (T.pack ("Undefined variables used in body " ++ show undefinedVars))
                     | not (null undefinedSorts)    = Left $ MinError (T.pack ("Variables have undefined sorts " ++ show undefinedSorts))
                     | otherwise                    = Right $ FuncDef n ps b
    where
        undefinedVars :: Set.Set (RefByName VarDef)
        undefinedVars = Set.difference (freeVars b) (Set.fromList (map (RefByName . name) (TorXakis.VarsDecl.toList ps)))

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . elemSort ctx . sort) (toList ps)

instance SortContext a => HasFuncSignature a FuncDef
    where
        getFuncSignature sctx (FuncDef fn pds bd) = case addVarDefs (fromSortContext sctx) (toList pds) of
                                                        Left e     -> error ("getFuncSignature is unable to add vars to sort context" ++ show e)
                                                        Right vctx -> FuncSignature fn (map (getSort sctx) (TorXakis.VarsDecl.toList pds)) (getSort vctx bd)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
