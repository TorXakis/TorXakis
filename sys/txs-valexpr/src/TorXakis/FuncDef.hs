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
, TorXakis.FuncDef.funcName
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

import           TorXakis.ContextValExprConstruction
import           TorXakis.Error
import           TorXakis.FreeVars
import           TorXakis.FuncSignature
import           TorXakis.FuncSignatureContext
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.VarContext
import           TorXakis.VarDef
import           TorXakis.VarsDecl
import           TorXakis.ValExpr.ValExpr

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

-- | constructor for FuncDef
-- TODO: what should be checked here?
--       * also checkbody?
--       * Don't check sort (is already done to construct VarsDecl)?
mkFuncDef :: FuncSignatureContext a => a -> Name -> VarsDecl -> ValExpression -> Either Error FuncDef
mkFuncDef ctx n ps b | not (Set.null undefinedVars) = Left $ Error (T.pack ("Undefined variables used in body " ++ show undefinedVars))
                     | not (null undefinedSorts)    = Left $ Error (T.pack ("Variables have undefined sorts " ++ show undefinedSorts))
                     | otherwise                    = Right $ FuncDef n ps b
    where
        undefinedVars :: Set.Set (RefByName VarDef)
        undefinedVars = Set.difference (freeVars b) (Set.fromList (map toRefByName (toList ps)))

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . memberSort ctx . TorXakis.VarDef.sort) (toList ps)

instance FuncSignatureContext a => HasFuncSignature a FuncDef
    where
        getFuncSignature sctx (FuncDef fn ps bd) = 
            let vs = toList ps in
                case addVars (fromFuncSignatureContext sctx) vs of
                     Left e -> error ("getFuncSignature is unable to make new context" ++ show e)
                     Right vctx -> case mkFuncSignature sctx fn (map (getSort sctx) vs) (getSort vctx bd) of
                                         Left e -> error ("getFuncSignature is unable to create FuncSignature" ++ show e)
                                         Right x -> x

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
