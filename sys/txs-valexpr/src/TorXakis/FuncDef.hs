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
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.Set            as Set
import           GHC.Generics        (Generic)

import           TorXakis.ContextValExprConstruction
import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.FuncSignatureContext
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.VarContext
import           TorXakis.Var
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

sortOfBody :: FuncSignatureContext a => a -> [VarDef] -> ValExpression -> Sort
sortOfBody ctx vs b =
    case addVars (fromFuncSignatureContext ctx) vs of
        Left e      -> error ("sortOfBody is unable to make new context" ++ show e)
        Right vctx  -> getSort vctx b

-- | constructor for FuncDef
-- TODO: what should be checked here?
--       * also checkbody?
--       * FreeVars of body are subset of VarsDecl?
--       * Don't check sort (is already done to construct VarsDecl)?
mkFuncDef :: FuncSignatureContext a => a -> Name -> VarsDecl -> ValExpression -> Either Error FuncDef
mkFuncDef ctx n ps b | not (Set.null undefinedVars)                             = Left $ Error ("Undefined variables used in body " ++ show undefinedVars)
                     | not (null undefinedSorts)                                = Left $ Error ("Variables have undefined sorts " ++ show undefinedSorts)
                     | not (isReservedFuncSignature ctx n argSorts retSort)     = Left $ Error ("Function has reserved signature " ++ show n ++ " " ++ show argSorts ++ " " ++ show retSort)
                     | not (isPredefinedNonSolvableFuncSignature signature)     = Left $ Error ("Function has predefined signature " ++ show n ++ " " ++ show argSorts ++ " " ++ show retSort)
                     | otherwise                                                = Right $ FuncDef n ps b
    where
        vs :: [VarDef]
        vs = toList ps
        
        undefinedVars :: Set.Set (RefByName VarDef)
        undefinedVars = Set.difference (freeVars b) (Set.fromList (map toRefByName vs))

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . memberSort ctx . TorXakis.Var.sort) vs

        argSorts :: [Sort]
        argSorts = map (getSort ctx) vs

        retSort :: Sort
        retSort = sortOfBody ctx vs b

        signature :: FuncSignature
        signature = case mkFuncSignature ctx n argSorts retSort of
                        Left e  -> error ("mkFuncDef is unable to create FuncSignature" ++ show e)
                        Right f -> f

instance forall a . FuncSignatureContext a => HasFuncSignature a FuncDef
    where
        getFuncSignature ctx (FuncDef fn ps bd) =
            let vs = toList ps in
                case mkFuncSignature ctx fn (map (getSort ctx) vs) (sortOfBody ctx vs bd) of
                     Left e -> error ("getFuncSignature is unable to create FuncSignature" ++ show e)
                     Right x -> x

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
