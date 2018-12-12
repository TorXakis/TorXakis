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
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TorXakis.FuncDef
( FuncDefView(..)
, FuncDef
, view
, mkFuncDef
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.VarDef
import           TorXakis.ValExpr.ValExpr (ValExpression)

-- | Data structure to store the information of a Function Definition:
-- * A Name
-- * A list of variables
-- * A body (possibly using the variables)
data FuncDefView v = FuncDefView { -- | The name of the function (of type 'TorXakis.Name')
                                   funcName :: Name
                                   -- | The function parameter definitions
                                 , paramDefs :: [v]
                                   -- | The body of the function
                                 , body :: ValExpression v
                                 }
     deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

newtype FuncDef v = FuncDef { -- | view on FuncDef
                              view :: FuncDefView v
                            }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- constructor for FuncDef
mkFuncDef :: forall v . VarDef v => Name -> [v] -> ValExpression v -> Either MinError (FuncDef v)
mkFuncDef n ps b | not $ null nonUniqueNames       = Left $ MinError (T.pack ("Non unique names : " ++ show nonUniqueNames))
                 | otherwise                       = Right $ FuncDef (FuncDefView n ps b)
    where
        nonUniqueNames :: [v]
        nonUniqueNames = repeatedByName ps

instance VarDef v => HasFuncSignature (FuncDef v)
    where
        getFuncSignature = getFuncSignature . view

instance VarDef v => HasFuncSignature (FuncDefView v)
    where
        getFuncSignature (FuncDefView fn pds bd) = FuncSignature fn (map getSort pds) (getSort bd)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
