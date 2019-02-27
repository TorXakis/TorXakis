{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  VarsDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Variables Declarations
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.Var.VarsDecl
( VarsDecl
, mkVarsDecl
, toList
)
where

import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import qualified Data.Text            as T
import           GHC.Generics         (Generic)

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.PrettyPrint.TorXakis
import           TorXakis.SortContext
import           TorXakis.Var.VarDef

-- | Data for a variables declarations.
--   The order of the variables declarations is relevant.
newtype VarsDecl = VarsDecl { -- | toList
                                toList :: [VarDef]
                        }
         deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | smart constructor for VarsDecl
-- Error is returned when 
--
-- * Any Sort of the variables is not defined within context.
--
-- * Variable Names are not unique.
--
mkVarsDecl :: SortContext a => a -> [VarDef] -> Either Error VarsDecl
mkVarsDecl ctx l | not $ null nuVars            = Left $ Error ("Non unique names: " ++ show nuVars)
                 | not $ null undefinedSorts    = Left $ Error ("List of variables with undefined sorts: " ++ show undefinedSorts)
                 | otherwise                    = Right $ VarsDecl l
    where
        nuVars :: [VarDef]
        nuVars = repeatedByName l

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . flip memberSort ctx . sort) l

instance PrettyPrint a VarsDecl where
    prettyPrint o c vs = TxsString (T.concat [ T.pack "( "
                                             , T.intercalate sepParam (map (TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o c) (toList vs))
                                             , close
                                             ])
        where sepParam = if multiline o then T.pack "\n, "
                                        else T.pack ", "
              close = if multiline o then T.pack "\n)"
                                     else T.pack ")"
