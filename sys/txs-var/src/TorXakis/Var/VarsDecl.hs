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
import           Data.Maybe           (mapMaybe)
import qualified Data.Set             as Set
import qualified Data.Text            as T
import           GHC.Generics         (Generic)

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.PrettyPrint.TorXakis
import           TorXakis.Sort
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
mkVarsDecl :: SortContext c => c -> [VarDef] -> Either Error VarsDecl
mkVarsDecl ctx l | not $ null nuVars                = Left $ Error ("Non unique names: " ++ show nuVars)
                 | not $ null varsUndefinedSorts    = Left $ Error ("List of variables with undefined sorts: " ++ show varsUndefinedSorts)
                 | otherwise                        = Right $ VarsDecl l
    where
        nuVars :: [VarDef]
        nuVars = repeatedByName l

        maybeUndefinedSorts :: Set.Set Sort -> VarDef -> Maybe (VarDef, Set.Set Sort)
        maybeUndefinedSorts definedSorts vardef =
            let undefinedSorts = usedSorts ctx vardef `Set.difference` definedSorts in
                if null undefinedSorts
                    then Nothing
                    else Just (vardef,undefinedSorts)

        varsUndefinedSorts :: [(VarDef, Set.Set Sort)]
        varsUndefinedSorts = let definedSorts = Set.fromList (elemsSort ctx) in
                                 mapMaybe (maybeUndefinedSorts definedSorts) l

instance UsedSorts c VarsDecl where
    usedSorts ctx (VarsDecl l) = Set.unions $ map (usedSorts ctx) l

instance PrettyPrint c VarsDecl where
    prettyPrint o c vs = TxsString (T.concat [ T.pack "( "
                                             , T.intercalate sepParam (map (TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o c) (toList vs))
                                             , close
                                             ])
        where sepParam = if multiline o then T.pack "\n, "
                                        else T.pack ", "
              close = if multiline o then T.pack "\n)"
                                     else T.pack ")"
