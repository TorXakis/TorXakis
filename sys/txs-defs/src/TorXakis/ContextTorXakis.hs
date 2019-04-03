{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ContextTorXakis
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- The TorXakis Context containing all definitions.
-- The TorXakis Context adds additional realization constraints,
-- including
--
-- * Prevent name clashes with implicit functions from Sorts
--
-- * Prevent name clashes with keywords
--
-- * Prevent function signature clashes with predefined functions and operators
--
-- * Enable round tripping
-----------------------------------------------------------------------------
module TorXakis.ContextTorXakis
( -- * Context TorXakis
  ContextTorXakis
, TorXakis.ContextTorXakis.empty
)
where
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as Set

import           TorXakis.ContextFunc   -- for now
import           TorXakis.Error         ( Error ( Error ) )
import           TorXakis.FuncSignature
import           TorXakis.Language
import           TorXakis.Name
import           TorXakis.Sort          ( Sort ( .. )
                                        , ADTDef
                                        , adtName
                                        , elemsConstructor
                                        , ConstructorDef
                                        , elemsField
                                        , FieldDef
                                        , sort
                                        )



-- | The TorXakis Context.
newtype ContextTorXakis = ContextTorXakis { innerContext :: ContextFunc
                                          }

-- | Constructor of empty ContextTorXakis
empty :: ContextTorXakis
empty = ContextTorXakis TorXakis.ContextFunc.empty

instance SortContext ContextTorXakis where
    memberSort r = memberSort r . innerContext

    memberADT r = memberADT r . innerContext

    lookupADT r = lookupADT r . innerContext

    elemsADT = elemsADT . innerContext

    -- Add additional checks to make round tripping possible.
    --
    -- * Names are no keywords -- TODO
    --
    -- * Implicit functions have distinct function signatures:
    --   for each constructor TorXakis adds 'isCstr' :: X -> Bool      function which should not conflict with
    --              the accessor function   'field'  :: X -> SortField
    -- hence for round tripping we need to check that fields of type Bool don't have a name equal to any isCstr.
    addADTs as (ContextTorXakis ctx)
            | not $ null conflictADTDefs = Left $ Error ("ADTDefs with conflicts between Boolean Field and implicit isConstructor function: " ++ show conflictADTDefs)
            | not $ null keywordADTDefs  = Left $ Error ("ADTDefs that use the given TorXakis keywords: " ++ show keywordADTDefs)
            | otherwise                  = case addADTs as ctx of
                                                Left e     -> Left e
                                                Right sctx -> Right $ ContextTorXakis sctx
      where
        conflictADTDefs :: [(Name, Set.Set TxsString)]
        conflictADTDefs = mapMaybe maybeADTConflict as

        maybeADTConflict :: ADTDef -> Maybe (Name, Set.Set TxsString)
        maybeADTConflict a = if Set.null conflictFieldNames
                                then Nothing
                                else Just (adtName a, conflictFieldNames)
            where
                conflictFieldNames :: Set.Set TxsString
                conflictFieldNames = Set.filter hasBoolFieldNameConflict allBoolFieldNames

                -- | A Name of Bool Field has a conflict when it has the same textual representation as the implicit is-made-by-constructor function
                hasBoolFieldNameConflict :: TxsString -> Bool
                hasBoolFieldNameConflict n = n `elem` isConstructorNames

                isConstructorNames :: [TxsString]
                isConstructorNames = map txsFuncNameIsConstructor cs

                cs :: [ConstructorDef]
                cs = elemsConstructor a

                allFields :: [FieldDef]
                allFields = concatMap elemsField cs

                allBoolFields :: [FieldDef]
                allBoolFields = filter boolField allFields

                allBoolFieldNames :: Set.Set TxsString
                allBoolFieldNames = Set.fromList (map txsFuncNameField allBoolFields)

                boolField :: FieldDef -> Bool
                boolField f = sort f == SortBool

        keywordADTDefs :: [(Name, Set.Set Name)]
        keywordADTDefs = mapMaybe maybeADTKeyword as

        maybeADTKeyword :: ADTDef -> Maybe (Name, Set.Set Name)
        maybeADTKeyword a = let actualNames = usedNames a
                                reservedUsed = Set.filter (isTxsReserved . TorXakis.Name.toText) actualNames
                              in
                                if Set.null reservedUsed
                                    then Nothing
                                    else Just (adtName a, reservedUsed)

instance FuncContext ContextTorXakis where
    memberFunc f = memberFunc f . innerContext

    lookupFunc f = lookupFunc f . innerContext

    funcSignatures = funcSignatures . innerContext

    elemsFunc = elemsFunc . innerContext

    addFuncs fs (ContextTorXakis ctx)
        | not $ null predefinedNonSolvableFuncSignature = Left $ Error ("Functions with signature equal to a predefined function: " ++ show predefinedNonSolvableFuncSignature)
        | not $ null reservedFuncSignature              = Left $ Error ("Functions with signature equal to a reserved function: " ++ show reservedFuncSignature)
        | otherwise                                     = case addFuncs fs ctx of
                                                               Left e     -> Left e
                                                               Right nctx -> Right $ ContextTorXakis nctx
        where
            predefinedNonSolvableFuncSignature = filter (isPredefinedNonSolvableFuncSignature . getFuncSignature ctx) fs
            reservedFuncSignature = filter (isReservedFunctionSignature ctx . getFuncSignature ctx) fs
