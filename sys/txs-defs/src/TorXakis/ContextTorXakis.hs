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
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ContextTorXakis
( -- * Context TorXakis
  ContextTorXakis
, TorXakis.ContextTorXakis.empty
)
where
import           Data.Data              (Data)
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as Set
import           GHC.Generics           (Generic)

import           TorXakis.Error         ( Error ( Error ) )
import           TorXakis.Language
import           TorXakis.Name
import           TorXakis.Sort          ( Sort ( SortBool )
                                        , ADTDef
                                        , adtName
                                        , elemsConstructor
                                        , ConstructorDef
                                        , elemsField
                                        , FieldDef
                                        , sort
                                        )
import           TorXakis.SortContext
import           TorXakis.FuncContext

import           TorXakis.ContextFunc   -- for now

-- | The TorXakis Context.
newtype ContextTorXakis = ContextTorXakis { innerContext :: ContextFunc
                                          } deriving (Eq, Ord, Read, Show, Generic, Data)

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
    
    elemsFunc = elemFunc . innerContext
    
    addFuncs fs (ContextTorXakis ctx)
        | not $ null predefinedNonSolvableFuncSignature = Left $ Error ("Functions with signature equal to a predefined function: " ++ show predefinedNonSolvableFuncSignature)
        | not $ null reservedFuncSignature              = Left $ Error ("Functions with signature equal to a reserved function: " ++ show reservedFuncSignature)
        | otherwise                                     = case addFuncs fs ctx of
                                                               Left e     -> Left e
                                                               Right nctx -> Right $ ContextTorXakis nctx
        where
            {-
            argSorts :: [Sort]
            argSorts = map (getSort ctx) vs

            retSort :: Sort
            retSort = getSort varContext b

            signature :: FuncSignature
            signature = case mkFuncSignature ctx n argSorts retSort of
                            Left e  -> error ("mkFuncDef is unable to create FuncSignature" ++ show e)
                            Right f -> f
            -}
            
            -- | is Predefined NonSolvable Function Signature?
            -- One can make funcSignatures for Predefined NonSolvable Functions
            -- However these funcSignature can't be added to a Func(Signature)Context.
            isPredefinedNonSolvableFuncSignature :: FuncSignature -> Bool
            isPredefinedNonSolvableFuncSignature f =
                case (TorXakis.FunctionName.toString (funcName f), args f,                   returnSort f ) of
                     ("toString",                                  [_],                      SortString   ) -> True  -- toString with single argument is predefined for all Sorts
                     ("fromString",                                [SortString],             _            ) -> True
                     ("toXML",                                     [_],                      SortString   ) -> True
                     ("fromXML",                                   [SortString],             _            ) -> True
                     ("takeWhile",                                 [SortString, SortString], SortString   ) -> True
                     ("takeWhileNot",                              [SortString, SortString], SortString   ) -> True
                     ("dropWhile",                                 [SortString, SortString], SortString   ) -> True
                     ("dropWhileNot",                              [SortString, SortString], SortString   ) -> True
                     _                                                                                      -> False

            -- | isReservedFunctionSignature includes
            --
            -- * TorXakis FuncSignatures that are mapped onto special constructors
            --
            -- * FuncSignatures that are implicitly defined by defining Sorts / ADTDefs
            isReservedFunctionSignature :: SortContext c => c -> FunctionName -> [Sort] -> Sort -> Bool
            isReservedFunctionSignature ctx n ss s =    isMappedFuncSignature
                                                     || isSortFuncSignature
                where
                    txsFuncName :: TxsString
                    txsFuncName = TxsString (TorXakis.FunctionName.toText n)

                    isMappedFuncSignature :: Bool
                    isMappedFuncSignature =
                        case (txsFuncName,              ss,                         s          ) of
                             (txsOperatorEqual,         [a,b],                      SortBool   ) -> a == b   -- equality is defined for all types
                             (txsOperatorNotEqual,      [a,b],                      SortBool   ) -> a == b   -- not equality is defined for all types
                             (txsFunctionNot,           [SortBool],                 SortBool   ) -> True
                             (txsOperatorAnd,           [SortBool, SortBool],       SortBool   ) -> True
                             (txsOperatorOr,            [SortBool, SortBool],       SortBool   ) -> True
                             (txsOperatorXor,           [SortBool, SortBool],       SortBool   ) -> True
                             (txsOperatorImplies,       [SortBool, SortBool],       SortBool   ) -> True
                             (txsFunctionAbs,           [SortInt],                  SortInt    ) -> True
                             (txsOperatorUnaryPlus,     [SortInt],                  SortInt    ) -> True
                             (txsOperatorUnaryMinus,    [SortInt],                  SortInt    ) -> True
                             (txsOperatorPlus,          [SortInt, SortInt],         SortInt    ) -> True
                             (txsOperatorMinus,         [SortInt, SortInt],         SortInt    ) -> True
                             (txsOperatorTimes,         [SortInt, SortInt],         SortInt    ) -> True
                             (txsOperatorDivide,        [SortInt, SortInt],         SortInt    ) -> True
                             (txsOperatorModulo,        [SortInt, SortInt],         SortInt    ) -> True
                             (txsOperatorPower,         [SortInt, SortInt],         SortInt    ) -> True
                             (txsOperatorLessThan,      [SortInt, SortInt],         SortBool   ) -> True
                             (txsOperatorLessEqual,     [SortInt, SortInt],         SortBool   ) -> True
                             (txsOperatorGreaterEqual,  [SortInt, SortInt],         SortBool   ) -> True
                             (txsOperatorGreaterThan,   [SortInt, SortInt],         SortBool   ) -> True
                             (txsFunctionLength,        [SortString],               SortInt    ) -> True
                             (txsOperatorConcat,        [SortString, SortString],   SortString ) -> True
                             (txsFunctionAt,            [SortString, SortInt],      SortString ) -> True
                             (txsFunctionStringInRegex, [SortString, SortRegex],    SortBool   ) -> True
                             _                                                                   -> False

                    isSortFuncSignature :: Bool
                    isSortFuncSignature = equalsConstructor || equalsIsConstructor || equalsFieldAccess

                    -- | exists constructor : funcName == cstrName && same arguments && same returnSort (an ADT)
                    equalsConstructor :: Bool
                    equalsConstructor =
                        case s of
                            SortADT a -> case lookupADT (toName a) ctx of
                                              Nothing   -> error ("equalsConstructor -- ADTDef " ++ show a ++ " not defined in context ")
                                              Just aDef -> any (\c -> txsFuncName == txsFuncNameConstructor c && ss == map sort (elemsField c)) (elemsConstructor aDef)
                            _         -> False

                    -- | exists constructor : returnSort func == Bool && funcName == isCstrName
                    equalsIsConstructor :: Bool
                    equalsIsConstructor =
                        (s == SortBool) && case ss of
                                             [SortADT a] -> case lookupADT (toName a) ctx of
                                                                Nothing   -> error ("equalsIsConstructor -- ADTDef " ++ show a ++ " not defined in context ")
                                                                Just aDef -> any (\c -> txsFuncName == txsFuncNameIsConstructor c) (elemsConstructor aDef)
                                             _           -> False

                    -- | exists field : funcName == fieldName && funcReturnSort == fieldSort
                    equalsFieldAccess :: Bool
                    equalsFieldAccess =
                        case ss of
                            [SortADT a] -> case lookupADT (toName a) ctx of
                                            Nothing   -> error ("equalsFieldAccess -- ADTDef " ++ show a ++ " not defined in context ")
                                            Just aDef -> any (any (\f -> txsFuncName == txsFuncNameField f && s == sort f) . elemsField) (elemsConstructor aDef) 
                            _           -> False

