{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncSignature
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Kerem Ispirli <kerem.ispirli@tno.nl>
--                Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the data structure for a Signature of a Function.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.FuncSignature.FuncSignature
( -- * Function Signature
  FuncSignature (funcName, args, returnSort)
, mkFuncSignature
, isPredefinedNonSolvableFuncSignature
, isReservedFunctionSignature
  -- * Has Function Signature class
, HasFuncSignature (..)
  -- ** Conversion List to Map By Function Signature
, toMapByFuncSignature
-- ** Repeated Function Signatures functions
, repeatedByFuncSignature
, repeatedByFuncSignatureIncremental
  -- dependencies, yet part of interface
, Error
, FunctionName
, Sort
) where
import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hashWithSalt))
import           Data.HashMap         (Map, fromList)
import           Data.List.Unique     (repeated)
import           GHC.Generics         (Generic)

import           TorXakis.Error
import           TorXakis.Name        (toName, toText)
import           TorXakis.FunctionName(FunctionName, mkFunctionName, toString)
import           TorXakis.Sort
import           TorXakis.SortContext

-- | A generalized, type-safe reference.
data FuncSignature = FuncSignature { -- | The 'Name' of the function/operator.
                                     funcName :: FunctionName
                                     -- | The 'Sort's of the function arguments.
                                   , args :: [Sort]
                                     -- | The 'Sort' of the result that the function returns.
                                   , returnSort :: Sort
                                   }
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

-- | is Predefined NonSolvable Function Signature?
-- One can make funcSignatures for Predefined NonSolvable Functions
-- However these funcSignature can't be added to a Func(Signature)Context.
isPredefinedNonSolvableFuncSignature :: FuncSignature -> Bool
isPredefinedNonSolvableFuncSignature f =
    case (toString (funcName f), args f,                   returnSort f ) of
         ("toString",            [_],                      SortString   ) -> True  -- toString with single argument is predefined for all Sorts
         ("fromString",          [SortString],             _            ) -> True
         ("toXML",               [_],                      SortString   ) -> True
         ("fromXML",             [SortString],             _            ) -> True
         ("takeWhile",           [SortString, SortString], SortString   ) -> True
         ("takeWhileNot",        [SortString, SortString], SortString   ) -> True
         ("dropWhile",           [SortString, SortString], SortString   ) -> True
         ("dropWhileNot",        [SortString, SortString], SortString   ) -> True
         _                                                                -> False

-- | isReservedFunctionSignature includes
--
-- * TorXakis FuncSignatures that are mapped onto special constructors
--
-- * FuncSignatures that are implicitly defined by defining Sorts / ADTDefs
isReservedFunctionSignature :: SortContext c => c -> FunctionName -> [Sort] -> Sort -> Bool
isReservedFunctionSignature ctx n ss s =    isMappedFuncSignature
                                         || isSortFuncSignature
  where
    isMappedFuncSignature :: Bool
    isMappedFuncSignature =
        case (toString n,    ss,                         s          ) of
             ("==",          [a,b],                      SortBool   ) -> a == b   -- equality is defined for all types
             ("<>",          [a,b],                      SortBool   ) -> a == b   -- not equality is defined for all types
             ("not",         [SortBool],                 SortBool   ) -> True
             ("/\\",         [SortBool, SortBool],       SortBool   ) -> True
             ("\\/",         [SortBool, SortBool],       SortBool   ) -> True
             ("\\|/",        [SortBool, SortBool],       SortBool   ) -> True
             ("=>",          [SortBool, SortBool],       SortBool   ) -> True
             ("abs",         [SortInt],                  SortInt    ) -> True
             ("+",           [SortInt],                  SortInt    ) -> True
             ("-",           [SortInt],                  SortInt    ) -> True
             ("+",           [SortInt, SortInt],         SortInt    ) -> True
             ("-",           [SortInt, SortInt],         SortInt    ) -> True
             ("*",           [SortInt, SortInt],         SortInt    ) -> True
             ("/",           [SortInt, SortInt],         SortInt    ) -> True
             ("%",           [SortInt, SortInt],         SortInt    ) -> True
             ("<",           [SortInt, SortInt],         SortBool   ) -> True
             ("<=",          [SortInt, SortInt],         SortBool   ) -> True
             (">=",          [SortInt, SortInt],         SortBool   ) -> True
             (">",           [SortInt, SortInt],         SortBool   ) -> True
             ("len",         [SortString],               SortInt    ) -> True
             ("++",          [SortString, SortString],   SortString ) -> True
             ("at",          [SortString, SortInt],      SortString ) -> True
             ("strinre",     [SortString, SortRegex],    SortBool   ) -> True
             _                                                        -> False

    isSortFuncSignature :: Bool
    isSortFuncSignature =
        case ss of
             [SortADT a] -> case lookupADT (toName a) ctx of
                                Nothing   -> error ("isReservedFunctionSignature -- ADTDef " ++ show a ++ " not defined in context ")
                                Just aDef -> equalsIsConstructorFunc aDef || equalsAccessorFunc aDef
             _           -> False

    -- | exists constructor : funcName == isCstrName
    equalsIsConstructorFunc :: ADTDef -> Bool
    equalsIsConstructorFunc aDef =
           s == SortBool 
        && any (\c -> n == functionNameIsConstructor c) (elemsConstructor aDef)

    -- | exists field : funcName == fieldName && funcReturnSort == fieldSort
    equalsAccessorFunc :: ADTDef -> Bool
    equalsAccessorFunc aDef =
        any (any (\f -> functionNameField f == n && sort f == s) . elemsField) (elemsConstructor aDef) 
      where
        functionNameField :: FieldDef -> FunctionName
        functionNameField f =
            case mkFunctionName (toText (fieldName f)) of
                Left e   -> error ("functionNameField failed on field " ++ show f ++ " with " ++ show e)
                Right fn -> fn

-- | Smart constructor for 'TorXakis.FuncSignature.FuncSignature'.
--   A FuncSignature is returned when the following constraints are satisfied:
--
--   * FuncSignature is not a reserved signature (both default and depending on sort).
--
--   * Sorts of arguments and return value are defined.
--
--   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
mkFuncSignature :: SortContext a => a -> FunctionName -> [Sort] -> Sort -> Either Error FuncSignature
mkFuncSignature ctx n as s | not $ null undefinedSorts              = Left $ Error ("mkPrefixFuncSignature: Arguments have undefined sorts " ++ show undefinedSorts)
                           | isReservedFunctionSignature ctx n as s = Left $ Error ("mkPrefixFuncSignature: Reserved function signature " ++ show n ++ " " ++ show as ++ " " ++ show s)
                           | memberSort s ctx                       = Right $ FuncSignature n as s
                           | otherwise                              = Left $ Error ("mkPrefixFuncSignature: Return sort has undefined sort " ++ show s)
    where
        undefinedSorts :: [Sort]
        undefinedSorts = filter (not . flip memberSort ctx) as

-- | Enables 'FuncSignature's of entities to be accessed in a common way.
class HasFuncSignature c e where
    -- | return the function signature of the given element
    getFuncSignature :: c -> e -> FuncSignature

instance HasFuncSignature a FuncSignature where
    getFuncSignature _ = id

instance Hashable FuncSignature where
    s `hashWithSalt` (FuncSignature n as r) = s `hashWithSalt`
                                              n `hashWithSalt`
                                              as `hashWithSalt`
                                              r

-- | Return 'Data.HashMap.Map' where the 'FuncSignature' of the element is taken as key
--   and the element itself is taken as value.
toMapByFuncSignature :: HasFuncSignature c a => c -> [a] -> Map FuncSignature a
toMapByFuncSignature ctx = fromList . map (\e -> (getFuncSignature ctx e,e))

-- |  Return the elements with non-unique function signatures that the second list contains in the combination of the first and second list.
repeatedByFuncSignatureIncremental :: (HasFuncSignature c a, HasFuncSignature c b) => c -> [a] -> [b] -> [b]
repeatedByFuncSignatureIncremental ctx xs ys = filter ((`elem` nuFuncSignatures) . getFuncSignature ctx) ys
    where nuFuncSignatures = repeated $ map (getFuncSignature ctx) xs ++ map (getFuncSignature ctx) ys

-- | Return the elements with non-unique function signatures: 
-- the elements with a 'FuncSignature' that is present more than once in the list.
repeatedByFuncSignature :: (HasFuncSignature c a) => c -> [a] -> [a]
repeatedByFuncSignature ctx = repeatedByFuncSignatureIncremental ctx ([] :: [FuncSignature])