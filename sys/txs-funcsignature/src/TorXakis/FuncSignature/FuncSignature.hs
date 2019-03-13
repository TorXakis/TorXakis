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
( -- * Function name
  FuncName (..)
, toText
  -- * Function Signature
, FuncSignature (funcName, args, returnSort)
, mkPrefixFuncSignature
, isPredefinedNonSolvableFuncSignature
, isReservedPrefixFunctionSignature
, mkInfixFuncSignature
, isReservedInfixFunctionSignature
  -- * Has Function Signature class
, HasFuncSignature (..)
  -- ** Conversion List to Map By Function Signature
, toMapByFuncSignature
-- ** Repeated Function Signatures functions
, repeatedByFuncSignature
, repeatedByFuncSignatureIncremental
  -- dependencies, yet part of interface
, Error
, TorXakis.Name.Name
, TorXakis.OperatorName.OperatorName
, Sort
) where
import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hashWithSalt))
import           Data.HashMap         (Map, fromList)
import           Data.List.Unique     (repeated)
import qualified Data.Text            as T
import           GHC.Generics         (Generic)

import           TorXakis.Error
import qualified TorXakis.Name
import qualified TorXakis.OperatorName
import           TorXakis.Sort
import           TorXakis.SortContext

-- | Function Name is either a 'TorXakis.Name' or an 'TorXakis.OperatorName'.
data FuncName = NamePrefix TorXakis.Name.Name
              | NameInfix  TorXakis.OperatorName.OperatorName
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

instance Hashable FuncName where
    s `hashWithSalt` (NamePrefix n) = s `hashWithSalt` n
    s `hashWithSalt` (NameInfix  n) = s `hashWithSalt` n

-- | toText
toText :: FuncName -> T.Text
toText (NamePrefix n) = TorXakis.Name.toText n
toText (NameInfix  n) = TorXakis.OperatorName.toText n

-- | A generalized, type-safe reference.
data FuncSignature = FuncSignature { -- | The 'Name' of the function/operator.
                                     funcName :: FuncName
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
    let str = T.unpack (toText (funcName f)) in
        case (str,            args f,                   returnSort f ) of
             ("toString",     [_],                      SortString   ) -> True  -- toString with single argument is predefined for all Sorts
             ("fromString",   [SortString],             _            ) -> True
             ("toXML",        [_],                      SortString   ) -> True
             ("fromXML",      [SortString],             _            ) -> True
             ("takeWhile",    [SortString, SortString], SortString   ) -> True
             ("takeWhileNot", [SortString, SortString], SortString   ) -> True
             ("dropWhile",    [SortString, SortString], SortString   ) -> True
             ("dropWhileNot", [SortString, SortString], SortString   ) -> True
             _                                                         -> False

-- | is made by Constructor Function Name
-- needed for round tripping: TorXakis maps this operator on a function.
isConstructorFuncName :: ConstructorDef -> TorXakis.Name.Name
isConstructorFuncName c = case TorXakis.Name.mkName (T.append (T.pack "is") (TorXakis.Name.toText (constructorName c))) of
                                Left e -> error ("isConstructorFuncName failed on constructor " ++ show c ++ " with " ++ show e)
                                Right n -> n

-- | isReservedInfixFunctionSignature
-- Includes 
-- * TorXakis Operator Signatures that are mapped onto special constructors
--
-- * Operator Signatures that are implicitly defined by defining Sorts / ADTDefs
isReservedInfixFunctionSignature :: c -> TorXakis.OperatorName.OperatorName -> [Sort] -> Sort -> Bool
isReservedInfixFunctionSignature _ n ss s =    isMappedOperatorSignature
  where
    isMappedOperatorSignature :: Bool
    isMappedOperatorSignature =
        let str = T.unpack (TorXakis.OperatorName.toText n) in
            case (str,           ss,                         s          ) of
                 ("==",          [a,b],                      SortBool   ) -> a == b   -- equality is defined for all types
                 ("<>",          [a,b],                      SortBool   ) -> a == b   -- not equality is defined for all types
                 ("/\\",         [SortBool, SortBool],       SortBool   ) -> True
                 ("\\/",         [SortBool, SortBool],       SortBool   ) -> True
                 ("\\|/",        [SortBool, SortBool],       SortBool   ) -> True
                 ("=>",          [SortBool, SortBool],       SortBool   ) -> True
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
                 ("++",          [SortString, SortString],   SortString ) -> True
                 _                                                        -> False

-- | isReservedPrefixFunctionSignature
-- Includes 
-- * TorXakis FuncSignatures that are mapped onto special constructors
--
-- * FuncSignatures that are implicitly defined by defining Sorts / ADTDefs
isReservedPrefixFunctionSignature :: SortContext c => c -> TorXakis.Name.Name -> [Sort] -> Sort -> Bool
isReservedPrefixFunctionSignature ctx n ss s =    isMappedFuncSignature
                                     || isSortFuncSignature
  where
    isMappedFuncSignature :: Bool
    isMappedFuncSignature =
        let str = T.unpack (TorXakis.Name.toText n) in
            case (str,           ss,                         s          ) of
                 ("not",         [SortBool],                 SortBool   ) -> True
                 ("abs",         [SortInt],                  SortInt    ) -> True
                 ("len",         [SortString],               SortInt    ) -> True
                 ("at",          [SortString, SortInt],      SortString ) -> True
                 ("strinre",     [SortString, SortRegex],    SortBool   ) -> True
                 _                                                        -> False

    isSortFuncSignature :: Bool
    isSortFuncSignature =
        case ss of
             [SortADT a] -> case lookupADT (TorXakis.Name.toName a) ctx of
                                Nothing   -> error ("isReservedPrefixFunctionSignature -- ADTDef " ++ show a ++ " not defined in context ")
                                Just aDef -> equalsIsConstructorFunc aDef || equalsAccessorFunc aDef
             _           -> False

    -- | exists constructor : funcName == isCstrName
    equalsIsConstructorFunc :: ADTDef -> Bool
    equalsIsConstructorFunc aDef =
           s == SortBool 
        && any (\c -> n == isConstructorFuncName c) (elemsConstructor aDef)

    -- | exists field : funcName == fieldName && funcReturnSort == fieldSort
    equalsAccessorFunc :: ADTDef -> Bool
    equalsAccessorFunc aDef =
        any (any (\f -> fieldName f == n && sort f == s) . elemsField) (elemsConstructor aDef) 

-- | Smart constructor for 'TorXakis.FuncSignature.FuncSignature'.
--   A FuncSignature is returned when the following constraints are satisfied:
--
--   * FuncSignature is not a reserved signature (both default and depending on sort).
--
--   * Sorts of arguments and return value are defined.
--
--   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
mkPrefixFuncSignature :: SortContext a => a -> TorXakis.Name.Name -> [Sort] -> Sort -> Either Error FuncSignature
mkPrefixFuncSignature ctx n as s | not $ null undefinedSorts                    = Left $ Error ("mkPrefixFuncSignature: Arguments have undefined sorts " ++ show undefinedSorts)
                                 | isReservedPrefixFunctionSignature ctx n as s = Left $ Error ("mkPrefixFuncSignature: Reserved function signature " ++ show n ++ " " ++ show as ++ " " ++ show s)
                                 | memberSort s ctx                             = Right $ FuncSignature (NamePrefix n) as s
                                 | otherwise                                    = Left $ Error ("mkPrefixFuncSignature: Return sort has undefined sort " ++ show s)
    where
        undefinedSorts :: [Sort]
        undefinedSorts = filter (not . flip memberSort ctx) as

-- | Smart constructor for 'TorXakis.FuncSignature.FuncSignature'.
--   A FuncSignature is returned when the following constraints are satisfied:
--
--   * FuncSignature is not a reserved signature (both default and depending on sort).
--
--   * Sorts of arguments and return value are defined.
--
--   * Operator has one or two arguments.
--
--   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
mkInfixFuncSignature :: SortContext a => a -> TorXakis.OperatorName.OperatorName -> [Sort] -> Sort -> Either Error FuncSignature
mkInfixFuncSignature ctx n as s | not $ null undefinedSorts                   = Left $ Error ("mkInfixFuncSignature: Arguments have undefined sorts " ++ show undefinedSorts)
                                | isReservedInfixFunctionSignature ctx n as s = Left $ Error ("mkInfixFuncSignature: Reserved function signature " ++ show n ++ " " ++ show as ++ " " ++ show s)
                                | not $ memberSort s ctx                      = Left $ Error ("mkInfixFuncSignature: Return sort has undefined sort " ++ show s)
                                | otherwise                                   = case as of
                                                                                     [_]      -> Right $ FuncSignature (NameInfix  n) as s
                                                                                     [_, _]   -> Right $ FuncSignature (NameInfix  n) as s
                                                                                     _        -> Left $ Error ("mkInfixFuncSignature: Operator has one or two arguments not " ++ show (length as))

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