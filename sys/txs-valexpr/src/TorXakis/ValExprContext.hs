{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExprContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context containing Value Expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.ValExprContext
( -- * Context
  -- ** Func Context
  FuncContext (..)
  -- ** Minimal Func Context
, MinimalFuncContext
, fromSortContext
  -- ** ValExpr Context
, ValExprContext
  -- ** Minimal ValExpr Context
, MinimalValExprContext
, fromVarContext
, fromFuncContext
  -- * Substitution of variables
  -- ** Partial Substitution
, partSubst
  -- ** Complete Substitution
, compSubst
  -- * Constructor for optimize ValExpression given a FuncContext
, mkFuncOpt
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import           Data.Either
import qualified Data.HashMap    as HashMap
import qualified Data.Map        as Map
import           Data.Maybe
import qualified Data.Set        as Set
import qualified Data.Text       as T
import           GHC.Generics           (Generic)

import           TorXakis.Error
import           TorXakis.FreeVars
import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort (Sort, HasSort, getSort, elemSort, SortContext(..))
import           TorXakis.ValExpr.Unsafe
import           TorXakis.ValExpr.ValExpr
import           TorXakis.ValExpr.ValExprBasis
import           TorXakis.VarContext (VarContext(..))
import           TorXakis.VarDef
import           TorXakis.VarsDecl

------------------------------------------------------------------------------------------------------------------
-- Func Context
------------------------------------------------------------------------------------------------------------------

-- | A FuncContext instance contains all definitions to work with functions and references thereof
class SortContext a => FuncContext a where
    -- | Accessor for Function Definitions
    funcDefs :: a -> HashMap.Map FuncSignature FuncDef

    -- | Add function definitions to function context.
    --   A function context is returned when the following constraints are satisfied:
    --
    --   * The signatures of the function definitions are unique.
    --
    --   * The variables used in any function body are subset of the function parameters.
    --
    --   * All references (both Sort and FunctionDefinition) are known
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addFuncDefs :: a -> [FuncDef] -> Either MinError a

-- | A minimal instance of 'FuncContext'.
data MinimalFuncContext a = MinimalFuncContext { sortContext :: a
                                                 -- function definitions
                                               , _funcDefs :: HashMap.Map FuncSignature FuncDef
                                               } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Create FuncContext from SortContext
fromSortContext :: a -> MinimalFuncContext a
fromSortContext srt = MinimalFuncContext srt HashMap.empty

instance SortContext a => SortContext (MinimalFuncContext a) where
    empty = MinimalFuncContext TorXakis.Sort.empty HashMap.empty
    adtDefs ctx    = adtDefs (sortContext ctx)
    addAdtDefs ctx as = case addAdtDefs (sortContext ctx) as of
                          Left e     -> Left e
                          Right sctx -> Right $ ctx {sortContext = sctx} 

instance SortContext a => FuncContext (MinimalFuncContext a) where
    funcDefs = _funcDefs
    addFuncDefs ctx fds
        | not $ null nuFuncDefs              = Left $ MinError (T.pack ("Non unique function signatures: " ++ show nuFuncDefs))
        | not $ null undefinedSorts          = Left $ MinError (T.pack ("List of function signatures with undefined sorts: " ++ show undefinedSorts))
        | not $ null undefinedVariables      = Left $ MinError (T.pack ("List of function signatures with undefined variables in their bodies: " ++ show undefinedVariables))
        | not $ null undefinedFuncSignatures = Left $ MinError (T.pack ("List of function signatures with undefined function signatures in their bodies: " ++ show undefinedFuncSignatures))
        | otherwise                          = Right $ newCtx ctx (toMapByFuncSignature ctx fds)
      where
        nuFuncDefs :: [FuncDef]
        nuFuncDefs = repeatedByFuncSignatureIncremental ctx (HashMap.elems (funcDefs ctx)) fds

        undefinedSorts :: [(FuncSignature, Set.Set Sort)]
        undefinedSorts = mapMaybe undefinedSort fds

        undefinedSort :: FuncDef -> Maybe (FuncSignature, Set.Set Sort)
        undefinedSort fd = let fs = getFuncSignature ctx fd
                               as = args fs
                               rs = returnSort fs
                             in
                                case filter (not . elemSort ctx) (rs:as) of
                                    [] -> Nothing
                                    xs -> Just (fs, Set.fromList xs)

        undefinedVariables :: [(FuncSignature, Set.Set (RefByName VarDef))]
        undefinedVariables = mapMaybe undefinedVariable fds

        undefinedVariable :: FuncDef -> Maybe (FuncSignature, Set.Set (RefByName VarDef))
        undefinedVariable fd = let definedVars   :: Set.Set (RefByName VarDef)
                                   definedVars   = Set.fromList (map (RefByName . name) (toList (paramDefs fd)))
                                   usedVars      = freeVars (body fd)
                                   undefinedVars = Set.difference usedVars definedVars
                                in
                                    if Set.null undefinedVars
                                        then Nothing
                                        else Just (getFuncSignature ctx fd, undefinedVars)

        undefinedFuncSignatures :: [(FuncSignature, Set.Set FuncSignature)]
        undefinedFuncSignatures = mapMaybe undefinedFuncSignature fds

        undefinedFuncSignature :: FuncDef -> Maybe (FuncSignature, Set.Set FuncSignature)
        undefinedFuncSignature fd = let definedFuncSignatures = HashMap.union (toMapByFuncSignature ctx fds) (_funcDefs ctx) in
                                        case findUndefinedFuncSignature definedFuncSignatures (body fd) of
                                            [] -> Nothing
                                            xs -> Just (getFuncSignature ctx fd, Set.fromList xs)

        newCtx :: MinimalFuncContext a -> HashMap.Map FuncSignature FuncDef -> MinimalFuncContext a 
        newCtx ctx' mfs = let updateCtx :: MinimalFuncContext a
                              updateCtx = ctx'{ _funcDefs = HashMap.union (funcDefs ctx') mfs }
                              (lm, rm)  = HashMap.mapEither (newFuncDef updateCtx) mfs in
                                if HashMap.null lm 
                                    then let (lc, rc) = HashMap.partition isConstBody rm in
                                            if HashMap.null lc 
                                                then ctx'{ _funcDefs = HashMap.union (funcDefs ctx') rc }
                                                else newCtx (ctx'{ _funcDefs = HashMap.union (funcDefs ctx') lc }) rc
                                    else error ("All check passed, yet errors occurred\n" ++ show (HashMap.elems lm))

        newFuncDef :: MinimalFuncContext a -> FuncDef -> Either MinError FuncDef
        newFuncDef updateCtx fd = let nm = TorXakis.FuncDef.funcName fd
                                      ps = paramDefs fd
                                      bd = body fd in
                                        optimize updateCtx bd >>= mkFuncDef ctx nm ps
        
        isConstBody :: FuncDef -> Bool
        isConstBody fd = case view (body fd) of
                                Vconst {} -> True
                                _         -> False

-- | Find Undefined Function Signatures in given Value Expression (given the defined Function Signatures)
findUndefinedFuncSignature :: HashMap.Map FuncSignature FuncDef -> ValExpression -> [FuncSignature]
findUndefinedFuncSignature definedFuncSignatures = findUndefinedFuncSignature'
    where
        findUndefinedFuncSignature' :: ValExpression -> [FuncSignature]
        findUndefinedFuncSignature' = findUndefinedFuncSignatureView . view
        
        findUndefinedFuncSignatureView :: ValExpressionView -> [FuncSignature]
        findUndefinedFuncSignatureView Vconst{}                            = []
        findUndefinedFuncSignatureView Vvar{}                              = []
        findUndefinedFuncSignatureView (Vequal v1 v2)                      = findUndefinedFuncSignature' v1 ++ findUndefinedFuncSignature' v2
        findUndefinedFuncSignatureView (Vite c t f)                        = findUndefinedFuncSignature' c ++ findUndefinedFuncSignature' t ++ findUndefinedFuncSignature' f
        findUndefinedFuncSignatureView (Vfunc f as)                        = (if HashMap.member f definedFuncSignatures
                                                                                    then []
                                                                                    else [f]
                                                                              )
                                                                              ++ concatMap findUndefinedFuncSignature' as
        findUndefinedFuncSignatureView (Vpredef _ as)                      = concatMap findUndefinedFuncSignature' as
        findUndefinedFuncSignatureView (Vnot v)                            = findUndefinedFuncSignature' v
        findUndefinedFuncSignatureView (Vand vs)                           = concatMap findUndefinedFuncSignature' (Set.toList vs)
        findUndefinedFuncSignatureView (Vdivide t n)                       = findUndefinedFuncSignature' t ++ findUndefinedFuncSignature' n
        findUndefinedFuncSignatureView (Vmodulo t n)                       = findUndefinedFuncSignature' t ++ findUndefinedFuncSignature' n
        findUndefinedFuncSignatureView (Vsum mp)                           = concatMap findUndefinedFuncSignature' (Map.keys mp)
        findUndefinedFuncSignatureView (Vproduct mp)                       = concatMap findUndefinedFuncSignature' (Map.keys mp)
        findUndefinedFuncSignatureView (Vgez v)                            = findUndefinedFuncSignature' v
        findUndefinedFuncSignatureView (Vlength v)                         = findUndefinedFuncSignature' v
        findUndefinedFuncSignatureView (Vat s p)                           = findUndefinedFuncSignature' s ++ findUndefinedFuncSignature' p
        findUndefinedFuncSignatureView (Vconcat vs)                        = concatMap findUndefinedFuncSignature' vs
        findUndefinedFuncSignatureView (Vstrinre s r)                      = findUndefinedFuncSignature' s ++ findUndefinedFuncSignature' r
        findUndefinedFuncSignatureView (Vcstr _ _ as)                      = concatMap findUndefinedFuncSignature' as
        findUndefinedFuncSignatureView (Viscstr _ _ v)                     = findUndefinedFuncSignature' v
        findUndefinedFuncSignatureView (Vaccess _ _ _ v)                   = findUndefinedFuncSignature' v

-- | Optimize value expression using func context
optimize :: FuncContext a => a -> ValExpression -> Either MinError ValExpression
optimize ctx  = optimizeView . view
    where
        optimizeView :: ValExpressionView -> Either MinError ValExpression
        optimizeView (Vconst c)                = unsafeConst c
        optimizeView (Vvar r)                  = unsafeVar r
        optimizeView (Vequal ve1 ve2)          = optimizeView (view ve1) >>= (\ne1 ->
                                                 optimizeView (view ve2) >>= 
                                                 unsafeEqual ne1)
        optimizeView (Vite c tb fb)            = optimizeView (view tb) >>= (\ntb ->
                                                 optimizeView (view fb) >>= (\nfb ->
                                                 optimizeView (view  c) >>= (\nc  ->
                                                 unsafeITE nc ntb nfb)))
        optimizeView (Vfunc fs vs)             = case partitionEithers (map (optimizeView . view) vs) of
                                                    ([], nvs) -> unsafeFunc ctx fs nvs
                                                    (es, _)   -> Left $ MinError (T.pack ("Optimize 'func' failed\n" ++ show es))
        optimizeView (Vpredef fs vs)           = case partitionEithers (map (optimizeView . view) vs) of
                                                    ([], nvs) -> unsafePredefNonSolvable ctx fs nvs
                                                    (es, _)   -> Left $ MinError (T.pack ("Optimize 'predef' failed\n" ++ show es))
        optimizeView (Vnot v)                  = optimizeView (view v) >>= unsafeNot
        optimizeView (Vand s)                  = case partitionEithers (map (optimizeView . view) (Set.toList s)) of
                                                    ([], ns) -> unsafeAnd (Set.fromList ns)
                                                    (es, _)  -> Left $ MinError (T.pack ("Optimize 'and' failed\n" ++ show es))
        optimizeView (Vdivide t n)             = optimizeView (view t) >>= (\nt ->
                                                 optimizeView (view n) >>=
                                                 unsafeDivide nt)
        optimizeView (Vmodulo t n)             = optimizeView (view t) >>= (\nt ->
                                                 optimizeView (view n) >>=
                                                 unsafeModulo nt)
        optimizeView (Vsum m)                  = case partitionEithers (map (\(x,i) -> optimizeView (view x) >>= (\nx -> Right (nx,i)))
                                                                            (Map.toList m)) of
                                                    ([], l) -> unsafeSumFromMap (Map.fromListWith (+) l)
                                                    (es, _) -> Left $ MinError (T.pack ("Optimize 'sum' failed\n" ++ show es))
        optimizeView (Vproduct m)              = case partitionEithers (map (\(x,i) -> optimizeView (view x) >>= (\nx -> Right (nx,i)))
                                                                            (Map.toList m)) of
                                                    ([], l) -> unsafeProductFromMap (Map.fromListWith (+) l)
                                                    (es, _) -> Left $ MinError (T.pack ("Optimize 'product' failed\n" ++ show es))
        optimizeView (Vgez v)                  = optimizeView (view v) >>= unsafeGEZ
        optimizeView (Vlength s)               = optimizeView (view s) >>= unsafeLength
        optimizeView (Vat s i)                 = optimizeView (view s) >>= (\ns ->
                                                 optimizeView (view i) >>= 
                                                 unsafeAt ns)
        optimizeView (Vconcat s)               = case partitionEithers (map (optimizeView . view) s) of
                                                    ([], ns) -> unsafeConcat ns
                                                    (es, _)  -> Left $ MinError (T.pack ("Optimize 'concat' failed\n" ++ show es))
        optimizeView (Vstrinre s r)            = optimizeView (view s) >>= (\ns ->
                                                 optimizeView (view r) >>= 
                                                 unsafeStrInRe ns)
        optimizeView (Vcstr a c l)             = case partitionEithers (map (optimizeView . view) l) of
                                                    ([], nl) -> unsafeCstr a c nl
                                                    (es, _)  -> Left $ MinError (T.pack ("Optimize 'cstr' failed\n" ++ show es))
        optimizeView (Viscstr a c v)           = optimizeView (view v) >>= unsafeIsCstr a c
        optimizeView (Vaccess a c p v)         = optimizeView (view v) >>= unsafeAccess a c p

------------------------------------------------------------------------------------------------------------------
-- ValExpr Context
------------------------------------------------------------------------------------------------------------------

-- | A ValExprContext instance contains all definitions to work with value expressions and references thereof
class (VarContext a, FuncContext a) => ValExprContext a

-- | A minimal instance of 'ValExprContext'.
data MinimalValExprContext a = MinimalValExprContext { funcContext :: a
                                                       -- var definitions
                                                     , _varDefs :: HashMap.Map (RefByName VarDef) VarDef
                                                     } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Create ValExprContext from FuncContext
fromFuncContext :: a -> MinimalValExprContext a
fromFuncContext fc = MinimalValExprContext fc HashMap.empty

-- | Create ValExprContext from VarContext
fromVarContext :: VarContext a => a -> MinimalValExprContext (MinimalFuncContext a)
fromVarContext vc = MinimalValExprContext (fromSortContext vc) (varDefs vc)

instance SortContext a => SortContext (MinimalValExprContext a) where
    empty = MinimalValExprContext TorXakis.Sort.empty HashMap.empty
    adtDefs ctx         = adtDefs (funcContext ctx)
    addAdtDefs ctx as   = case addAdtDefs (funcContext ctx) as of
                            Left e     -> Left e
                            Right fctx -> Right $ ctx {funcContext = fctx}

instance FuncContext a => FuncContext (MinimalValExprContext a) where
    funcDefs ctx        = funcDefs (funcContext ctx)
    addFuncDefs ctx fds = case addFuncDefs (funcContext ctx) fds of
                            Left e     -> Left e
                            Right fctx -> Right $ ctx {funcContext = fctx}

instance SortContext a => VarContext (MinimalValExprContext a) where
    varDefs = _varDefs
    addVarDefs ctx vs
        | not $ null nuVarDefs               = Left $ MinError (T.pack ("Non unique variable definitions: " ++ show nuVarDefs))
        | not $ null undefinedSorts          = Left $ MinError (T.pack ("List of variable definitions with undefined sorts: " ++ show undefinedSorts))
        | otherwise                          = Right $ ctx { _varDefs = HashMap.union (toMapByName vs) (varDefs ctx)}
      where
        nuVarDefs :: [VarDef]
        nuVarDefs = repeatedByName vs

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . elemSort ctx . sort) vs

instance FuncContext a => ValExprContext (MinimalValExprContext a)
------------------------------------------------------------------------------------------------------------------
-- Substitution
------------------------------------------------------------------------------------------------------------------

-- | find mismatches in sort in mapping
mismatchesSort :: forall c b . (VarContext c, HasSort c b) => c -> HashMap.Map (RefByName VarDef) b -> HashMap.Map (RefByName VarDef) b
mismatchesSort ctx = HashMap.filterWithKey mismatch
    where
        mismatch :: RefByName VarDef -> b -> Bool
        mismatch r e = case HashMap.lookup r (varDefs ctx) of
                            Nothing -> error ("mismatchesSort - variable not defined in context - " ++ show r)
                            Just v  -> TorXakis.VarDef.sort v /= getSort ctx e

-- | Partial Substitution: Substitute some variables by value expressions in a value expression.
-- The Either is needed since substitution can cause an invalid ValExpr. 
-- For example, substitution of a variable by zero can cause a division by zero error
partSubst :: ValExprContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> ValExpression -> Either MinError ValExpression
partSubst ctx mp ve | HashMap.null mp               = Right ve
                    | not (HashMap.null mismatches) = Left $ MinError (T.pack ("Sort mismatches in map : " ++ show mismatches))
                    | otherwise                     = partSubstView (view ve)
  where
    mismatches :: HashMap.Map (RefByName VarDef) ValExpression
    mismatches = mismatchesSort ctx mp
    
    partSubstView :: ValExpressionView -> Either MinError ValExpression
    partSubstView (Vconst c)                = unsafeConst c
    partSubstView (Vvar r)                  = case HashMap.lookup r mp of
                                                    Nothing -> unsafeVar r
                                                    Just x  -> Right x
    partSubstView (Vequal ve1 ve2)          = partSubstView (view ve1) >>= (\ne1 ->
                                              partSubstView (view ve2) >>= 
                                              unsafeEqual ne1)
    partSubstView (Vite c tb fb)            = partSubstView (view tb) >>= (\ntb ->
                                              partSubstView (view fb) >>= (\nfb ->
                                              partSubstView (view c) >>= (\nc ->
                                              unsafeITE nc ntb nfb)))
    partSubstView (Vfunc fs vs)             = case partitionEithers (map (partSubstView . view) vs) of
                                                ([], nvs) -> unsafeFunc ctx fs nvs
                                                (es, _)   -> Left $ MinError (T.pack ("Subst partSubst 'func' failed\n" ++ show es))
    partSubstView (Vpredef fs vs)           = case partitionEithers (map (partSubstView . view) vs) of
                                                ([], nvs) -> unsafePredefNonSolvable ctx fs nvs
                                                (es, _)   -> Left $ MinError (T.pack ("Subst partSubst 'predef' failed\n" ++ show es))
    partSubstView (Vnot v)                  = partSubstView (view v) >>= unsafeNot
    partSubstView (Vand s)                  = case partitionEithers (map (partSubstView . view) (Set.toList s)) of
                                                ([], ns) -> unsafeAnd (Set.fromList ns)
                                                (es, _)  -> Left $ MinError (T.pack ("Subst partSubst 'and' failed\n" ++ show es))
    partSubstView (Vdivide t n)             = partSubstView (view t) >>= (\nt ->
                                              partSubstView (view n) >>=
                                              unsafeDivide nt)
    partSubstView (Vmodulo t n)             = partSubstView (view t) >>= (\nt ->
                                              partSubstView (view n) >>=
                                              unsafeModulo nt)
    partSubstView (Vsum m)                  = case partitionEithers (map (\(x,i) -> partSubstView (view x) >>= (\nx -> Right (nx,i)))
                                                                         (Map.toList m)) of
                                                ([], l) -> unsafeSumFromMap (Map.fromListWith (+) l)
                                                (es, _) -> Left $ MinError (T.pack ("Subst partSubst 'sum' failed\n" ++ show es))
    partSubstView (Vproduct m)              = case partitionEithers (map (\(x,i) -> partSubstView (view x) >>= (\nx -> Right (nx,i)))
                                                                         (Map.toList m)) of
                                                ([], l) -> unsafeProductFromMap (Map.fromListWith (+) l)
                                                (es, _) -> Left $ MinError (T.pack ("Subst partSubst 'product' failed\n" ++ show es))
    partSubstView (Vgez v)                  = partSubstView (view v) >>= unsafeGEZ
    partSubstView (Vlength s)               = partSubstView (view s) >>= unsafeLength
    partSubstView (Vat s i)                 = partSubstView (view s) >>= (\ns ->
                                              partSubstView (view i) >>= 
                                              unsafeAt ns)
    partSubstView (Vconcat s)               = case partitionEithers (map (partSubstView . view) s) of
                                                ([], ns) -> unsafeConcat ns
                                                (es, _)  -> Left $ MinError (T.pack ("Subst partSubst 'concat' failed\n" ++ show es))
    partSubstView (Vstrinre s r)            = partSubstView (view s) >>= (\ns ->
                                              partSubstView (view r) >>= 
                                              unsafeStrInRe ns)
    partSubstView (Vcstr a c l)             = case partitionEithers (map (partSubstView . view) l) of
                                                ([], nl) -> unsafeCstr a c nl
                                                (es, _)  -> Left $ MinError (T.pack ("Subst partSubst 'cstr' failed\n" ++ show es))
    partSubstView (Viscstr a c v)           = partSubstView (view v) >>= unsafeIsCstr a c
    partSubstView (Vaccess a c p v)         = partSubstView (view v) >>= unsafeAccess a c p


-- | Complete Substitution: Substitute all variables by value expressions in a value expression.
-- Since all variables are changed, one can change the kind of variables.
compSubst :: ValExprContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> ValExpression -> Either MinError ValExpression
compSubst ctx mp ve | not (HashMap.null mismatches)    = Left $ MinError (T.pack ("Sort mismatches in map : " ++ show mismatches))
                    | otherwise                        = compSubstView (view ve)
  where
    mismatches :: HashMap.Map (RefByName VarDef) ValExpression
    mismatches = mismatchesSort ctx mp

    compSubstView :: ValExpressionView -> Either MinError ValExpression
    compSubstView (Vconst c)                = unsafeConst c
    compSubstView (Vvar v)                  = case HashMap.lookup v mp of
                                                    Nothing -> Left $ MinError (T.pack ("Subst compSubst: incomplete. Missing " ++ show v))
                                                    Just w  -> Right w
    compSubstView (Vequal ve1 ve2)          = compSubstView (view ve1) >>= (\ne1 ->
                                              compSubstView (view ve2) >>= 
                                              unsafeEqual ne1)
    compSubstView (Vite c tb fb)            = compSubstView (view tb) >>= (\ntb ->
                                              compSubstView (view fb) >>= (\nfb ->
                                              compSubstView (view c) >>= (\nc ->
                                              unsafeITE nc ntb nfb)))
    compSubstView (Vfunc fs vs)             = case partitionEithers (map (compSubstView . view) vs) of
                                                    ([], nvs) -> unsafeFunc ctx fs nvs
                                                    (es, _)   -> Left $ MinError (T.pack ("Subst compSubst 'func' failed\n" ++ show es))
    compSubstView (Vpredef fs vs)           = case partitionEithers (map (compSubstView . view) vs) of
                                                    ([], nvs) -> unsafePredefNonSolvable ctx fs nvs
                                                    (es, _)   -> Left $ MinError (T.pack ("Subst compSubst 'predef' failed\n" ++ show es))
    compSubstView (Vnot v)                  = compSubstView (view v) >>= unsafeNot
    compSubstView (Vand s)                  = case partitionEithers (map (compSubstView . view) (Set.toList s)) of
                                                    ([], ns) -> unsafeAnd (Set.fromList ns)
                                                    (es, _)  -> Left $ MinError (T.pack ("Subst compSubst 'and' failed\n" ++ show es))
    compSubstView (Vdivide t n)             = compSubstView (view t) >>= (\nt ->
                                              compSubstView (view n) >>=
                                              unsafeDivide nt)
    compSubstView (Vmodulo t n)             = compSubstView (view t) >>= (\nt ->
                                              compSubstView (view n) >>=
                                              unsafeModulo nt)
    compSubstView (Vsum m)                  = case partitionEithers (map (\(x,i) -> compSubstView (view x) >>= (\nx -> Right (nx,i)))
                                                                         (Map.toList m)) of
                                                    ([], l) -> unsafeSumFromMap (Map.fromListWith (+) l)
                                                    (es, _) -> Left $ MinError (T.pack ("Subst compSubst 'sum' failed\n" ++ show es))
    compSubstView (Vproduct m)              = case partitionEithers (map (\(x,i) -> compSubstView (view x) >>= (\nx -> Right (nx,i)))
                                                                         (Map.toList m)) of
                                                    ([], l) -> unsafeProductFromMap (Map.fromListWith (+) l)
                                                    (es, _) -> Left $ MinError (T.pack ("Subst compSubst 'product' failed\n" ++ show es))
    compSubstView (Vgez v)                  = compSubstView (view v) >>= unsafeGEZ
    compSubstView (Vlength s)               = compSubstView (view s) >>= unsafeLength
    compSubstView (Vat s i)                 = compSubstView (view s) >>= (\ns ->
                                              compSubstView (view i) >>= 
                                              unsafeAt ns)
    compSubstView (Vconcat s)               = case partitionEithers (map (compSubstView . view) s) of
                                                    ([], ns) -> unsafeConcat ns
                                                    (es, _)  -> Left $ MinError (T.pack ("Subst compSubst 'concat' failed\n" ++ show es))
    compSubstView (Vstrinre s r)            = compSubstView (view s) >>= (\ns ->
                                              compSubstView (view r) >>= 
                                              unsafeStrInRe ns)
    compSubstView (Vcstr a c l)             = case partitionEithers (map (compSubstView . view) l) of
                                                    ([], nl) -> unsafeCstr a c nl
                                                    (es, _)  -> Left $ MinError (T.pack ("Subst compSubst 'cstr' failed\n" ++ show es))
    compSubstView (Viscstr a c v)           = compSubstView (view v) >>= unsafeIsCstr a c
    compSubstView (Vaccess a c p v)         = compSubstView (view v) >>= unsafeAccess a c p

-- | mkFuncOpt
-- Construct optimized function
-- This function should be prefered over mkFunc whenever a FuncContext is available
mkFuncOpt :: ValExprContext c => c -> FuncSignature -> [ValExpression] -> Either MinError ValExpression
mkFuncOpt ctx fs vs = mkFunc ctx fs vs >>= optimize ctx

unsafeFunc :: forall c . FuncContext c => c -> FuncSignature -> [ValExpression] -> Either MinError ValExpression
unsafeFunc ctx fs vs = case HashMap.lookup fs (funcDefs ctx) of
                            Nothing -> error ("unsafeFunc: function can't be found in context - " ++ show fs)
                            Just fd -> case view (body fd) of
                                            Vconst x  -> unsafeConst x
                                            _         -> case toMaybeValues vs of
                                                             Just _  -> let vars = toList (paramDefs fd) 
                                                                            nctx :: MinimalValExprContext c
                                                                            nctx = fromFuncContext ctx
                                                                         in
                                                                           case addVarDefs nctx vars of
                                                                               Left e     -> error ("UnsafeFunc can add function parameters to funcContext: " ++ show e)
                                                                               Right vctx -> compSubst vctx 
                                                                                                       (HashMap.fromList (zip (map (RefByName . name) vars) vs))
                                                                                                       (body fd)
                                                             Nothing -> Right $ ValExpression (Vfunc fs vs)
