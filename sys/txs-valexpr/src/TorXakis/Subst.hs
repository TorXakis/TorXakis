{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Subst
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context containing Value Expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module TorXakis.Subst
( -- * Context
  -- ** instance of ValExpr Context
  ContextValExpr
, TorXakis.Subst.fromVarContext
, fromFuncContext
  -- * Substitution of variables
, subst
  -- * Constructor for optimize ValExpression given a FuncContext
, mkFuncOpt
  -- dependencies, yet part of interface
, module TorXakis.ValExprContext
)
where
import           Control.Arrow          (first)
import           Data.Either
import qualified Data.HashMap           as HashMap
import           Data.List
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.Set               as Set
import qualified Data.Text              as T

import           TorXakis.ContextValExprConstruction
import           TorXakis.ContextVar
import           TorXakis.Error
import           TorXakis.FuncContext
import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.ValExprConstructionContext
import           TorXakis.ValExprContext
import           TorXakis.ValExpr.Unsafe
import           TorXakis.ValExpr.ValExpr
import           TorXakis.ValExpr.ValExprBasis
import           TorXakis.Var

-- | An instance of 'TorXakis.ValExprConstructionContext'.
data ContextValExpr = forall c . VarContext c =>
                            ContextValExpr { _varContext :: c
                                             -- funcSignatures
                                           , funcDefs :: HashMap.Map FuncSignature FuncDef
                                           }

-- | Create ContextValExpr from FuncSignatureContext
fromFuncContext :: FuncContext c => c -> ContextValExpr
fromFuncContext fc = ContextValExpr (fromSortContext fc) (toMapByFuncSignature fc (elemsFunc fc))

-- | Create ContextValExpr from VarContext
fromVarContext :: VarContext c => c -> ContextValExpr
fromVarContext vc = ContextValExpr vc HashMap.empty

instance SortContext ContextValExpr where
    -- Can't use
    -- memberSort   = memberSort . varContext
    -- since compiler complains:
    --        * Cannot use record selector `sortContext' as a function due to escaped type variables
    --          Probable fix: use pattern-matching syntax instead
    -- For more info see: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#extension-ExistentialQuantification
    memberSort r (ContextValExpr ctx _) = memberSort r ctx

    memberADT r (ContextValExpr ctx _) = memberADT r ctx

    lookupADT r (ContextValExpr ctx _) = lookupADT r ctx

    elemsADT (ContextValExpr ctx _) = elemsADT ctx

    addADTs as (ContextValExpr ctx vs) = case addADTs as ctx of
                                                        Left e     -> Left e
                                                        Right sctx -> Right $ ContextValExpr sctx vs

instance VarContext ContextValExpr where
    memberVar v (ContextValExpr ctx _) = memberVar v ctx

    lookupVar v (ContextValExpr ctx _) = lookupVar v ctx

    elemsVar (ContextValExpr ctx _) = elemsVar ctx

    addVars vs (ContextValExpr ctx fs) = case addVars vs ctx of
                                            Left e     -> Left e
                                            Right sctx -> Right $ ContextValExpr sctx fs

instance FuncSignatureContext ContextValExpr where
    memberFunc f ctx = HashMap.member f (funcDefs ctx)

    funcSignatures ctx    = HashMap.keys (funcDefs ctx)

instance FuncSignatureModifyContext ContextValExpr ContextValExprConstruction where
    addFuncSignatures fs ctx
        | not $ null undefinedSorts          = Left $ Error ("List of function signatures with undefined sorts: " ++ show undefinedSorts)
        | not $ null nuFuncSignatures        = Left $ Error ("Non unique function signatures: " ++ show nuFuncSignatures)
        | otherwise                          = addFuncSignatures (fs ++ funcSignatures ctx) (TorXakis.ContextValExprConstruction.fromVarContext ctx)
      where
        nuFuncSignatures :: [FuncSignature]
        nuFuncSignatures = repeatedByFuncSignatureIncremental ctx (funcSignatures ctx) fs

        undefinedSorts :: [FuncSignature]
        undefinedSorts = filter (\f -> any (not . flip memberSort ctx) (returnSort f: args f) ) fs

instance FuncContext ContextValExpr where
    lookupFunc f ctx = HashMap.lookup f (funcDefs ctx)

    elemsFunc ctx    = HashMap.elems (funcDefs ctx)

    addFuncs fds ctx
        | not $ null nuFuncDefs              = Left $ Error (T.pack ("Non unique function signatures: " ++ show nuFuncDefs))
        | not $ null undefinedSorts          = Left $ Error (T.pack ("List of function signatures with undefined sorts: " ++ show undefinedSorts))
        | not $ null undefinedVariables      = Left $ Error (T.pack ("List of function signatures with undefined variables in their bodies: " ++ show undefinedVariables))
        | not $ null undefinedFuncSignatures = Left $ Error (T.pack ("List of function signatures with undefined function signatures in their bodies: " ++ show undefinedFuncSignatures))
        | otherwise                          = Right $ newCtx ctx (toMapByFuncSignature ctx fds)
      where
        nuFuncDefs :: [FuncDef]
        nuFuncDefs = repeatedByFuncSignatureIncremental ctx (elemsFunc ctx) fds

        definedSorts :: Set.Set Sort
        definedSorts = Set.fromList (elemsSort ctx)

        undefinedSorts :: [(FuncSignature, Set.Set Sort)]
        undefinedSorts = mapMaybe maybeUndefinedSorts fds

        maybeUndefinedSorts :: FuncDef -> Maybe (FuncSignature, Set.Set Sort)
        maybeUndefinedSorts fd = let vctx = toValExprContext (paramDefs fd)
                                     usedS = Set.unions [usedSorts ctx (paramDefs fd), usedSorts vctx (body fd)]
                                     undefinedS = usedS `Set.difference` definedSorts
                                  in
                                     if Set.null undefinedS
                                        then Nothing
                                        else Just (getFuncSignature ctx fd, undefinedS)

        toValExprContext :: VarsDecl -> ContextValExpr
        toValExprContext vs = case addVars (toList vs) (fromFuncContext ctx) of
                                   Left e      -> error ("toValExprContext is unable to make new context" ++ show e)
                                   Right vctx  -> vctx

        undefinedVariables :: [(FuncSignature, Set.Set (RefByName VarDef))]
        undefinedVariables = mapMaybe maybeUndefinedVariables fds

        maybeUndefinedVariables :: FuncDef -> Maybe (FuncSignature, Set.Set (RefByName VarDef))
        maybeUndefinedVariables fd = let definedVars :: Set.Set (RefByName VarDef)
                                         definedVars = Set.fromList (map (RefByName . name) (toList (paramDefs fd)))
                                         usedVars = freeVars (body fd)
                                         undefinedVars = usedVars `Set.difference` definedVars
                                      in
                                         if Set.null undefinedVars
                                            then Nothing
                                            else Just (getFuncSignature ctx fd, undefinedVars)

        definedFuncSignatures :: Set.Set FuncSignature
        definedFuncSignatures = Set.fromList (funcSignatures ctx ++ map (getFuncSignature ctx) fds)

        undefinedFuncSignatures :: [(FuncSignature, Set.Set FuncSignature)]
        undefinedFuncSignatures = mapMaybe maybeUndefinedFuncSignatures fds

        maybeUndefinedFuncSignatures :: FuncDef -> Maybe (FuncSignature, Set.Set FuncSignature)
        maybeUndefinedFuncSignatures fd = let usedFS = usedFuncSignatures (body fd)
                                              undefinedFS = usedFS `Set.difference` definedFuncSignatures
                                            in
                                               if Set.null undefinedFS
                                                  then Nothing
                                                  else Just (getFuncSignature ctx fd, undefinedFS)

        newCtx :: ContextValExpr -> HashMap.Map FuncSignature FuncDef -> ContextValExpr
        newCtx ctx' mfs = let updateCtx = ctx'{ funcDefs = HashMap.union (funcDefs ctx') mfs }
                              (lm, rm)  = HashMap.mapEither (newFuncDef updateCtx) mfs in
                                if HashMap.null lm
                                    then let (lc, rc) = HashMap.partition isConstBody rm in
                                            if HashMap.null lc
                                                then ctx'{ funcDefs = HashMap.union (funcDefs ctx') rc }
                                                else newCtx (ctx'{ funcDefs = HashMap.union (funcDefs ctx') lc }) rc
                                    else error ("All check passed, yet errors occurred\n" ++ show (HashMap.elems lm))

        newFuncDef :: ContextValExpr -> FuncDef -> Either Error FuncDef
        newFuncDef updateCtx fd = let nm = TorXakis.FuncDef.funcName fd
                                      ps = paramDefs fd
                                      bd = body fd in
                                        unsafeSubst updateCtx HashMap.empty bd >>= mkFuncDef ctx nm ps

        isConstBody :: FuncDef -> Bool
        isConstBody fd = case view (body fd) of
                                Vconst {} -> True
                                _         -> False


instance ValExprConstructionContext ContextValExpr

instance ValExprContext ContextValExpr

unsafeSubst :: ValExprContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> ValExpression -> Either Error ValExpression
unsafeSubst ctx  mp = unsafeSubstView . view
  where
    unsafeSubstView :: ValExpressionView -> Either Error ValExpression
    unsafeSubstView (Vconst c)                = unsafeConst c
    unsafeSubstView (Vvar r)                  = case HashMap.lookup r mp of
                                                    Nothing -> unsafeVar r
                                                    Just x  -> Right x
    unsafeSubstView (Vequal ve1 ve2)          = unsafeEqual (unsafeSubstView (view ve1)) (unsafeSubstView (view ve2))
    unsafeSubstView (Vite c tb fb)            = unsafeITE (unsafeSubstView (view c)) (unsafeSubstView (view tb)) (unsafeSubstView (view fb))
    unsafeSubstView (Vfunc r vs)              = unsafeFunc ctx r (map (unsafeSubstView . view) vs)
    unsafeSubstView (Vpredef r vs)            = unsafePredefNonSolvable ctx r (map (unsafeSubstView . view) vs)
    unsafeSubstView (Vnot v)                  = unsafeNot (unsafeSubstView (view v))
    unsafeSubstView (Vand s)                  = unsafeAnd (Set.map (unsafeSubstView . view) s)
    unsafeSubstView (Vdivide t n)             = unsafeDivide (unsafeSubstView (view t)) (unsafeSubstView (view n))
    unsafeSubstView (Vmodulo t n)             = unsafeModulo (unsafeSubstView (view t)) (unsafeSubstView (view n))
    unsafeSubstView (Vsum m)                  = unsafeSumFactor (map (first (unsafeSubstView . view)) (Map.toList m))
    unsafeSubstView (Vproduct m)              = unsafeProductFactor (map (first (unsafeSubstView . view)) (Map.toList m))
    unsafeSubstView (Vgez v)                  = unsafeGEZ (unsafeSubstView (view v))
    unsafeSubstView (Vlength s)               = unsafeLength (unsafeSubstView (view s))
    unsafeSubstView (Vat s i)                 = unsafeAt (unsafeSubstView (view s)) (unsafeSubstView (view i))
    unsafeSubstView (Vconcat s)               = unsafeConcat (map (unsafeSubstView . view) s)
    unsafeSubstView (Vstrinre s r)            = unsafeStrInRe (unsafeSubstView (view s)) (unsafeSubstView (view r))
    unsafeSubstView (Vcstr a c l)             = unsafeCstr a c (map (unsafeSubstView . view) l)
    unsafeSubstView (Viscstr a c v)           = unsafeIsCstr a c (unsafeSubstView (view v))
    unsafeSubstView (Vaccess a c p v)         = unsafeAccess a c p (unsafeSubstView (view v))


-----------------------------------------------------------------------------
-- Substitute
-----------------------------------------------------------------------------
-- | find mismatches in sort in mapping
mismatchesSort :: ValExprConstructionContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> HashMap.Map (RefByName VarDef) ValExpression
mismatchesSort ctx = HashMap.filterWithKey mismatch
    where
        mismatch :: RefByName VarDef -> ValExpression -> Bool
        mismatch r e = case lookupVar (toName r) ctx of
                            Nothing -> error ("mismatchesSort - variable not defined in context - " ++ show r)
                            Just v  -> TorXakis.Var.sort v /= getSort ctx e

-- | Substitution: Substitute some variables by value expressions in a value expression.
-- The Either is needed since substitution can cause an invalid ValExpr.
-- For example, substitution of a variable by zero can cause a division by zero error
-- TODO: should we check the replacing val expressions? And the valExpression we get to work on?
-- TODO: should we support context shrinking, since by replacing the variable a by 10, the variable a might no longer be relevant in the context (and thus be removed)?
subst :: ValExprContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> ValExpression -> Either Error ValExpression
subst ctx mp ve | HashMap.null mp               = Right ve
                | not (HashMap.null mismatches) = Left $ Error (T.pack ("Sort mismatches in map : " ++ show mismatches))
                | not (null undefinedVars)      = Left $ Error (T.pack ("Undefined variables in map : " ++ show undefinedVars))
                | otherwise                     = unsafeSubst ctx mp ve
  where
    mismatches :: HashMap.Map (RefByName VarDef) ValExpression
    mismatches = mismatchesSort ctx mp

    undefinedVars :: [RefByName VarDef]
    undefinedVars = filter (not . flip memberVar ctx . toName) (HashMap.keys mp)

-- | TODO: needed? Complete Substitution: Substitute all variables by value expressions in a value expression.
-- Since all variables are changed, one can change the kind of variables.
-- TODO: from one context to another
-- compSubst :: ValExprContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> ValExpression -> Either Error ValExpression

-- | mkFuncOpt
-- Construct optimized function
-- This function should be prefered over mkFunc whenever a 'TorXakis.ValExprContext' is available
-- Note: since arguments of function can contain variables (e.g. the parameters of the calling function) a ValExprContext is needed (instead of a FuncContext)
mkFuncOpt :: ValExprContext c => c -> RefByFuncSignature -> [ValExpression] -> Either Error ValExpression
mkFuncOpt ctx r vs = case mkFunc ctx r vs of
                        Left e  -> Left e
                        Right v -> unsafeSubst ctx HashMap.empty v

-- TODO? More laziness?
-- e.g. depending on some parameter value, some other parameter values might be irrelevant
-- e.g. ANY/Error for not initialized variables of State Automaton translated to a ProcDef
unsafeFunc :: ValExprContext c => c -> RefByFuncSignature -> [Either Error ValExpression] -> Either Error ValExpression
unsafeFunc ctx r vs = let fs = toFuncSignature r in
                        case partitionEithers vs of
                             ([], xs)   -> case lookupFunc fs ctx of
                                                Nothing -> error ("unsafeFunc: function can't be found in context - " ++ show fs)
                                                Just fd -> case view (body fd) of
                                                                Vconst x  -> unsafeConst x
                                                                _         -> case toMaybeValues xs of
                                                                                 Just _  -> let ps = paramDefs fd
                                                                                                in case addVars (toList ps) (fromFuncContext ctx) of
                                                                                                        Left e -> error ("unsafeFunc: can't make new context - " ++ show e)
                                                                                                        Right nctx -> subst nctx
                                                                                                                            (HashMap.fromList (zip (map (RefByName . name) (toList ps)) xs))
                                                                                                                            (body fd)
                                                                                 Nothing -> Right $ ValExpression (Vfunc r xs)
                             (es, _)    -> Left $ Error ("unsafeFunc Error " ++ show (length es) ++ "\n" ++ intercalate "\n" (map show es))

