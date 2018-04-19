{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
module TorXakis.Compiler.Simplifiable where

import           Control.Lens    (over)
import           Data.Data.Lens  (uniplate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)

import qualified BehExprDefs     as BExpr
import           FuncDef         (FuncDef (FuncDef))
import           FuncId          (FuncId (FuncId), name)
import           FuncTable       (FuncTable, Signature (Signature), toMap)
import           ProcId          (ProcId)
import           TxsDefs         (ActOffer (ActOffer), BExpr,
                                  BExprView (ActionPref, ValueEnv),
                                  ChanOffer (Exclam, Quest), Offer (Offer),
                                  ProcDef (ProcDef), actionPref, valueEnv)
import           ValExpr         (ValExpr, ValExprView (Vfunc, Vite), cstrITE,
                                  cstrVar)
import qualified ValExpr
import           VarId           (VarId)

-- | TODO: probably a better name will be 'BetaReducible' or something like
-- that. But we have to be careful about how this beta-reduction operation is
-- defined.
class Simplifiable e where
    -- | Simplify the expressions by applying the functions defined in the function table.
    --
    -- TODO: Return an Either instead of forcing the instances of this class to throw an error.
    simplify :: FuncTable VarId
             -> [Text] -- ^ Symbols that are allowed to be simplified (used for
                       -- compliance with old TorXakis compiler)
             -> e
             -> e

instance Simplifiable (ValExpr VarId) where
-- ^ Only simplify these function calls. Once we do not
                    -- need to be compliant with the old TorXakis compiler we
                    -- can optimize further.
    simplify ft fns ex@(ValExpr.view -> Vfunc (FuncId n _ aSids rSid) vs) =
        -- TODO: For now make the simplification only if "n" is a predefined
        -- symbol. Once compliance with the current `TorXakis` compiler is not
        -- needed we can remove this constraint and simplify further.
        if n `elem` fns
        then fromMaybe (error "Could not apply handler") $ do
            sh <- Map.lookup n (toMap ft)
            h  <- Map.lookup (Signature aSids rSid) sh
            return $ h (simplify ft fns <$> vs)
        else ex

    simplify ft fns (ValExpr.view -> Vite ex0 ex1 ex2) =
        cstrITE
        (simplify ft fns ex0)
        (simplify ft fns ex1)
        (simplify ft fns ex2)
    simplify ft fns x                          =
        over uniplate (simplify ft fns) x

instance Simplifiable FuncId where
    simplify _ _ = id

instance Simplifiable (FuncDef VarId) where
    simplify ft fns (FuncDef vs ex) = FuncDef vs (simplify ft fns ex)

instance (Simplifiable a, Simplifiable b) => Simplifiable (a, b) where
    simplify ft fns (a, b) = (simplify ft fns a, simplify ft fns b)

instance (Simplifiable a) => Simplifiable [a] where
    simplify ft fns = fmap (simplify ft fns)

instance (Simplifiable a, Ord a) => Simplifiable (Set a) where
    simplify ft fns = Set.fromList . fmap (simplify ft fns) . Set.toList

instance (Simplifiable k, Simplifiable v, Ord k) => Simplifiable (Map k v) where
    simplify ft fns = Map.fromList . simplify ft fns .Map.toList

instance Simplifiable ProcId where
    simplify _ _ = id

instance Simplifiable ProcDef where
    simplify ft fns (ProcDef cs vs bexp) = ProcDef cs vs (simplify ft fns bexp)

instance Simplifiable BExpr where
    simplify ft fns (BExpr.view -> ActionPref ao bexp)
        = actionPref (simplify ft fns ao) (simplify ft fns bexp)
    simplify ft fns (BExpr.view -> ValueEnv env bexp)
        = valueEnv (simplify ft fns env) (simplify ft fns bexp)
    simplify _ _ ex = ex

instance Simplifiable ActOffer where
    simplify ft fns (ActOffer aos c) = ActOffer (simplify ft fns aos) (simplify ft fns c)

instance Simplifiable Offer where
    simplify ft fns (Offer cId os) = Offer cId (simplify ft fns os)

instance Simplifiable ChanOffer where
    simplify _  _   x@(Quest _)   = x
    simplify ft fns (Exclam vexp) = Exclam (simplify ft fns vexp)

instance Simplifiable VarId where
    simplify _ _ = id
