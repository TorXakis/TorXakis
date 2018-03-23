-- |

module TorXakis.Compiler.ValExpr.FuncDef where

import           Control.Arrow                     (left)
import           Data.Either                       (partitionEithers)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Monoid                       (mempty, (<>))
import qualified Data.Text                         as T
import           GHC.Exts                          (fromList)

import           FuncDef                           (FuncDef (FuncDef))
import           FuncId                            (FuncId)
import           ValExpr                           (cstrVar)
import           VarId                             (VarId)

import           Control.Monad.Error.Class         (liftEither)
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.ValExpr.FuncId
import           TorXakis.Compiler.ValExpr.ValExpr
import           TorXakis.Parser.Data

funcDeclsToFuncDefs :: (HasSortIds e, HasVarDecls e, HasVarIds e, HasFuncIds e)
                    => e
                    -> [FuncDecl]
                    -> CompilerM (Map FuncId (FuncDef VarId))
funcDeclsToFuncDefs e fs = liftEither $ gFuncDeclsToFuncDefs mempty fs
    where
      gFuncDeclsToFuncDefs :: SEnv (Map FuncId (FuncDef VarId))
                           -> [FuncDecl]
                           -> Either Error (Map FuncId (FuncDef VarId))
      gFuncDeclsToFuncDefs e' gs =
          case partitionEithers (funcDeclToFuncDef e e' <$> gs) of
              ([], rs) -> Right $ fromSEnv $ fromList rs <> e'
              (ls, []) -> Left  $ T.pack $ "Could not resolve " ++ show ls
              (ls, rs) -> gFuncDeclsToFuncDefs (e' <> fromList rs) ls

-- | TODO: we pass two environments, since e cannot be extended. We should try
-- to solve this by trying to replace IEnv with types like:
--
-- > newtype SEnv t = SEnv t
-- > data CEnv t w  = CEnv t w
--
-- And use type-families (or some other type-level) trick to avoid the
-- overlapping instances problem.
funcDeclToFuncDef :: (HasSortIds e, HasVarDecls e, HasVarIds e, HasFuncIds e, HasFuncDefs e')
                  => e
                  -> e'
                  -> FuncDecl
                  -> Either FuncDecl (FuncId, FuncDef VarId)
funcDeclToFuncDef e e' f = left (const f) $ do
    fId  <- findFuncId e (getLoc f)
    pIds <- traverse (findVarId e . getLoc) (funcParams f)
    vExp <- expDeclToValExpr e e' (funcBody f)
    return (fId, FuncDef pIds vExp)

