-- |

module TorXakis.Compiler.ValExpr.ValExpr where

import           ValExpr                             (ValExpr, cstrConst,
                                                      cstrVar)
import qualified ValExpr
import           VarId                               (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.ValExpr.ConstDefs
import           TorXakis.Parser.Data

expDeclToValExpr :: (HasVarDecls e, HasVarIds e)
                 => e -> ExpDecl -> CompilerM (ValExpr VarId)
expDeclToValExpr e ex = case expChild ex of
    VarRef _ l -> do
        vDecl <- findVarDeclM e l
        vId   <- findVarIdM e (getLoc vDecl)
        return $ cstrVar vId
    ConstLit c ->
        return $ cstrConst (constToConstDef c)


