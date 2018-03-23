-- |

module TorXakis.Compiler.ValExpr.ValExpr where

import           ValExpr                             (ValExpr, cstrConst,
                                                      cstrFunc, cstrVar)
import qualified ValExpr
import           VarId                               (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.ValExpr.ConstDefs
import           TorXakis.Parser.Data

expDeclToValExpr :: (HasVarDecls e, HasVarIds e, HasFuncIds e, HasFuncDefs e')
                 => e -> e' -> ExpDecl -> Either Error (ValExpr VarId)
expDeclToValExpr e e' ex = case expChild ex of
    VarRef _ l -> do
        vDecldfDecl <- findVarDecl e l
        case vDecldfDecl of
            Left vDecl -> do
                vId   <- findVarId e (getLoc vDecl)
                return $ cstrVar vId
            Right fDecl -> do
                fId <- findFuncId e (getLoc fDecl)
                -- TODO: check that the function with id 'fId' takes no arguments!
                return $ cstrFunc (getFuncDefT e') fId []
    ConstLit c ->
        return $ cstrConst (constToConstDef c)


