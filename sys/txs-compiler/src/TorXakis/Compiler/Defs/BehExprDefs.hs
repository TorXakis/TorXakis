module TorXakis.Compiler.Defs.BehExprDefs where

import           TxsDefs                (BExpr, stop)

import           TorXakis.Compiler.Data
import           TorXakis.Parser.Data

bexpDeclToExprDefs :: BExpDecl -> CompilerM BExpr
bexpDeclToExprDefs Stop          = return stop
bexpDeclToExprDefs (ActPref _ _) = undefined
