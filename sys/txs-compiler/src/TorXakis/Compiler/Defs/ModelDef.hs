-- |

module TorXakis.Compiler.Defs.ModelDef where

import           TxsDefs                            (ModelDef (ModelDef))

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.BehExprDefs
import           TorXakis.Parser.Data

modelDeclToModelDef :: ModelDecl -> CompilerM ModelDef
modelDeclToModelDef md = do
    be <- toBExpr undefined (modelBExp md)
    return $ ModelDef  [] [] [] be
