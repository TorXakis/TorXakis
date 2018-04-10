-- |

module TorXakis.Compiler.Defs.ModelId where

import           Id                     (Id (Id))
import           TxsDefs                (ModelId (ModelId))

import           TorXakis.Compiler.Data
import           TorXakis.Parser.Data   hiding (nextId)

modelDeclToModelId :: ModelDecl -> CompilerM ModelId
modelDeclToModelId md = do
    mId <- getNextId
    return $ ModelId (modelName md) (Id mId)
