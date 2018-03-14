module TorXakis.Compiler where

--import           Data.Text (Text)
import           Control.Arrow ((|||))
    
import           Id      (Id)
import           TxsDefs (TxsDefs)
import           Sigs    (Sigs)
import           VarId   (VarId)

import           TorXakis.Compiler.Error (Error)

-- | Compile a string into a TorXakis model.
--
-- TODO: for now we use String's to be able to leverage on the 'haskell' token
-- parser, in the future we might want to change this to text, and benchmark
-- what is the performance gain.
compile :: String -> Either Error (Id, TxsDefs, Sigs VarId)
compile = undefined

-- | Legacy compile function, used to comply with the old interface. It should
-- be deprecated in favor of @compile@.
compileLegacy :: String -> (Id, TxsDefs, Sigs VarId)
compileLegacy = (throwOnLeft ||| id) . compile 
    where throwOnLeft = error . show
