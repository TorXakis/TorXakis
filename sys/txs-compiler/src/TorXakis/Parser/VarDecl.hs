module TorXakis.Parser.VarDecl where


import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs

varDeclP :: TxsParser VarDecl
varDeclP = mkVarDecl <$> lcIdentifier <*> mkLoc <*> ofSortP
