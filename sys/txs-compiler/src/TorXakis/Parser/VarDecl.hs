module TorXakis.Parser.VarDecl where


import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs

varDeclsP :: TxsParser [VarDecl]
varDeclsP = idOfSortsP "" "" mkVarDecl

