module TorXakis.Parser.MapperDecl where

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.ChanRef

mapperDeclP :: TxsParser MapperDecl
mapperDeclP =  do
    txsSymbol "MAPPERDEF"
    l  <- mkLoc
    n  <- txsLexeme identifier
    txsSymbol "::="
    is <- chansInDecl
    os <- chansOutDecl
    ys <- chansSyncDecl
    txsSymbol "BEHAVIOUR"
    be <- bexpDeclP
    txsSymbol "ENDDEF"
    return $ mkMapperDecl n l is os ys be
