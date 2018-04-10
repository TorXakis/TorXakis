-- |

module TorXakis.Parser.BExpDecl where

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

bexpDeclP :: TxsParser BExpDecl
bexpDeclP = beStopP

beStopP :: TxsParser BExpDecl
beStopP = txsSymbol "STOP" >> return Stop
