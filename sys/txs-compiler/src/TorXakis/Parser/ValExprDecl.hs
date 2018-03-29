-- |

module TorXakis.Parser.ValExprDecl where

import           Text.Parsec              (optionMaybe, sepBy, (<|>))

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs

valExpP :: TxsParser ExpDecl
valExpP =  mkVarExp <$> lcIdentifier <*> getMetadata
       <|> mkBoolConstExp <$> txsBoolP <*> getMetadata
       <|> mkIntConstExp <$> txsIntP <*> getMetadata
       <|> mkStringConstExp <$> txsStringP <*> getMetadata
       <|> letExpP

letExpP :: TxsParser ExpDecl
letExpP = do
    m <- getMetadata
    txsSymbol "LET"
    vs <- letVarDeclsP
    txsSymbol "IN"
    subEx <- valExpP
    txsSymbol "NI"
    return $ mkLetExpDecl vs subEx m

letVarDeclsP :: TxsParser [LetVarDecl]
letVarDeclsP = letVarDeclP `sepBy` txsSymbol ";"

letVarDeclP :: TxsParser LetVarDecl
letVarDeclP = do
    m <- getMetadata
    v <- txsLexeme lcIdentifier
    ms <- optionMaybe ofSortP
    txsSymbol "="
    subEx <- valExpP
    return $ mkLetVarDecl v ms subEx m

txsBoolP :: TxsParser Bool
txsBoolP =  (txsSymbol "True" >> return True)
        <|> (txsSymbol "False" >> return False)

