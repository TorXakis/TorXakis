{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
--  Parser for the 'TorXakis' language.
--------------------------------------------------------------------------------
module TorXakis.Parser
    ( ParsedDefs
    , adts
    , funcs
    , consts
    , models
    , chdecls
    , procs
    , txsP
    , parseFile
    , parseString
    , parse
    )
where

import           Control.Arrow              (left)
import           Control.Lens               ((%~))
import           Control.Monad.Identity     (runIdentity)
import qualified Data.Text                  as T
import           Text.Parsec                (ParseError, eof, errorPos, many,
                                             runParserT, sourceColumn,
                                             sourceLine, try, (<|>))

import           TorXakis.TxsCompiler.Error    (Error (Error), ErrorLoc (ErrorLoc),
                                             ErrorType (ParseError),
                                             errorColumn, errorLine, _errorLoc,
                                             _errorMsg, _errorType)
import           TorXakis.Parser.ChanDecl   (chanDeclsP)
import           TorXakis.Parser.CnectDecl  (cnectDeclP)
import           TorXakis.Parser.Common     (TxsParser, txsWhitespace)
import           TorXakis.Parser.ConstDecl  (constDeclsP)
import           TorXakis.Parser.Data       (ADTDecl, ChanDecl, CnectDecl,
                                             FuncDecl, MapperDecl, ModelDecl,
                                             ParsedDefs, ProcDecl, PurpDecl,
                                             StautDecl, adts, chdecls, cnects,
                                             consts, emptyPds, funcs, mappers,
                                             mkState, models, procs, purps,
                                             stauts)
import           TorXakis.Parser.FuncDefs   (fDeclP)
import           TorXakis.Parser.MapperDecl (mapperDeclP)
import           TorXakis.Parser.ModelDecl  (modelDeclP)
import           TorXakis.Parser.ProcDecl   (procDeclP)
import           TorXakis.Parser.PurpDecl   (purpDeclP)
import           TorXakis.Parser.StautDecl  (stautDeclP)
import           TorXakis.Parser.TypeDefs   (adtP)

-- | Parse a string using the initial value for the unique id's counter.
parse :: Int            -- ^ Initial value for the unique id's counter
      -> String         -- ^ Name of the source from which the input was read.
      -> String         -- ^ Input for the parser
      -> TxsParser a    -- ^ Parser to run
      -> Either Error a
parse uid source input parser = left parseErrorAsError $
    runIdentity (runParserT parser (mkState uid) source input)

-- | Parse a TorXakis file from file.
parseFile :: FilePath -> IO (Either Error ParsedDefs)
parseFile fp = parseString fp <$> readFile fp

-- | Parse a TorXakis file from a string.
parseString :: FilePath -> String -> Either Error ParsedDefs
parseString fp input = left parseErrorAsError $
    runIdentity (runParserT txsP (mkState 1000) fp input)

-- | Convert a parser error to a compiler error.
parseErrorAsError :: ParseError -> Error
parseErrorAsError err = Error
    { _errorType = ParseError
    , _errorLoc = ErrorLoc
        { errorLine   = sourceLine (errorPos err)
        , errorColumn = sourceColumn (errorPos err)
        }
    , _errorMsg = T.pack (show err)
    }

-- | TorXakis top-level definitions
data TLDef = TLADT    ADTDecl
           | TLFunc   FuncDecl
           | TLConsts [FuncDecl]
           | TLModel  ModelDecl
           | TLChan   [ChanDecl]
           | TLProc   ProcDecl
           | TLStaut  StautDecl
           | TLPurp   PurpDecl
           | TLCnect  CnectDecl
           | TLMapper MapperDecl

-- | Group a list of top-level definitions per-type.
asParsedDefs :: [TLDef] -> ParsedDefs
asParsedDefs = foldr sep emptyPds
    where
      sep (TLADT a)     = adts %~ (a:)
      sep (TLFunc f)    = funcs %~ (f:)
      sep (TLConsts cs) = consts %~ (cs++)
      sep (TLModel m)   = models %~ (m:)
      sep (TLChan chs)  = chdecls %~ (chs++)
      sep (TLProc p)    = procs %~ (p:)
      sep (TLStaut s)   = stauts %~ (s:)
      sep (TLPurp p)    = purps %~ (p:)
      sep (TLCnect c)   = cnects %~ (c:)
      sep (TLMapper c)  = mappers %~ (c:)

-- | Root parser for the TorXakis language.
txsP :: TxsParser ParsedDefs
txsP = do
    txsWhitespace
    ts <- many $  fmap TLADT    adtP
              <|> fmap TLFunc   fDeclP
              <|> fmap TLConsts (try constDeclsP)
              <|> fmap TLModel  (try modelDeclP)
              <|> fmap TLChan   (try chanDeclsP)
              <|> fmap TLProc   procDeclP
              <|> fmap TLStaut  stautDeclP
              <|> fmap TLPurp   purpDeclP
              <|> fmap TLCnect  cnectDeclP
              <|> fmap TLMapper mapperDeclP
    eof
    return $ asParsedDefs ts
