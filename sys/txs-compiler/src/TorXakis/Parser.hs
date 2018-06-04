{-# LANGUAGE TemplateHaskell #-}
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
import           Control.Lens               (Lens', to, (%~), (^.))
import           Control.Lens.TH            (makeLenses)
import           Control.Monad.Identity     (runIdentity)
import qualified Data.Text                  as T
import           Text.Parsec                (ParseError, eof, errorPos, many,
                                             runParserT, sourceColumn,
                                             sourceLine, try, (<|>))

import           TorXakis.Compiler.Error    (Error (Error), ErrorLoc (ErrorLoc),
                                             ErrorType (ParseError),
                                             errorColumn, errorLine, _errorLoc,
                                             _errorMsg, _errorType)
import           TorXakis.Parser.ChanDecl
import           TorXakis.Parser.CnectDecl
import           TorXakis.Parser.Common     (TxsParser, txsWhitespace)
import           TorXakis.Parser.ConstDecl  (constDeclsP)
import           TorXakis.Parser.Data
import           TorXakis.Parser.FuncDefs   (fdeclP)
import           TorXakis.Parser.MapperDecl
import           TorXakis.Parser.ModelDecl
import           TorXakis.Parser.ProcDecl
import           TorXakis.Parser.PurpDecl
import           TorXakis.Parser.StautDecl
import           TorXakis.Parser.TypeDefs   (adtP)

parse :: Int            -- ^ Initial value for the unique id's counter
      -> String         -- ^ Name of the source from which the input was read.
      -> String         -- ^ Input for the parser
      -> TxsParser a    -- ^ Parser to run
      -> Either Error a
parse uid source input parser = left parseErrorAsError $
    runIdentity (runParserT parser (mkState uid) source input)

parseFile :: FilePath -> IO (Either Error ParsedDefs)
parseFile fp = parseString fp <$> readFile fp

parseString :: FilePath -> String -> Either Error ParsedDefs
parseString fp input = left parseErrorAsError $
    runIdentity (runParserT txsP (mkState 1000) fp input)

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
data TLDef = TLADT       ADTDecl
           | TLFunc      FuncDecl -- TODO: make this a list of '[FuncDecl]', and rename to 'TLFuncs'
           | TLConsts    [FuncDecl]
           | TLModel     ModelDecl
           | TLChanDecls [ChanDecl]
           | TLProcDecl  ProcDecl
           | TLStautDecl StautDecl
           | TLPurpDecl  PurpDecl
           | TLCnectDecl CnectDecl
           | TLMapper    MapperDecl

-- | Group a list of top-level definitions per-type.
asParsedDefs :: [TLDef] -> ParsedDefs
asParsedDefs = foldr sep emptyPds
    where
      sep (TLADT a)         = adts %~ (a:)
      sep (TLFunc f)        = funcs %~ (f:)
      sep (TLConsts cs)     = consts %~ (cs++)
      sep (TLModel m)       = models %~ (m:)
      sep (TLChanDecls chs) = chdecls %~ (chs++)
      sep (TLProcDecl p)    = procs %~ (p:)
      sep (TLStautDecl s)   = stauts %~ (s:)
      sep (TLPurpDecl p)    = purps %~ (p:)
      sep (TLCnectDecl c)   = cnects %~ (c:)
      sep (TLMapper c)      = mappers %~ (c:)

-- | Root parser for the TorXakis language.
txsP :: TxsParser ParsedDefs
txsP = do
    txsWhitespace
    ts <- many $  fmap TLADT       adtP
              <|> fmap TLFunc      fdeclP
              <|> fmap TLConsts    (try constDeclsP)
              <|> fmap TLModel     (try modelDeclP)
              <|> fmap TLChanDecls (try chanDeclsP)
              <|> fmap TLProcDecl  procDeclP
              <|> fmap TLStautDecl stautDeclP
              <|> fmap TLPurpDecl  purpDeclP
              <|> fmap TLCnectDecl cnectDeclP
              <|> fmap TLMapper    mapperDeclP        -- TODO: Be consistent (decl or not?)
    eof
    return $ asParsedDefs ts
