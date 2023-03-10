{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | 

module Pact.LSP.PactTrace where

import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import Language.LSP.Types (Diagnostic (..), Range (Range), Position (Position), DiagnosticSeverity (..))
import qualified Data.Text as T
import Control.Monad (void)
import Data.Functor (($>))

type ParsedDiagnostic = (FilePath, Diagnostic)

parseDiagnostics :: String -> Either ParseError [ParsedDiagnostic]
parseDiagnostics = parse ((parseEnding $> []) <|> (sepBy parseEntry newline <* parseEnding)) "repl"

parseEnding :: Parser ()
parseEnding = string "Load " >> (string "successful"  <|> string "failed") $> ()

parseEntry :: Parser (FilePath, Diagnostic)
parseEntry = do
  (fp, ln, col, _severity) <- pInfo
  msg <- many (notFollowedBy (void parseEnding <|> void (newline >> pInfo)) *> anyChar)
  let
    _range = toRange ln col
    _code = Nothing
    _source = Nothing
    _tags = Nothing
    _relatedInformation = Nothing
    _message = T.pack msg
  pure (fp, Diagnostic{..})
  
  where
    pcolon = void (char ':')
    string' str = try (string str)
    parseSeverity = choice
      [ string' "OutputWarning" $> DsWarning
      , string' "OutputFailure" $> DsError
      , string' "Trace"         $> DsInfo
      , string' "Error"         $> DsError
      , string' " error"        $> DsError
      ]
    toRange ln col = let ln' = ln-1
                     in Range (Position ln' col) (Position ln' (col+3))
    pInfo = do
      fp <- many1 (noneOf ":\n")
      pcolon
      ln <- read <$> many1 digit
      pcolon
      col <- read <$> many1 digit
      pcolon
      serv <- optionMaybe $ parseSeverity <*  pcolon
      pure (fp, ln, col, serv)
