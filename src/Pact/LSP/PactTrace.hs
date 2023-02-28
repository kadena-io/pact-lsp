{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | 

module Pact.LSP.PactTrace where

import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import Language.LSP.Types (Diagnostic (..), Range (Range), Position (Position), DiagnosticSeverity (..))
import qualified Data.Text as T
import Control.Monad (void)
import Data.Functor (($>))

parseDiagnostics :: String -> Either ParseError [(FilePath, Diagnostic)]
parseDiagnostics = parse p "repl"
  where
    ppass = string "Load successful"
    pfail = string "Load failed"
    p = many (try parseLine) <* (try ppass <|> pfail)

parseLine :: Parser (FilePath, Diagnostic)
parseLine = do
  fp <-  manyTill anyChar (char ':')
  ln <- read <$> many1 digit
  pcolon
  col <- read <$> many1 digit
  pcolon
  (_severity, isCritical) <- pseverity
  pcolon
  _message <- if isCritical
    then T.pack <$> manyTill anyChar (lookAhead (string "Load failed"))
    else T.pack . unlines <$> ptrace

  let
    _range = toRange ln col
    _code = Nothing
    _source = Nothing
    _tags = Nothing
    _relatedInformation = Nothing
  pure (fp, Diagnostic{..})
  where
    toRange ln col =
      let ln' = ln-1 in Range (Position ln' col) (Position ln' (col+3))

    string' str = try (string str)
    pcolon = void (char ':')
    pseverity = choice
      [ string' "OutputWarning" $> (Just DsWarning, False)
      , string' "OutputFailure" $> (Just DsError, False)
      , string' "Trace"         $> (Just DsInfo, False)
      , string' "Error"         $> (Just DsError, False)
      , string' " error"        $> (Just DsError, True)
      , manyTill anyChar (lookAhead (char ':')) $> (Nothing, False)
      ]
    ptrace = do
      hh <- spaces *> manyTill anyChar endOfLine
      b <-  many $ do
        indent <- many1 space
        content <- manyTill anyChar endOfLine
        pure (indent ++ content)
      pure (hh : b)
