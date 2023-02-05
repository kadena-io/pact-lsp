{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 

module Pact.LSP.PactTrace where

import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import Language.LSP.Types (Diagnostic (..), Range (Range), Position (Position), DiagnosticSeverity (..))
import qualified Data.Text as T

parseDiagnostics :: String -> Either ParseError [(FilePath, Diagnostic)]
parseDiagnostics = parse p "repl"
  where
    ppass = string "Load successful"
    pfail = string "Load failed"
    p = do
      diag <- many (try parseLine)
      _ <- try ppass <|> pfail
      pure diag

parseLine :: Parser (FilePath, Diagnostic)
parseLine = do
  fp <- manyTill anyChar (char ':')
  ln <- read <$> many1 digit
  _ <- char ':'
  col <- read <$> many1 digit
  _ <- char ':'
  _severity <- toSeverity <$> (spaces *> manyTill anyChar (char ':'))
  _message <- case _severity of
    Just DsError -> T.pack <$> manyTill anyChar (try (string "Load failed"))
    _others -> T.pack <$> manyTill anyChar endOfLine
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
    toSeverity = \case
      "Trace" -> Just DsInfo
      "Warning" -> Just DsWarning
      "Error" -> Just DsError
      "OutputFailure" -> Just DsError
      "OutputWarning" -> Just DsWarning
      "FAILURE" -> Just DsWarning
      _ -> Nothing

