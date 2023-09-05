{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Language.LSP.Test
import Pact.LSP.Server (runWith)
import System.IO
import System.Process
import Test.Hspec
import qualified Colog.Core as L
import Pact.LSP.PactTrace (parseDiagnostics)
import Data.Either (isRight)
import System.Directory
import Control.Monad (forM_)
import Data.List (isSuffixOf)
import Text.Parsec (parse, (<|>), manyTill, space, string, spaces)
import Text.Parsec.Char (char, letter)
import System.IO.Temp (withTempDirectory)
import Language.LSP.Protocol.Types (Diagnostic (..), DiagnosticSeverity (..), Position (..), Range (..))
import Data.Text (Text)
import qualified Data.Text as T

withLSPServer :: ((Handle, Handle) -> IO ()) -> IO ()
withLSPServer f = do
  (inR, inW) <- createPipe
  (outR, outW) <- createPipe

  bracket
    (forkIO $ runWith inR outW  L.logStringStderr)
    killThread
    (const (f (inW, outR)))

toDiagnostic :: Position -> DiagnosticSeverity -> Text -> Diagnostic
toDiagnostic beg@Position{..} s msg = Diagnostic
  { _range = Range beg (Position _line  (_character + 3))
  , _code  = Nothing
  , _source = Nothing
  , _tags = Nothing
  , _relatedInformation = Nothing
  , _message = msg
  , _severity = Just s
  , _data_ = Nothing
  , _codeDescription = Nothing
  }

main :: IO ()
main = do
  hspec $ around withLSPServer $ do
    describe "diagnostic" $ do
      it "should send no diagnostic" $  \(hin, hout) ->
        runSessionWithHandles hin hout defaultConfig fullCaps "tests/data/" $ do
        _ <- openDoc "test.pact" "pact"
        diags <- waitForDiagnostics
        liftIO $ diags `shouldBe` []

      it "should send error diagnostic" $  \(hin, hout) ->
        runSessionWithHandles hin hout defaultConfig fullCaps "tests/data/" $ do
        _ <- openDoc "test-fail.repl" "pact"
        diags <- waitForDiagnostics
        liftIO $ diags `shouldNotBe` []

  hspec $ do
    describe "parser" $ do
      it "should parse selected" $ do
        let
          ex1 = unlines
            [ "pact/offchain.pact:12:0:Trace: Loaded module n_bd7f56c0bc111ea42026912c37ff5da89149d9dc.offchain, hash POpawqVzqc0UVwZfMk8y0Q9jVk00Hk4aAlQWyDjF58Y"
            , "pact/offchain.pact:139:0:Trace: true"
            , "pact/offchain.pact:141:0:Trace: [\"TableCreated\" \"TableCreated\"]"
            , "pact/offchain.repl:46:0:Trace: Commit Tx 2"
            , "Load successful"]

        parseDiagnostics ex1 `shouldBe`
          Right [ ("pact/offchain.pact", toDiagnostic (Position 11 0) DiagnosticSeverity_Information
                    "Loaded module n_bd7f56c0bc111ea42026912c37ff5da89149d9dc.offchain, hash POpawqVzqc0UVwZfMk8y0Q9jVk00Hk4aAlQWyDjF58Y" )
                , ("pact/offchain.pact", toDiagnostic (Position 138 0) DiagnosticSeverity_Information "true")
                , ("pact/offchain.pact", toDiagnostic (Position 140 0) DiagnosticSeverity_Information "[\"TableCreated\" \"TableCreated\"]")
                , ("pact/offchain.repl", toDiagnostic (Position  45 0) DiagnosticSeverity_Information "Commit Tx 2\n")
                ]

        let
          ex2 = unlines
            [ "pact/offchain.repl:47:1:Trace: Verification of n_bd7f56c0bc111ea42026912c37ff5da89149d9dc.offchain failed"
            , ":OutputFailure: pact/offchain.pact:58:17: could not parse (!= public-key \"\"): couldn't find property variable public-key"
            , "Load failed"
            ]
        parseDiagnostics ex2 `shouldBe`
          Right [ ("pact/offchain.repl", toDiagnostic (Position 46 1) DiagnosticSeverity_Information (T.unlines
                                                                               [ "Verification of n_bd7f56c0bc111ea42026912c37ff5da89149d9dc.offchain failed"
                                                                               , ":OutputFailure: pact/offchain.pact:58:17: could not parse (!= public-key \"\"): couldn't find property variable public-key"]))]


        parseDiagnostics "Load successful" `shouldSatisfy` isRight

  hspec $ parallel $ do
    let path = "tests/data/pact-corpus/"
    files <- runIO $ filter (".pact" `isSuffixOf`) <$> listDirectory path
    forM_ files $ \x ->
      it ("should parse pact output of: " <> x) $ do
        fcontent <- liftIO (readFile (path <> x))
        let
          modName = parse (char '(' >> (string "module" <|> string "interface") >> spaces >> manyTill letter space) "" fcontent
          stmt = case modName of
            Left _ -> ""
            Right r -> "\n(typecheck \"" <> r <> "\")\n(verify \"" <>r<>"\")"

        content <- liftIO $ withTempDirectory "./" "tmp" $ \p -> do
          let
            fname = take (length x - 4) x
            fpath = p <> "/" <> fname <> ".repl"
          writeFile fpath (fcontent ++ stmt)
          (_,_, content) <- readProcessWithExitCode "pact" ["-t", fpath] ""
          pure content

        parseDiagnostics content `shouldSatisfy` isRight
