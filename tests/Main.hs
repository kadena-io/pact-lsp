{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Language.LSP.Test
import Pact.LSP.Server (runWith)
import System.IO
import System.Process
import Test.Hspec


withLSPServer :: ((Handle, Handle) -> IO ()) -> IO ()
withLSPServer f = do
  (inR, inW) <- createPipe
  (outR, outW) <- createPipe

  bracket
    (forkIO $ runWith inR outW)
    killThread
    (const (f (inW, outR)))
  
main :: IO ()
main = hspec $ around withLSPServer $ do
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
