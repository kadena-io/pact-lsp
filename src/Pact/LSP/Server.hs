{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Pact.LSP.Server where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Text as T
import           Language.LSP.Server
import qualified Language.LSP.Types as J

import           Pact.LSP.Types
import           Pact.LSP.Handlers
import System.IO (Handle, stdin, stdout, withFile, IOMode (WriteMode))
import Colog.Core
import qualified Colog.Core as L
import Prettyprinter (viaShow, Pretty (pretty))
import Language.LSP.Logging (defaultClientLogger)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as A
import System.Environment (getArgs)
import Language.LSP.Types (TextDocumentSyncOptions (..), TextDocumentSyncKind (TdSyncIncremental))

run :: IO ()
run = getArgs >>= \case
  ["--debug"] -> withFile "pact-lsp-debug.log" WriteMode (runWith stdin stdout . L.logStringHandle)
  _otherwise ->  runWith stdin stdout L.logStringStderr

runWith :: Handle -> Handle -> LogAction IO String -> IO ()
runWith i o l = do
  let
    defaultConfig = ServerConfig {pactExe = "pact"}
    onConfigurationChange old = \case
      Object obj -> case A.lookup "pact" obj of
        Just pactConf -> case A.fromJSON pactConf of
                           A.Success cfg -> Right cfg
                           A.Error msg   -> Left (T.pack msg)
        Nothing -> Right old
      _other -> Right old

    doInitialize env _ = pure (Right env)
    staticHandlers = mconcat
      [ initializeHandler
      , documentOpenNotificationHandler
      , documentCloseNotificationHandler
      , documentSaveNotificationHandler
      , documentChangeNotificationHandler
      , workspaceChangeNotificationHandler
      , hoverRequestHandler
      , completionRequestHandler
      ]
    
    forward :: LanguageContextEnv ServerConfig -> HandlerM a -> IO a
    forward env handler =
        runLspT env $ do
          res <- runHandlerM handler
          case res of
            Left errMsg -> do
              -- send user notification for failure
              sendNotification J.SWindowLogMessage J.LogMessageParams{_xtype = J.MtError, _message = T.pack (show errMsg)}
              liftIO (fail (show errMsg))
            Right a -> pure a

    interpretHandler e = Iso (forward e) liftIO
    options = defaultOptions{textDocumentSync = Just syncOpt}
    
  void (runServerWithHandles ioLogger lspLogger i o ServerDefinition{..})
  where
    prettyMsg m = "[" <> viaShow (L.getSeverity m) <> "] " <> pretty (L.getMsg m)
    ioLogger :: LogAction IO (WithSeverity LspServerLog)
    ioLogger = L.cmap (show . prettyMsg) l
    lspLogger :: LogAction (LspM config) (WithSeverity LspServerLog)
    lspLogger =
      let clientLogger = L.cmap (fmap (T.pack . show . pretty)) defaultClientLogger
      in clientLogger <> L.hoistLogAction liftIO ioLogger

    syncOpt :: TextDocumentSyncOptions
    syncOpt = TextDocumentSyncOptions
      { _openClose = Just True
      , _change = Just TdSyncIncremental
      , _willSave = Just False
      , _willSaveWaitUntil = Just False
      , _save = Just (J.InR (J.SaveOptions (Just True)))
      }
