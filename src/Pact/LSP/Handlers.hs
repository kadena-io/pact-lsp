{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |

module Pact.LSP.Handlers where

import Language.LSP.Types
import Pact.LSP.Types
import Language.LSP.Types.Lens
import Control.Monad.Trans (MonadTrans (..), liftIO)
import qualified Data.Text as T
import Control.Monad.Except (MonadError(..))
import System.Process (readProcessWithExitCode)
import Pact.LSP.PactTrace (parseDiagnostics)
import Control.Lens ((^.))
import Language.LSP.Server (Handlers, LspT, notificationHandler, getConfig, sendNotification)

liftLsp :: LspT ServerConfig IO a -> HandlerM a
liftLsp = HandlerM . lift

initializeHandler :: Handlers HandlerM
initializeHandler = notificationHandler SInitialized $ const (pure ())

documentChangeNotificationHandler :: Handlers HandlerM
documentChangeNotificationHandler = notificationHandler STextDocumentDidChange $ const (pure ())

workspaceChangeNotificationHandler :: Handlers HandlerM
workspaceChangeNotificationHandler = notificationHandler SWorkspaceDidChangeConfiguration $ const (pure ())

documentOpenNotificationHandler :: Handlers HandlerM
documentOpenNotificationHandler = notificationHandler STextDocumentDidOpen $ \msg -> do
  let _uri = msg ^. params.textDocument.uri
  documentDiagnostics _uri

documentSaveNotificationHandler :: Handlers HandlerM
documentSaveNotificationHandler = notificationHandler STextDocumentDidSave $ \msg -> do
  let _uri = msg^.params.textDocument.uri
  documentDiagnostics _uri


documentDiagnostics ::  Uri -> HandlerM ()
documentDiagnostics _uri = do
  let
    nuri = toNormalizedUri _uri
    nfp = uriToNormalizedFilePath nuri
  _diagnostics <- case nfp of
    Nothing -> throwError (UnkownError ("Error reading " <> T.pack (show nfp)))
    Just fp -> do
      let cmd = ["-r", "-t", fromNormalizedFilePath fp]
      srvCfg <-liftLsp getConfig

      (_,_, stderr) <- liftIO $ readProcessWithExitCode (pactExe srvCfg) cmd mempty
      case parseDiagnostics stderr of
          Left err -> throwError (PactTraceParseError (T.pack (show err)))
          Right diagsRes -> do
            let fileDiags = filter (\diag -> toNormalizedFilePath (fst diag) == fp) diagsRes
                (_, diags) = unzip fileDiags
            pure (List diags)
  let
     _version = Nothing
  liftLsp (sendNotification STextDocumentPublishDiagnostics PublishDiagnosticsParams{..})
