{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Pact.LSP.Server where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Text as T
import           Language.LSP.Server
import qualified Language.LSP.Types as J

import           Pact.LSP.Types
import           Pact.LSP.Handlers
import System.IO (Handle, stdin, stdout)
import Colog.Core
import qualified Colog.Core as L
import Data.Text.Prettyprint.Doc (viaShow, Pretty (pretty))
import Language.LSP.Logging (defaultClientLogger)

run :: IO ()
run = runWith stdin stdout

runWith :: Handle -> Handle -> IO ()
runWith i o = do  
  let
    defaultConfig = ServerConfig {pactExe = "pact"}
    onConfigurationChange _ new = case A.fromJSON new of
                                      A.Success cfg -> Right cfg
                                      A.Error msg   -> Left (T.pack msg)
    doInitialize env _ = pure (Right env)
    staticHandlers = mconcat
      [ initializeHandler
      , documentOpenNotificationHandler
      , documentSaveNotificationHandler
      , documentChangeNotificationHandler
      , workspaceChangeNotificationHandler
      , hoverRequestHandler
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
    options = defaultOptions
    
  void (runServerWithHandles ioLogger lspLogger i o ServerDefinition{..})
  where
    prettyMsg l = "[" <> viaShow (L.getSeverity l) <> "] " <> pretty (L.getMsg l)
    ioLogger :: LogAction IO (WithSeverity LspServerLog)
    ioLogger = L.cmap (show . prettyMsg) L.logStringStderr
    lspLogger :: LogAction (LspM config) (WithSeverity LspServerLog)
    lspLogger =
      let clientLogger = L.cmap (fmap (T.pack . show . pretty)) defaultClientLogger
      in clientLogger <> L.hoistLogAction liftIO ioLogger
