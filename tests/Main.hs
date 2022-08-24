{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- So we can keep using the old prettyprinter modules (which have a better
-- compatibility range) for now.
{-# OPTIONS_GHC -Wno-deprecations -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-type-defaults #-}

{- |
This is an example language server built with haskell-lsp using a 'Reactor'
design. With a 'Reactor' all requests are handled on a /single thread/.
A thread is spun up for it, which repeatedly reads from a 'TChan' of
'ReactorInput's.
The `lsp` handlers then simply pass on all the requests and
notifications onto the channel via 'ReactorInput's.
This way there is the option of executing requests on multiple threads, without
blocking server communication.

To try out this server, install it with
> cabal install lsp-demo-reactor-server -fdemo
and plug it into your client of choice.
-}
module Main (main) where

import           Colog.Core (LogAction (..), WithSeverity (..))
import qualified Colog.Core as L
import           Pact.LSP.Server
import           Test.Hspec
import           Data.Aeson hiding (defaultOptions)
import qualified Data.Aeson as J
import           Data.Default
import qualified Data.HashMap.Strict as HM
import           Data.Either
import           Data.Maybe
import qualified Data.Text as T
import           Data.Type.Equality
import           Control.Applicative.Combinators
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Lens hiding (List, Iso)
import           Language.LSP.Server hiding (sendRequest)
import           Language.LSP.Test
import           Language.LSP.Types
import           Language.LSP.Types.Lens hiding
  (capabilities, message, rename, applyEdit)
import qualified Language.LSP.Types.Lens as LSP
import           Language.LSP.Types.Capabilities as LSP
import           Language.LSP.Logging (defaultClientLogger)
import           System.Directory
import           System.FilePath
import           System.Process
import           System.IO
import UnliftIO.Concurrent
import UnliftIO


-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------
--

{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}



{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
{-# ANN module ("HLint: ignore Unnecessary hiding" :: String) #-}


withDummyServer :: ((Handle, Handle) -> IO ()) -> IO ()
withDummyServer f = do
  (hinRead, hinWrite) <- createPipe
  (houtRead, houtWrite) <- createPipe

  rin  <- atomically newTChan :: IO (TChan ReactorInput)

  let
    -- Three loggers:
    -- 1. To stderr
    -- 2. To the client (filtered by severity)
    -- 3. To both
    stderrLogger :: LogAction IO (WithSeverity T.Text)
    stderrLogger = L.cmap show L.logStringStderr
    clientLogger :: LogAction (LspM Config) (WithSeverity T.Text)
    clientLogger = defaultClientLogger
    dualLogger :: LogAction (LspM Config) (WithSeverity T.Text)
    dualLogger = clientLogger <> L.hoistLogAction liftIO stderrLogger

    definition = ServerDefinition
          { defaultConfig = Config {fooTheBar = False, wibbleFactor = 0 }
          , onConfigurationChange = \_old v -> do
              case J.fromJSON v of
                J.Error e -> Left (T.pack e)
                J.Success cfg -> Right cfg
          , doInitialize = \env _ -> forkIO (reactor stderrLogger rin) >> pure (Right env)
          -- Handlers log to both the client and stderr
          , staticHandlers = lspHandlers dualLogger rin
          , interpretHandler = \env -> Iso (runLspT env) liftIO
          , options = lspOptions
          }
  bracket
    (forkIO $ void $ runServerWithHandles mempty mempty hinRead houtWrite definition)
    killThread
    (const $ f (hinWrite, houtRead))

main :: IO ()
main = hspec $ around withDummyServer $ do
  describe "Session" $ do
    it "fails a test" $ \(hin, hout) ->
      let session = runSessionWithHandles hin hout def fullCaps "." $ do
                      openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
                      anyRequest
        in session `shouldThrow` anySessionException
    it "initializeResponse" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      rsp <- initializeResponse
      liftIO $ rsp ^. result `shouldSatisfy` isRight

    it "runSessionWithConfig" $ \(hin, hout) ->
      runSessionWithHandles hin hout def didChangeCaps "." $ return ()

    describe "withTimeout" $ do
      it "times out" $ \(hin, hout) ->
        let sesh = runSessionWithHandles hin hout def fullCaps "." $ do
                    openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
                    -- won't receive a request - will timeout
                    -- incoming logging requests shouldn't increase the
                    -- timeout
                    withTimeout 5 $ skipManyTill anyMessage (message SWorkspaceApplyEdit)
          -- wait just a bit longer than 5 seconds so we have time
          -- to open the document
          in timeout 6000000 sesh `shouldThrow` anySessionException

      it "doesn't time out" $ \(hin, hout) ->
        let sesh = runSessionWithHandles hin hout def fullCaps "." $ do
                    openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
                    withTimeout 5 $ skipManyTill anyMessage publishDiagnosticsNotification
          in void $ timeout 6000000 sesh

      it "further timeout messages are ignored" $ \(hin, hout) ->
        runSessionWithHandles hin hout def fullCaps "." $ do
          doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
          -- shouldn't timeout
          withTimeout 3 $ getDocumentSymbols doc
          -- longer than the original timeout
          liftIO $ threadDelay (5 * 10^6)
          -- shouldn't throw an exception
          getDocumentSymbols doc
          return ()

      it "overrides global message timeout" $ \(hin, hout) ->
        let sesh =
              runSessionWithHandles hin hout (def { messageTimeout = 5 }) fullCaps "." $ do
                doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
                -- shouldn't time out in here since we are overriding it
                withTimeout 10 $ liftIO $ threadDelay 7000000
                getDocumentSymbols doc
                return True
        in sesh `shouldReturn` True

      it "unoverrides global message timeout" $ \(hin, hout) ->
        let sesh =
              runSessionWithHandles hin hout (def { messageTimeout = 5 }) fullCaps "." $ do
                doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
                -- shouldn't time out in here since we are overriding it
                withTimeout 10 $ liftIO $ threadDelay 7000000
                getDocumentSymbols doc
                -- should now timeout
                skipManyTill anyMessage (message SWorkspaceApplyEdit)
            isTimeout (Timeout _) = True
            isTimeout _ = False
        in sesh `shouldThrow` isTimeout


    describe "SessionException" $ do
      it "throw on time out" $ \(hin, hout) ->
        let sesh = runSessionWithHandles hin hout (def {messageTimeout = 10}) fullCaps "." $ do
                skipMany loggingNotification
                _ <- message SWorkspaceApplyEdit
                return ()
        in sesh `shouldThrow` anySessionException

      it "don't throw when no time out" $ \(hin, hout) ->
        runSessionWithHandles hin hout (def {messageTimeout = 5}) fullCaps "." $ do
          loggingNotification
          liftIO $ threadDelay $ 6 * 1000000
          _ <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
          return ()

      describe "UnexpectedMessageException" $ do
        it "throws when there's an unexpected message" $ \(hin, hout) ->
          let selector (UnexpectedMessage "Publish diagnostics notification" (FromServerMess SWindowLogMessage _)) = True
              selector _ = False
            in runSessionWithHandles hin hout def fullCaps "." publishDiagnosticsNotification `shouldThrow` selector
        it "provides the correct types that were expected and received" $ \(hin, hout) ->
          let selector (UnexpectedMessage "Response for: STextDocumentRename" (FromServerRsp STextDocumentDocumentSymbol _)) = True
              selector _ = False
              sesh = do
                doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
                sendRequest STextDocumentDocumentSymbol (DocumentSymbolParams Nothing Nothing doc)
                skipMany anyNotification
                response STextDocumentRename -- the wrong type
            in runSessionWithHandles hin hout def fullCaps "." sesh
              `shouldThrow` selector

  describe "text document VFS" $ do
    it "sends back didChange notifications (documentChanges)" $ \(hin, hout) ->
      runSessionWithHandles hin hout def fullCaps "." $ do
        doc <- openDoc "tests/data/refactor/Main.hs" "haskell"
        VersionedTextDocumentIdentifier _ beforeVersion <- getVersionedDoc doc

        let args = toJSON (VersionedTextDocumentIdentifier (doc ^. uri) beforeVersion)
            reqParams = ExecuteCommandParams Nothing "doAVersionedEdit" (Just (List [args]))

        request_ SWorkspaceExecuteCommand reqParams

        editReq <- message SWorkspaceApplyEdit
        liftIO $ do
          let Just (List [InL(TextDocumentEdit vdoc (List [InL edit_]))]) =
                editReq ^. params . edit . documentChanges
          vdoc `shouldBe` VersionedTextDocumentIdentifier  (doc ^. uri) beforeVersion
          edit_ `shouldBe` TextEdit (Range (Position 0 0) (Position 0 5)) "howdy"

        change <- customNotification "custom/textDocument/didChange"
        let NotMess (NotificationMessage _ _ (c::Value)) = change
            Success (DidChangeTextDocumentParams reportedVDoc _edit) = fromJSON c
            VersionedTextDocumentIdentifier _ reportedVersion = reportedVDoc

        contents <- documentContents doc

        liftIO $ contents `shouldBe` "howdy:: IO Int\nmain = return (42)\n"
        VersionedTextDocumentIdentifier _ afterVersion <- getVersionedDoc doc
        liftIO $ afterVersion `shouldNotBe` beforeVersion

        liftIO $ reportedVersion `shouldNotBe` beforeVersion

    it "sends back didChange notifications" $ \(hin, hout) ->
      runSessionWithHandles hin hout def fullCaps "." $ do
        doc <- openDoc "tests/data/refactor/Main.hs" "haskell"

        let args = toJSON (doc ^. uri)
            reqParams = ExecuteCommandParams Nothing "doAnEdit" (Just (List [args]))
        request_ SWorkspaceExecuteCommand reqParams

        editReq <- message SWorkspaceApplyEdit
        liftIO $ do
          let (Just cs) = editReq ^. params . edit . changes
              [(u, List es)] = HM.toList cs
          u `shouldBe` doc ^. uri
          es `shouldBe` [TextEdit (Range (Position 0 0) (Position 0 5)) "howdy"]
        contents <- documentContents doc
        liftIO $ contents `shouldBe` "howdy:: IO Int\nmain = return (42)\n"

  describe "getDocumentEdit" $
    it "automatically consumes applyedit requests" $ \(hin, hout) ->
      runSessionWithHandles hin hout def fullCaps "." $ do
        doc <- openDoc "tests/data/refactor/Main.hs" "haskell"

        let args = toJSON (doc ^. uri)
            reqParams = ExecuteCommandParams Nothing "doAnEdit" (Just (List [args]))
        request_ SWorkspaceExecuteCommand reqParams
        contents <- getDocumentEdit doc
        liftIO $ contents `shouldBe` "howdy:: IO Int\nmain = return (42)\n"

  describe "getCodeActions" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      doc <- openDoc "tests/data/refactor/Main.hs" "haskell"
      waitForDiagnostics
      [InR action] <- getCodeActions doc (Range (Position 0 0) (Position 0 2))
      actions <- getCodeActions doc (Range (Position 1 14) (Position 1 18))
      liftIO $ action ^. title `shouldBe` "Delete this"
      liftIO $ actions `shouldSatisfy` null

  describe "getAllCodeActions" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      doc <- openDoc "tests/data/refactor/Main.hs" "haskell"
      _ <- waitForDiagnostics
      actions <- getAllCodeActions doc
      liftIO $ do
        let [InR action] = actions
        action ^. title `shouldBe` "Delete this"
        action ^. command . _Just . command  `shouldBe` "deleteThis"

  describe "getDocumentSymbols" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"

      skipMany loggingNotification

      Left (mainSymbol:_) <- getDocumentSymbols doc

      liftIO $ do
        mainSymbol ^. name `shouldBe` "foo"
        mainSymbol ^. kind `shouldBe` SkObject
        mainSymbol ^. range `shouldBe` mkRange 0 0 3 6

  describe "applyEdit" $ do
    it "increments the version" $ \(hin, hout) -> runSessionWithHandles hin hout def docChangesCaps "." $ do
      doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
      VersionedTextDocumentIdentifier _ (Just oldVersion) <- getVersionedDoc doc
      let edit = TextEdit (Range (Position 1 1) (Position 1 3)) "foo"
      VersionedTextDocumentIdentifier _ (Just newVersion) <- applyEdit doc edit
      liftIO $ newVersion `shouldBe` oldVersion + 1
    it "changes the document contents" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
      let edit = TextEdit (Range (Position 0 0) (Position 0 2)) "foo"
      applyEdit doc edit
      contents <- documentContents doc
      liftIO $ contents `shouldSatisfy` T.isPrefixOf "foodule"

  describe "getCompletions" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"

      comps <- getCompletions doc (Position 5 5)
      let item = head comps
      liftIO $ item ^. label `shouldBe` "foo"

  -- describe "getReferences" $
  --   it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
  --     doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
  --     let pos = Position 40 3 -- interactWithUser
  --         uri = doc ^. LSP.uri
  --     refs <- getReferences doc pos True
  --     liftIO $ refs `shouldContain` map (Location uri) [
  --         mkRange 41 0 41 16
  --       , mkRange 75 6 75 22
  --       , mkRange 71 6 71 22
  --       ]

  -- describe "getDefinitions" $
  --   it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
  --     doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
  --     let pos = Position 49 25 -- addItem
  --     defs <- getDefinitions doc pos
  --     liftIO $ defs `shouldBe` [Location (doc ^. uri) (mkRange 28 0 28 7)]

  -- describe "getTypeDefinitions" $
  --   it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
  --     doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
  --     let pos = Position 20 23  -- Quit value
  --     defs <- getTypeDefinitions doc pos
  --     liftIO $ defs `shouldBe` [Location (doc ^. uri) (mkRange 10 0 14 19)]  -- Type definition

  describe "waitForDiagnosticsSource" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      openDoc "tests/data/Error.hs" "haskell"
      [diag] <- waitForDiagnosticsSource "dummy-server"
      liftIO $ do
        diag ^. severity `shouldBe` Just DsWarning
        diag ^. source `shouldBe` Just "dummy-server"

  -- describe "rename" $ do
  --   it "works" $ \(hin, hout) -> pendingWith "HaRe not in hie-bios yet"
  --   it "works on javascript" $
  --     runSessionWithHandles hin hout "javascript-typescript-stdio" fullCaps "tests/data/javascriptPass" $ do
  --       doc <- openDoc "test.js" "javascript"
  --       rename doc (Position 2 11) "bar"
  --       documentContents doc >>= liftIO . (`shouldContain` "function bar()") . T.unpack

  describe "getHover" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
      hover <- getHover doc (Position 45 9)
      liftIO $ hover `shouldSatisfy` isJust

  -- describe "getHighlights" $
  --   it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
  --     doc <- openDoc "tests/data/renamePass/Desktop/simple.hs" "haskell"
  --     skipManyTill loggingNotification $ count 2 noDiagnostics
  --     highlights <- getHighlights doc (Position 27 4) -- addItem
  --     liftIO $ length highlights `shouldBe` 4

  -- describe "formatDoc" $
  --   it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
  --     doc <- openDoc "tests/data/Format.hs" "haskell"
  --     oldContents <- documentContents doc
  --     formatDoc doc (FormattingOptions 4 True)
  --     documentContents doc >>= liftIO . (`shouldNotBe` oldContents)

  -- describe "formatRange" $
  --   it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
  --     doc <- openDoc "tests/data/Format.hs" "haskell"
  --     oldContents <- documentContents doc
  --     formatRange doc (FormattingOptions 4 True) (Range (Position 1 10) (Position 2 10))
  --     documentContents doc >>= liftIO . (`shouldNotBe` oldContents)

  describe "closeDoc" $
    it "works" $ \(hin, hout) ->
      let sesh =
            runSessionWithHandles hin hout def fullCaps "." $ do
              doc <- openDoc "tests/data/Format.hs" "haskell"
              closeDoc doc
              -- need to evaluate to throw
              documentContents doc >>= liftIO . print
      in sesh `shouldThrow` anyException

  describe "satisfy" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      openDoc "tests/data/Format.hs" "haskell"
      let pred (FromServerMess SWindowLogMessage _) = True
          pred _ = False
      void $ satisfy pred

  describe "satisfyMaybe" $ do
    it "returns matched data on match" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      -- Wait for window/logMessage "initialized" from the server.
      let pred (FromServerMess SWindowLogMessage _) = Just "match" :: Maybe String
          pred _ = Nothing :: Maybe String
      result <- satisfyMaybe pred
      liftIO $ result `shouldBe` "match"

    it "doesn't return if no match" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      let pred (FromServerMess STextDocumentPublishDiagnostics _) = Just "matched" :: Maybe String
          pred _ = Nothing :: Maybe String
      -- We expect a window/logMessage from the server, but
      -- not a textDocument/publishDiagnostics.
      result <- satisfyMaybe pred <|> (message SWindowLogMessage *> pure "no match")
      liftIO $ result `shouldBe` "no match"

  describe "ignoreLogNotifications" $
    it "works" $ \(hin, hout) ->
      runSessionWithHandles hin hout (def { ignoreLogNotifications = True }) fullCaps "." $ do
        openDoc "tests/data/Format.hs" "haskell"
        void publishDiagnosticsNotification

  describe "dynamic capabilities" $ do

    it "keeps track" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      loggingNotification -- initialized log message

      createDoc ".register" "haskell" ""
      message SClientRegisterCapability

      doc <- createDoc "Foo.watch" "haskell" ""
      msg <- message SWindowLogMessage
      liftIO $ msg ^. params . LSP.message `shouldBe` "got workspace/didChangeWatchedFiles"

      [SomeRegistration (Registration _ regMethod regOpts)] <- getRegisteredCapabilities
      liftIO $ do
        case regMethod `mEqClient` SWorkspaceDidChangeWatchedFiles of
          Just (Right HRefl) ->
            regOpts `shouldBe` (DidChangeWatchedFilesRegistrationOptions $ List
                                [ FileSystemWatcher "*.watch" (Just (WatchKind True True True)) ])
          _ -> expectationFailure "Registration wasn't on workspace/didChangeWatchedFiles"

      -- now unregister it by sending a specific createDoc
      createDoc ".unregister" "haskell" ""
      message SClientUnregisterCapability

      createDoc "Bar.watch" "haskell" ""
      void $ sendRequest STextDocumentHover $ HoverParams doc (Position 0 0) Nothing
      count 0 $ loggingNotification
      void $ anyResponse

    it "handles absolute patterns" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "" $ do
      curDir <- liftIO $ getCurrentDirectory

      loggingNotification -- initialized log message

      createDoc ".register.abs" "haskell" ""
      message SClientRegisterCapability

      doc <- createDoc (curDir </> "Foo.watch") "haskell" ""
      msg <- message SWindowLogMessage
      liftIO $ msg ^. params . LSP.message `shouldBe` "got workspace/didChangeWatchedFiles"

      -- now unregister it by sending a specific createDoc
      createDoc ".unregister.abs" "haskell" ""
      message SClientUnregisterCapability

      createDoc (curDir </> "Bar.watch") "haskell" ""
      void $ sendRequest STextDocumentHover $ HoverParams doc (Position 0 0) Nothing
      count 0 $ loggingNotification
      void $ anyResponse

  describe "call hierarchy" $ do
    let workPos = Position 1 0
        notWorkPos = Position 0 0
        params pos = CallHierarchyPrepareParams (TextDocumentIdentifier (Uri "")) pos Nothing
        item = CallHierarchyItem "foo" SkFunction Nothing Nothing (Uri "")
                                 (Range (Position 1 2) (Position 3 4))
                                 (Range (Position 1 2) (Position 3 4))
                                 Nothing
    it "prepare works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      rsp <- prepareCallHierarchy (params workPos)
      liftIO $ head rsp ^. range `shouldBe` Range (Position 2 3) (Position 4 5)
    it "prepare not works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      rsp <- prepareCallHierarchy (params notWorkPos)
      liftIO $ rsp `shouldBe` []
    it "incoming calls" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      [CallHierarchyIncomingCall _ (List fromRanges)] <- incomingCalls (CallHierarchyIncomingCallsParams Nothing Nothing item)
      liftIO $ head fromRanges `shouldBe` Range (Position 2 3) (Position 4 5)
    it "outgoing calls" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      [CallHierarchyOutgoingCall _ (List fromRanges)] <- outgoingCalls (CallHierarchyOutgoingCallsParams Nothing Nothing item)
      liftIO $ head fromRanges `shouldBe` Range (Position 4 5) (Position 2 3)

  describe "semantic tokens" $ do
    it "full works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "." $ do
      let doc = TextDocumentIdentifier (Uri "")
      Just toks <- getSemanticTokens doc
      liftIO $ toks ^. xdata `shouldBe` List [0,1,2,1,0]

didChangeCaps :: ClientCapabilities
didChangeCaps = def { _workspace = Just workspaceCaps }
  where
    workspaceCaps = def { _didChangeConfiguration = Just configCaps }
    configCaps = DidChangeConfigurationClientCapabilities (Just True)

docChangesCaps :: ClientCapabilities
docChangesCaps = def { _workspace = Just workspaceCaps }
  where
    workspaceCaps = def { _workspaceEdit = Just editCaps }
    editCaps = WorkspaceEditClientCapabilities (Just True) Nothing Nothing Nothing Nothing