{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |

module Pact.LSP.Handlers where

import Pact.LSP.Types
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Types hiding (SemanticTokenAbsolute(..))
import Language.LSP.Protocol.Message

import Control.Monad.Trans (MonadTrans (..), liftIO)
import qualified Data.Text as T
import Control.Monad.Except (MonadError(..))
import System.Process (readProcessWithExitCode)
import Pact.LSP.PactTrace (parseDiagnostics)
import Control.Lens ((^.))
import Language.LSP.Server (Handlers, LspT, notificationHandler, getConfig, sendNotification, getVirtualFile, requestHandler)
import Language.LSP.VFS (VirtualFile(..))
import qualified Data.Text.Utf16.Rope as Rope
import Data.Text.Utf16.Rope (Rope)
import Control.Lens.Getter (view)
import Control.Lens.Setter (set)
import Data.Char (isSpace, isLetter)
import qualified Data.List as L
import System.Exit (ExitCode(ExitSuccess))

liftLsp :: LspT ServerConfig IO a -> HandlerM a
liftLsp = HandlerM . lift

initializeHandler :: Handlers HandlerM
initializeHandler = notificationHandler SMethod_Initialized $ const (pure ())

documentChangeNotificationHandler :: Handlers HandlerM
documentChangeNotificationHandler = notificationHandler SMethod_TextDocumentDidChange $ const (pure ())

workspaceChangeNotificationHandler :: Handlers HandlerM
workspaceChangeNotificationHandler = notificationHandler SMethod_WorkspaceDidChangeConfiguration $ const (pure ())

workspaceDidChangeWatchedFilesHandler :: Handlers HandlerM
workspaceDidChangeWatchedFilesHandler = notificationHandler SWorkspaceDidChangeWatchedFiles $ const (pure ())

documentOpenNotificationHandler :: Handlers HandlerM
documentOpenNotificationHandler = notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
  let _uri = msg ^. params.textDocument.uri
  documentDiagnostics _uri

documentCloseNotificationHandler :: Handlers HandlerM
documentCloseNotificationHandler = notificationHandler SMethod_TextDocumentDidClose $ const (pure ())

documentSaveNotificationHandler :: Handlers HandlerM
documentSaveNotificationHandler = notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
  let _uri = msg^.params.textDocument.uri
  documentDiagnostics _uri


documentDiagnostics ::  Uri -> HandlerM ()
documentDiagnostics _uri = do
  let
    nuri = toNormalizedUri _uri
    nfp = uriToNormalizedFilePath nuri

  vfM <- liftLsp $ getVirtualFile nuri
  vfRope <- case vfM of
    Nothing -> throwError (VirtualFileNotFound (T.pack (show nuri)))
    Just (VirtualFile _ _ rope) -> pure rope

  _diagnostics <- case nfp of
    Nothing -> throwError (UnkownError ("Error reading " <> T.pack (show nfp)))
    Just fp -> do
      let cmd = ["-r", "-t", fromNormalizedFilePath fp]
      srvCfg <-liftLsp getConfig

      (_,_, stderr) <- liftIO $ readProcessWithExitCode (pactExe srvCfg) cmd mempty
      case parseDiagnostics stderr of
          Left err -> throwError (PactTraceParseError (T.pack (show err)))
          Right diagsRes -> do
            let
              rawFileDiags = filter (\diag -> toNormalizedFilePath (fst diag) == fp) diagsRes
              diags = enhanceEndPos vfRope <$> map snd rawFileDiags
            pure diags
  let
     _version = Nothing
  liftLsp (sendNotification SMethod_TextDocumentPublishDiagnostics PublishDiagnosticsParams{..})




enhanceEndPos :: Rope -> Diagnostic -> Diagnostic
enhanceEndPos rope diag = case textAfterPosM of
  Nothing -> diag
  Just txtAfter -> case endPos txtAfter of
    -- single line case
    (0, 0, colOffset, _) -> set (range.end.character) (pos ^. character + colOffset +1) diag
    -- multiline case
    (_, lOffset, colOffset, n) ->
      if n < 64 then
        let newEndPos = Position (pos ^. line + lOffset) colOffset
        in set (range.end) newEndPos diag
      else -- take keyword
        let newEndOffset = min 16 (T.length (T.takeWhile isLetter txtAfter))
        in  set (range.end.character) (pos ^. character + fromIntegral newEndOffset +1) diag
  where
    pos = view (range.start) diag
    ropePos = Rope.Position (fromIntegral (_line pos)) (fromIntegral (_character pos +1))
    textAfterPosM = Rope.toText . snd <$> Rope.splitAtPosition ropePos rope

    accum '(' (open, lineCount, charCount, total) = (open+1, lineCount, charCount+1, total+1)
    accum ')' (open, lineCount, charCount, total) = (open-1, lineCount, charCount+1, total+1)
    accum '\n' (open, lineCount, _, total) = (open, lineCount+1, 0, total+1)
    accum _ (open, lineCount, charCount, total) = (open, lineCount, charCount+1, total+1)
    endPos txt = T.foldl' (\a@(open, _, _, total) c -> if open == 0 || total > 128 then a else accum c a) (1::UInt, 0::UInt, 0::UInt, 0::UInt) txt


hoverRequestHandler :: Handlers HandlerM
hoverRequestHandler = requestHandler SMethod_TextDocumentHover  $ \req resp -> do
  let
    uri_ = req ^. params.textDocument.uri
    pos  = req ^. params.position

  srvCfg <-liftLsp getConfig
  vfM <- liftLsp $ getVirtualFile (toNormalizedUri uri_)
  mSymbol <- case vfM of
      Nothing -> throwError (VirtualFileNotFound (T.pack (show uri_)))
      Just (VirtualFile _ _ rope) ->
        let
          (_, l) = Rope.splitAtLine (fromIntegral (pos ^. line)) rope
          (pre, post) = T.splitAt (fromIntegral (pos ^. character)) (Rope.toText l)
          suff = T.takeWhile (not.isSpace) post
          pref = T.takeWhileEnd (\s -> s /= '(' && s /= ' ') pre
          sym = pref <> suff
        in pure (L.find (== sym) pactBuiltins)

  case mSymbol of
    Nothing -> resp (Right (InR Null))
    Just sym -> do
      (exCode, stdout, _) <- liftIO $ readProcessWithExitCode (pactExe srvCfg) [] (T.unpack sym)
      let _contents = InL $ MarkupContent MarkupKind_PlainText (T.pack stdout)
          _range = Nothing

      if exCode == ExitSuccess
        then resp (Right (InL Hover{..}))
        else resp (Right (InR Null))

completionRequestHandler :: Handlers HandlerM
completionRequestHandler = requestHandler SMethod_TextDocumentCompletion $ \req resp -> do
   let
    uri_ = req ^. params.textDocument.uri
    pos  = req ^. params.position

   vfM <- liftLsp $ getVirtualFile (toNormalizedUri uri_)
   symbols <- case vfM of
      Nothing -> throwError (VirtualFileNotFound (T.pack (show uri_)))
      Just (VirtualFile _ _ rope) ->
        let
          (_, l) = Rope.splitAtLine (fromIntegral (pos ^. line)) rope
          (pre, _) = T.splitAt (fromIntegral (pos ^. character)) (Rope.toText l)
          pref = T.takeWhileEnd (\s -> s /= '(' && s /= ' ') pre
        in pure (L.filter (pref `T.isPrefixOf`) pactBuiltins)
   if null symbols
     then resp (Right (InL []))
     else resp (Right (InL (toComp <$> symbols)))

   where
     toComp str = CompletionItem str
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
