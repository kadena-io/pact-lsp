{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |

module Pact.LSP.Handlers where

import Language.LSP.Types hiding (line)
import Pact.LSP.Types
import Language.LSP.Types.Lens
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
import Control.Lens (over)
import Data.Char (isSpace)
import qualified Data.List as L
import System.Exit (ExitCode(ExitSuccess))

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
            let rawFileDiags = filter (\diag -> toNormalizedFilePath (fst diag) == fp) diagsRes
                diags = enhanceEndPos vfRope <$> map snd rawFileDiags
            pure (List diags)
  let
     _version = Nothing
  liftLsp (sendNotification STextDocumentPublishDiagnostics PublishDiagnosticsParams{..})




enhanceEndPos :: Rope -> Diagnostic -> Diagnostic
enhanceEndPos rope diag = case textAfterPosM of
  Nothing -> diag
  Just txtAfter -> case endPos txtAfter of
    (0, 0, colOffset, _) -> over (range.end.character) (+ (colOffset-1)) diag
    (0, lOffset, colOffset, _) ->
       let newEndPos = Position (pos ^. line + lOffset) colOffset
       in set (range.end) newEndPos diag
    _otherwise -> diag
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
hoverRequestHandler = requestHandler STextDocumentHover $ \req resp -> do
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
    Nothing -> resp (Right Nothing)
    Just sym -> do
      (exCode, stdout, _) <- liftIO $ readProcessWithExitCode (pactExe srvCfg) [] (T.unpack sym)
      let _contents = HoverContents (MarkupContent MkPlainText (T.pack stdout))
          _range = Nothing
          
      if exCode == ExitSuccess
        then resp (Right (Just Hover{..}))
        else resp (Right Nothing)


completionRequestHandler :: Handlers HandlerM
completionRequestHandler = requestHandler STextDocumentCompletion $ \req resp -> do
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
     then resp (Right (InR (CompletionList False (List []))))
     else resp (Right (InR (CompletionList False (List (toComp <$> symbols)))))
        
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
