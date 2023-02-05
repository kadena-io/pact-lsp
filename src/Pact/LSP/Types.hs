{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Pact.LSP.Types where

import Language.LSP.Server
import Data.Text (Text)
import Control.Monad.State
import Control.Monad.Except
import Data.Aeson
import GHC.Generics

data ServerConfig
  = ServerConfig
    { pactExe :: FilePath
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data HandlerError
  = UnkownError !Text
  | VirtualFileNotFound !Text
  | PactTraceParseError !Text
  deriving Show


newtype HandlerM a
  = HandlerM { unHandlerM :: ExceptT HandlerError (LspT ServerConfig IO) a }
  deriving newtype (Functor,Applicative, Monad, MonadIO, MonadError HandlerError)

runHandlerM :: HandlerM a -> LspT ServerConfig IO (Either HandlerError a)
runHandlerM (HandlerM handler) = runExceptT handler
