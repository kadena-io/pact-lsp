{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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


pactBuiltins :: [Text]
pactBuiltins =
  ["!="
  , "&"
  , "*"
  , "+"
  , "-"
  , "/"
  , "<"
  , "<="
  , "="
  , ">"
  , ">="
  , "CHARSET_ASCII"
  , "CHARSET_LATIN1"
  , "^"
  , "abs"
  , "add-time"
  , "and"
  , "and?"
  , "at"
  , "base64-decode"
  , "base64-encode"
  , "bind"
  , "ceiling"
  , "chain-data"
  , "compose"
  , "compose-capability"
  , "concat"
  , "constantly"
  , "contains"
  , "continue"
  , "create-capability-guard"
  , "create-capability-pact-guard"
  , "create-module-guard"
  , "create-pact-guard"
  , "create-principal"
  , "create-table"
  , "create-user-guard"
  , "days"
  , "decrypt-cc20p1305"
  , "define-keyset"
  , "define-namespace"
  , "describe-keyset"
  , "describe-module"
  , "describe-namespace"
  , "describe-table"
  , "diff-time"
  , "distinct"
  , "drop"
  , "emit-event"
  , "enforce"
  , "enforce-guard"
  , "enforce-keyset"
  , "enforce-one"
  , "enforce-pact-version"
  , "enumerate"
  , "exp"
  , "filter"
  , "floor"
  , "fold"
  , "fold-db"
  , "format"
  , "format-time"
  , "hash"
  , "hours"
  , "identity"
  , "if"
  , "insert"
  , "install-capability"
  , "int-to-str"
  , "is-charset"
  , "is-principal"
  , "keylog"
  , "keys"
  , "keys-2"
  , "keys-all"
  , "keys-any"
  , "keyset-ref-guard"
  , "length"
  , "list"
  , "list-modules"
  , "ln"
  , "log"
  , "make-list"
  , "map"
  , "minutes"
  , "mod"
  , "namespace"
  , "not"
  , "not?"
  , "or"
  , "or?"
  , "pact-id"
  , "pact-version"
  , "pairing-check"
  , "parse-time"
  , "point-add"
  , "public-chain-data"
  , "read"
  , "read-decimal"
  , "read-integer"
  , "read-keyset"
  , "read-msg"
  , "read-string"
  , "remove"
  , "require-capability"
  , "resume"
  , "reverse"
  , "round"
  , "scalar-mult"
  , "select"
  , "shift"
  , "sort"
  , "sqrt"
  , "str-to-int"
  , "str-to-list"
  , "take"
  , "time"
  , "try"
  , "tx-hash"
  , "txids"
  , "txlog"
  , "typeof"
  , "typeof-principal"
  , "update"
  , "validate-keypair"
  , "validate-principal"
  , "verify-spv"
  , "where"
  , "with-capability"
  , "with-default-read"
  , "with-read"
  , "write"
  , "xor"
  , "yield"
  , "zip"
  , "|"
  , "~"]
