{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Exit
import Pact.LSP.Server

main :: IO ()
main = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

