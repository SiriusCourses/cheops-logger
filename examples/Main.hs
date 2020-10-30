{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  where

import Cheops.Logger
import Data.Function

main :: IO ()
main = do
    withLogger config $ \ initial_logger -> do
      let ?logger = initial_logger
      internal
  where
    config = LoggerConfig defCapacity Nothing

internal :: (?logger::LoggerEnv) => IO ()
internal = do
  logDebug ?logger "Hello!"
  logInfo ?logger "World!"
  let ?logger = ?logger
              & addContext (sl "host" ("127.0.0.1"::String))
              & addNamespace "www"
  internal2

internal2 :: (?logger::LoggerEnv) => IO ()
internal2 = do
  logErr ?logger "Alert!"
