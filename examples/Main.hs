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
      logDebug ?logger "Hello!"
      logInfo ?logger "World!"
      let ?logger = ?logger
                  & addContext (sl "host" ("127.0.0.1"::String))
                  & addNamespace "www"
      logErr ?logger "Alert!"
  where
    config = LoggerConfig defCapacity Nothing
