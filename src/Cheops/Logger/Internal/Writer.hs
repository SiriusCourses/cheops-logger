-- |
-- Internal function that allows to dump the log message.
module Cheops.Logger.Internal.Writer
  ( feedHandle
  ) where

import Cheops.Logger.Internal.Structured
import Colog.Core
import Control.Exception
import Data.Aeson.Encoding as Aeson
import Data.ByteString.Builder qualified as Builder
import Data.Coerce
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import System.IO

-- | Low level function to that dumps message in the given handle. This function
-- is intended to be used as a logging action with @co-log@ library. It embedds all
-- functionality in it, on the contrary to the composable co-log nature.
--
-- This function serializes a message to a temporary buffer and dumps buffer contents
-- to the handle as soon as it's filled.
-- 
-- Message structure:
-- 
-- @
-- { "namespace": "segment1.segment2"
-- , "severity":  "DEBUG"
-- , "thread":    123121
-- , "data":  { key: attributes }
-- , "message":  message
-- }
-- @
-- 
-- In case if there are no user attributes "data" key is omitted.
--
-- Notes:
--
--   1. In case of exception this function tries to dump information about exception
--      in the handle once. But if another exception arrives function rethorws second
--      exception.
--
--   2. During log dumping this function captures all exceptions (@SomeException@) including
--      asynchronous ones. So it should be run under a mask. But it's ok as this function
--      it used with co-log-concurrent that runs it under a mask.
--
--   3. This function does not add timestamp to the message. This is done because
--      we can always rely on external tools to add timestamp, whether it would be @ts@
--      or @journald@ or docker-logger.
--      However if timestamp from the program is strictly needed it's possible to add
--      it to the user data.
--
--   4. If user data contains multiple values with the equal keys all the values will
--      be stored. In case if it's unbearable you should remove duplicates when generating
--      a @Message@
--
--   5. While dumping the message to the @Handle@ no lock is obtained, so the user
--      is responsible for running this function in a race free context. It can be done
--      either from a single thread, or using additional locking mechanisms.
--
--   6. This function relies on the line bufferring in order to dump logs promptly. It's
--      up to the user to decide how to setup it but in case of block buffering during
--      low activity logs may be delayed a lot.
--
feedHandle :: Handle -> LogAction IO Message
feedHandle h = LogAction \Message{..} ->
  let msg = Aeson.pairs $ mconcat
       [ case namespace of
           Nothing -> mempty
           Just xs -> Aeson.pair "namespace" $ Aeson.lazyText xs
       , Aeson.pair "severity" $ commonSeverity message_severity
       , Aeson.pair "thread" $ Aeson.int thread_id
       , case user_data of
           Nothing -> mempty
           Just xs -> Aeson.pair "data" $ Aeson.pairs xs
       , Aeson.pair "message" $ lazyText $ coerce TLB.toLazyText message
       ]
      namespace = TL.intercalate ".". toList <$> NE.nonEmpty
         [ TL.fromStrict tm
         | Segment tm <- toList attributes
         ]
      user_data = fold <$> NE.nonEmpty
        [ pair key attributeValue
        | Attr key attributeValue <- toList attributes
        ]
  in try (Builder.hPutBuilder h $ Aeson.fromEncoding msg <> Builder.char7 '\n') >>= \case
       Left e -> do
         Builder.hPutBuilder h $ Aeson.fromEncoding (Aeson.pairs
            $ mconcat
            [ Aeson.pair "namespace" (Aeson.text "logger")
            , Aeson.pair "exception" (Aeson.string (show (e::SomeException)))
            ])
            <> Builder.char7 '\n'
       Right _ -> pure ()

