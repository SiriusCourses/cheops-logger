-- |
-- Action that allows to write structured log message.
-- 
module Colog.Json.Action
  ( logToHandle
  , encodeMessage
  ) where

import Colog.Core
import Colog.Json.Internal.Structured
import Control.Exception
import Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.ByteString.Builder as Builder
import Data.Coerce
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import System.IO

-- | Dump logs to the output. On the contrary to the usual co-log functions this one
-- embeds all the functionality inside. However all internals are exposed and in the
-- case if you need any special functionality you can build your own function.
--
-- This function serializes a message to a temporary buffer and dumps buffer contents
-- to the handle as soon as it's filled.  See 'encodeMessage' for encoding details.
--
-- Notes:
--
--   1. In case of exception this function tries to dump information about exception
--      in the handle once. But if another exception arrives while storing info, 
--      function rethorws the second exception.
--
--   2. During log dumping this function captures all exceptions (@SomeException@) including
--      asynchronous ones. So it should be run under a mask.
--
--   3. Running a function under interruptible mask is safe as long no other thread 
--      is using a @Handle@.
--
--   4. This function does not add timestamp to the message. This is done because
--      we can always rely on external tools to add timestamp, whether it would be @ts@
--      or @journald@ or docker-logger.
--      However if timestamp from the program is strictly needed it's possible to add
--      it to the user data.
--
--   5. If user data contains multiple values with the equal keys all the values will
--      be stored. In case if it's unbearable you should remove duplicates when generating
--      a @Message@
--
--   6. While dumping the message to the @Handle@ no lock is obtained, so the user
--      is responsible for running this function in a race free context. It can be done
--      either from a single thread, or using additional locking mechanisms.
--
--   8. This function relies on the line bufferring in order to dump logs promptly. It's
--      up to the user to decide how to setup it but in case of block buffering during
--      low activity logs may be delayed a lot.
--
logToHandle :: Handle -> LogAction IO Message
logToHandle h = LogAction \m ->
  let msg = encodeMessage m
  in try (Builder.hPutBuilder h $ Aeson.fromEncoding msg <> Builder.char7 '\n') >>= \case
       Left e -> do
         Builder.hPutBuilder h $ Aeson.fromEncoding (Aeson.pairs
            $ mconcat
            [ Aeson.pair "namespace" (Aeson.text "logger")
            , Aeson.pair "exception" (Aeson.string (show (e::SomeException)))
            ])
            <> Builder.char7 '\n'
       Right _ -> pure ()


-- | Efficiently convert a message into JSON encoding.
--
-- Message structure:
-- 
-- @
-- { "namespace": "segment1.segment2"
-- , "severity": "DEBUG"
-- , "thread": 123121
-- , "data":  { ... }
-- , "message": "text message"
-- }
-- @
-- 
-- In case if there are no user attributes "data" key is omitted.
encodeMessage :: Message -> Encoding
{-# INLINE encodeMessage #-}
encodeMessage Message{..} = Aeson.pairs $ mconcat fields where
  fields =
     [ case namespace of
         Nothing -> mempty
         Just xs -> Aeson.pair "namespace" $ Aeson.lazyText xs
     , Aeson.pair "severity" $ encodeSeverity message_severity
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
    [ pair (Aeson.Key.fromText key) attributeValue
    | Attr key attributeValue <- toList attributes
    ]


