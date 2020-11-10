# co-log-json

`co-log-json` allows to write structured logs in your application.

The library allows to add additional machine readable context to the log
messages. I.e. each log message is a json object with a predefined structure
where additional user context can be added. Such logs can be easily indexed
by external logs systems such as [graylog](https://www.graylog.org) or 
[Elastic search](https://www.elastic.co). The message logging is done using
a special context, that keeps information about additional attributes.
Such contexts forms a nested scope, and all messages in the score inherit provided
context:

```
   scope [ user = "Joe"] 
     |
     | ==>  {"message":"user arrived", "data": { "user": "Joe"}
     |
     +------ scope [ tag = "human" ]
              |
              | ==> {"message": "user entered the door", "data": { "user": "Joe", "tag": "human"}
```


In other words this library takes two choices:

  1. We write full context with each message.
  This approach allows to use external tools (like elastic-search) that can index message
  without any additional work and information.
  Any message can be lost without affecting ability to decode other messages. Alternative
  approach could be emitting a list of events (possibly as a bytecode), such approach
  is taken by the tracing libraries. It requires more works during reading and indexing
  logs, and in case in some logs are logs may render later ones unusable or lose some info.

  2. We keep a state context so we don't need to attach context messages to each one.
  Alternative approach is extraction of the structure information from the message itself.
  This approach is taken in some structured logging libraries, it can provide better error
  messages. However it requires to think about the message context all the time (and developer 
  user may not even know all interesting context where the message will be used).


# Using a library

In order to use a library you'll need to add `co-log-json` to the dependency list.

```
library.cabal

library
  build-depends:
    base,
    co-log-json ^>= 0.0
```


## Setting the library


```haskell
import Colog.Json                       -- Core API for structured logging
import Colog.Json.Action (logToHandle)  -- Actions to store the message
import System.IO (stderr)

main :: IO ()
main = do
  -- First we need to setup log storing function:
  --   In order to emit logs we need to create a context.
  --    Context takes the action to store log and it's possible to
  --    attach additional information to it.

  let context = mkLogger (logToHandle stderr)
  --                          ^
  --                          |
  --                          +------  `LogAction IO Message` 
  --                                    (see discussion "Why IO below")
  --
  -- Once 'context' is created it can be passed to the other parts
  -- of the code or used to emit logs.

  logDebug context "Hi, there!"
  --         ^         ^
  --         |         |
  --         |         +----- `LogStr` type - is an efficient text builder.
  --         |                 
  --         |
  --         +-- log context

  -- Will output:
  --
  -- ```
  -- {"severity":"DEBUG", "thread": 1, "message": "Hi, there!"}
  -- ```
  --                ^               ^                ^
  --                |               |                +------------ Message itself
  --                |               +----------------------------- Id of the thread that emitted the message
  --                +----------------- Severity level: Debug, Info, Notice, Error, Critical, Alert, Emergency
  -- 
  -- to the stderr.

  -- There are other helper functions list `logInfo`, `logNotice`, etc.
```

Now let's discuss `LogStr` type. It exists in order to efficiently generate message without
extra allocations that can be avoided. Basically it's just a [Text.Builder](http://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text-Lazy-Builder.html) though iternal
representation may evolve in the future. `LogStr` allows message concatenation without allocating
intermediate structures, so it allows efficient log building. For example line:

```haskell
  logDebug context $ "a" <> "b" <> "c"
```

will write all text string "a", "b", "c" directy to the buffer, and can actually do more optimizations.
So it's quite safe to use the librart even with a large amount of logs.

The 'LogStr' type implement [IsString](http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-String.html#t:IsString) instance it means that you can just write string literals
and they will be treated as 'LogStr'. To concatenate two 'LogStr' you can use [`<>`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Semigroup.html#t:Semigroup) operation.

In addition you can convert any string lines that has [StringConv a T.Text](https://hackage.haskell.org/package/string-conv-0.1.2/docs/Data-String-Conv.html), i.e. can be converted
to LogStr using `ls :: StringConv a T.Text => a -> LogStr`.
Efficiency and safety of this convertion depends on the concrete instance implementation and is out
of control of the package.

```haskell
   logDebug context $ "foo is " <> ls ("ы":: ByteString)
```

In case your data structure does not implement `StringConv a` it's possible to use not efficient but
very general `showLS :: Show a => a -> LogStr` method that will work for any type that has Show instance.

## Adding context.

But just writing json messages is not very interesting, we want to control the context of the message.
In order to add user data to the context we can use `addContext :: PushContext -> LoggerEnv -> LoggerEnv`
method.

```haskell

   let context1 = addContext (sl "user_id" (1::Int)) context
   --    ^                    ^     ^         ^        ^
   --    |                    |     |         |        +--- old context
   --    |                    |     |         +------------ user data 
   --    |                    |     +---------------------- key for the user data
   --    |                    +---------------------------- function that generates an entry
   --    +------------------------------------------------- new context that has data attached

   logDebug context1 "Hi again!"

   -- Will emit to the stderr:
   --
   -- ```
   -- {"severity":"DEBUG", "thread": 1, "message": "Hi, there!", "data": {"user_id":1}}
   -- ```
   --                                                              ^           ^    ^
   --                                                              |           |    +-- value
   --                                                              |           +------- key
   --                                                              +--- all user data is kept under 'data' key
```

Sidenote: 'sl' function looks redundant, but it's needed in order to provide a future
compatibility for a case when internal structure of user data is changing. Historically
it came from [katip](https://hackage.haskell.org/package/katip) library, there are more functions that allows context creation but

Function `sl :: ToJSON a => T.Text -> a -> PushContext` prepares update to the context. 
It encodes user data to JSON. This is why we had to add type annotation to `1` otherwise GHC had
no means to infer the type).

It's important that library does not perform any compacation of key-values, i.e.:

```
   logDebug (addContext (sl "user_id" 2) context1) "Who am I?"
   -- Will emit:
   -- ```
   -- {"severity":"DEBUG", "thread": 1, "message": "Hi, there!", "data": {"user_id":1, "user_id": 2}}
   -- ```
```

This is done for purpose, but may be a subject to change in the future major versions.

`addContext` is not the only function that allows to modify context, there is the other one
`addNamespace :: Text -> LoggerEnv -> LoggerEnv`, this function adds an entry about the 
current namespace. Namespace is a nested list of strings that tells the logical part of 
the codebase the log belongs to. It's kept separate fom the data, and it's possible to
use in library filtering (using  [cfilter](https://kowainik.github.io/posts/2018-09-25-co-log#cfilter)) or filtering in external system.


The described functionality is enough to perform logging. However it may worth discussing 
interporability with the rest of co-log ecosystem and ergonomics.

## co-log interporability

co-log ecosystem works with "LogAction m a" and in the package we use "LogEnv" it means
that we are losing most of the benefits of the library and can't use a lot of utility
functions. In order to improve the situation it's possible to convert LogEnv into
`LogAction m (Severity,LogStr)` using function `unLogger`. Then we will have log action
that will emit a message with the current context, but context will no longer be modifiable.

# Ergonomics

The package provides no ergonomic tools by default and it's important that `addContext` is a pure
function. On the one hand it introduces a log of boilerplate code but on the other.

It allows to modify context even outside of the effectful computation for example with servant you may have:

```
-- | Catalogue of items 
data CatalogueApi route = CatalogueApi
  { _the_catalogue :: route :- Capture "catalogue_id" CatalogueId :> ToServantAPI Bar
  , ...
  } 

-- Item
data ItemApi route = ItemApi
  { _items :: route :- "items" :> Get '[JSON] (Vector Item)
  , ...
  }

handleCatalogue :: LoggerEnv -> ToServant CatalogueApi AsServer
handleCatalogue ctx' = CatalogueApi
  { _the_catalogue = \catalogue_id ->
     handleItem (addContext "catalogue_id" catalogue_id) ctx) catalogue_id
  , ...
  }
  where 
    ctx = addNamespace "catalogue" ctx'

handleItem :: LoggerEnv -> CatalogueId -> ToServant ItemApi AsServer
handleItem ctx' catalogue_id = ItemApi ...
  where ctx = addNamespace "item" ctx'
```

So we can build computation that we will execute later while modifying the context.

On the other hand it we can always wrap this pure function into an effect system 
of choice be it either handle pattern or readert or mtl or free-monad or effect system.
This library does not target any particular solution and do not want to introduce 
neither additional dependencies nor additional restrictions on the user.
