# co-log-json

`co-log-json` allows to write structured logs in your application.

With this library structured means that each log message can contain additional
context that is structured and can be easily parsed by the external system.

That context forms a nested scope, and all messages in the score inherit provided
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

  1. We write full context with each message. This approach allows to use external
  tools (like elastic-search) that can decode message without any additional work.
  Any message can be lost without affecting ability to decode other messages. Another
  choice there could be emitting a list of events (possibly as a bytecode), such approach
  is taken by the tracing libraries. But this approach require more works during storing
  (or reading) logs, and in case in some logs are logs may render later ones unusable or
  lossing required info.

  2. We keep a state context so we don't need to attach context messages to each one.
  The contrary approach is extraction of the structure information from the message itself.
  In that approach all interesting data should present in each message.

# Using a library

TODO

