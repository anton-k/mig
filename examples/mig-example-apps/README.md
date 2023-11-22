# mig-example-apps

Examples for the library mig.
We can find out how to build various servers:

* `HelloWorld` - basic single route server

* `RouteArgs` - example of various input and output newtype-wrappers to define handler functions for the server

* `Counter` - example on how to use custom monad based on Reader-pattern

* `JsonApi` - weather forecast JSON API

* `Html` - simple blog post site that servers HTML.

* `HtmlTemplate` - variation of `Html` example with safe URLs and HTML-templates

Also we can build clients:

* `HelloClient` - basic hello world client

* `RouteArgsClient` - client with all sorts of inputs

* `CounterClient` - how to build client and server from the same code

See `Makefile` for commands to build examples and run them.

