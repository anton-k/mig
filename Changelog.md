# Changelog

## v2.1

Adds some HTML goodies:

* cookies (new input `Cookie` and function to set the cookies)
* type safe stable URLs from server definition
* example `HtmlTemplate` on how to use template engine with library

Adds stack template to create new hello-world server with stack.

## v2

The servers are augmented with info on API structure. It allows us to 
build OpenApi schema, swagger and clients for servers.

* OpenApi schema for servers

* swagger servers support 

* clients from the same code as servers

* redesign of internal types

* redesign of DSL for routes

* many ergonomic improvements

* packages for extra utils

* split of `mig` package to several packages:
    
    * `mig` - core
    * `mig-wai` - rendering servers to wai apps
    * `mig-client` - clients
    * `mig-extra` - extra utils
    * `mig-server` - mig servers with batteries
    * `mig-swagger-ui` - swagger ui server

* tutorial and quickstart guide on github pages

* CI for repo with formatter, build and tests and update of docs on github pages
  
Thanks to Ambros for contribution. 

## v1

Initial version. Servers as functions with type-safe dsl.

