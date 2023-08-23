# TODO

Features, improvements and open problems for releases:

## v2.1

### Major

* update / add docs

* add Accept and Content-Type headers for client

* test client 

* fix swagger on Json example 
   * errors on missing components (use declareSchema and schemaName)
   * [] - empty list schema on list output

* check that all examples work

* html example:
  * write post produces nothing (bug)
  * use UUID as post ids

### Normal

* generate client for JsonExample

* fill missing captures at the end

* add swagger examples

* add client examples 
  * reuse JsonApi and hello-world
  * client from scratch (without defined server) to test client-only apps

* add static files for html example

* update external handler and reader apps examples

## v2.2

* RIO support (separate package mig-rio)

## v3

* add support for (Either Error (Response a))
* add capture all case
* add XML support
* add docs (build separate site on github pages)

## v4

* generate Mig client and server from open-api yaml file. Useful for spec first approach
