# TODO

Features, improvements and open problems for releases:

## random

* TH for deriving boilerplate
   * deriveParam ''Type
   * deriveBody ''Type

### Normal

-- after commit to main repo
* update external handler and reader apps examples

## v2.2

add client support

* add Accept and Content-Type headers for client

* test client 

* generate client for JsonExample

* add client examples 
  * reuse JsonApi and hello-world
  * client from scratch (without defined server) to test client-only apps

## v3

* add support for (Either Error (Response a))
* add capture all case
* add Queries case
* add XML support
* add docs (build separate site on github pages)
* support for LRU cache for routes (research on efficient route/api representation)

## v4

* generate Mig client and server from open-api yaml file. Useful for spec first approach

## v5

* Streaming

### ideas

* import client from swagger

   That might be super-useful for sketching API's we can download
   swagger open-api file from API web-page trim it to needed methods
   and provide type signatures for the client

   it will produce run time error if it does not work

   maybe we can generate clients as the next step from open api
