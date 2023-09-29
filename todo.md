# TODO

Features, improvements and open problems for releases:

## v2.1

### Major

* update / add docs

* go over Servant API and servant-openapi3 and collect cases 
   which one to implement and update API  
   for example maybe we don't need separate cases for BodyJsonInput or FormInput
   they are both just RequestBody with different media-types

* check that all examples work
  * html example:
    * write post produces nothing (bug)
    * use UUID as post ids

### Normal

* fill missing captures at the end

* update external handler and reader apps examples

## v2.2

add client support

* add Accept and Content-Type headers for client

* test client 

* generate client for JsonExample

* add client examples 
  * reuse JsonApi and hello-world
  * client from scratch (without defined server) to test client-only apps

## v2.3

* RIO support (separate package mig-rio)

## v3

* add support for (Either Error (Response a))
* add capture all case
* add XML support
* add docs (build separate site on github pages)

## v4

* generate Mig client and server from open-api yaml file. Useful for spec first approach

### ideas

* inport client from swagger

   That might be super-useful for sketching API's we can download
   swagger open-api file from API web-page trim it to needed methods
   and provide type signatures for the client

   it will produce run time error if it does not work

   maybe we can generate clients as the next step from open api


