# TODO

Next release due: start of November
Features, improvements and open problems for releases:

### Curent: v2

* add TH for deriving of all classes by type: `IsParam`, `IsBody`, `IsParaBody`, `IsReaderServer`
   * deriveParam ''Type
   * deriveBody ''Type
* try out Response as generic type a (without Resp) for special case of Json servers (use overlappable)
* support for LRU cache for routes (research on efficient route/api representation)

## v2.2

add client support

* write docs page on clients

* add client examples
  * generate client for JsonExample
  * make example for client and server from the same definition: use Counter as example
  * reuse JsonApi and hello-world
  * client from scratch (without defined server) to test client-only apps

* filter Api by path: it can be useful to create client for subset of the big main server:
  ```haskell
  filterPath :: (Path -> Bool) -> Server m -> Server m
  ```
* get all paths of the server. Might be useful for docs generation:
  ```haskell
  getServerPaths :: Server m -> [Path]
  ```
  
## v3

* add capture all case
* add Queries case
* add XML support
* cookbook for main scenarios (add to tutorial)

## v3.2

* make it faster and look out for optimization oportunities

## v4

* HTTP-Streaming
* websockets

## v5

* generate Mig client and server from open-api yaml file. Useful for spec first approach

### ideas

* import client from swagger

   That might be super-useful for sketching API's we can download
   swagger open-api file from API web-page trim it to needed methods
   and provide type signatures for the client

   it will produce run time error if it does not work

   maybe we can generate clients as the next step from open api
