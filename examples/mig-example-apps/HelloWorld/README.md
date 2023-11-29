# Hello wrold server

An example of the most basic JSON API server
that has two routes. It can greet the user and say good bye.

We can use `curl` to test it.
Run it with:

```
> stack run hello-world-mig-example-app	
```

After server start we can test it with [Swagger UI](http://localhost:8085/swagger-ui/index.html).
Or with curl:

Curl for hello route
```
curl -X 'GET' \
  'http://localhost:8085/api/v1/hello' \
  -H 'accept: application/json'
```

Curl for bye route

```
curl -X 'GET' \
  'http://localhost:8085/api/v1/bye?user=alice' \
  -H 'accept: application/json'
```  
