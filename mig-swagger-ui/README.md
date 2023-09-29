# mig-swagger-ui

The package defines utils to add swagger UI for mig library.
To add swagger just use function `withSwagger`:

```haskell
main = runServer port (withSwagger def server)
```

With `SwaggerConfig` we can update defaults.
To set information on app we can use `mapSchema` field of the swagger config.

There is type `DefaultInfo` to set common fields:

```haskell
main = runServer port (withSwagger swaggerConfig server)
  where
    swaggerConfig =
      def
        { swaggerFile = "swagger.json"
        , staticDir = "swagger-ui"
        , mapSchema = pure . addDefaultInfo info
        }

    info =
      DefaultInfo
        { title = "Hello world app"
        , description = "Demo application"
        , version = "1.0"
        }
```

Also see package `openapi3` on how to update OpenApi schema.

## Aknowledgments 

Thanks to Oleg Grenrus for providing [`servant-swagger-ui`](https://hackage.haskell.org/package/servant-swagger-ui) library
on which this package is based on. 
Thanks to David Johnson, Nickolay Kudasov, Maxim Koltsov authors of [`openapi3`](https://hackage.haskell.org/package/openapi3)
and [`servant-openapi3`](https://hackage.haskell.org/package/servant-openapi3) libraries. The mapping from Mig's Api to OpenApi 
is based on those packages.
