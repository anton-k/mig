# How to use Swagger

The Swagger is a powerful tool to try out your servers.
It provides easy to use Web UI to call routes in the server.
We already saw know how to augment server with swagger.
It is just a line of code:

```haskell
withSwagger def server
```

The function `withSwagger` is defined in the package `mig-swagger-ui`
which is re-exported by `mig-server`.


In this chapter we are going to learn how to tweak and fine-tune the swagger.

## Swagger config

We have used default swagger config with constant `def`.
Let's look at what can be configured:

```haskell
data SwaggerConfig m = SwaggerConfig
  { staticDir :: Path
  -- ^ path to server swagger (default is "/swagger-ui")
  , swaggerFile :: Path
  -- ^ swagger file name (default is "swaggger.json")
  , mapSchema :: OpenApi -> m OpenApi
  -- ^ apply transformation to OpenApi schema on serving OpenApi schema.
  -- it is useful to add additional info or set current date in the examples
  -- or apply any real-time transformation.
  }

instance (Applicative m) => Default (SwaggerConfig m) where
  def =
    SwaggerConfig
      { staticDir = "swagger-ui"
      , swaggerFile = "swagger.json"
      , mapSchema = pure
      }
```

We can set in `staticDir` at what path to serve the swagger,
how to name the swagger file with `swaggerFile` and
which run-time transformation we apply to the OpenApi schema.
We can use this mapping to add useful description to the app
or keep examples up to date if they use for example timestamps
that should be in the future to be valid.

## Add description to application

Often we would like to provide short description of the application,
which version it has. We can use the package `openapi3` to update OpenApi 
directly. But also there is a helper type for most often used fields:

```haskell
-- | Default info that is often added to OpenApi schema
data DefaultInfo = DefaultInfo
  { title :: Text
  , description :: Text
  , version :: Text
  }

addDefaultInfo :: DefaultInfo -> OpenApi -> OpenApi
```

We can set title, description and version for the application.
Here is an example:

```haskell
setSwagger :: Server IO -> Server IO
setSwagger = withSwagger config
  where
    config =
      (def :: SwaggerConfig IO)
        { mapSchema = pure . addDefaultInfo info
        }

    info =
      def
        { title = "Weather forecast"
        , description =
            "JSON API example for mig library which shows how to forecast weather to authorized users"
        , version = "0.1.0"
        }
```

## Describe the routes

Often we would like to add some useful documentation on the routes.
We can do it with the functions:

```haskell
-- | Sets description of the route
setDescription :: Text -> Server m -> Server m

-- | Sets summary of the route
setSummary :: Text -> Server m -> Server m

-- | Adds OpenApi tag to the route
addTag :: Text -> Server m -> Server m
```

We can apply those functions at definition of the route.
Also we can describe the inputs for the route:

```haskell
{-| Appends descriptiton for the inputs. It passes pairs for @(input-name, input-description)@.
special name request-body is dedicated to request body input
nd raw-input is dedicated to raw input
-}
describeInputs :: [(Text, Text)] -> Server m -> Server m
```

It takes a map from input parameter name to description. There is special name `"request-body"`
for the request body input.

An example:

```haskell
server = "calculator" /.
  mconcat
    [ describeAdd $ "add" /. add
    , describeMul $ "mul" /. mull
    ]
  where
    describeAdd = setDescription "Performs addition" . describeArgs
    describeMul = setDescription "Performs multiplication" . describeArgs

    describeArgs = describeInputs [("a", "first argument"), ("b", "second argument")]

add, mul :: Capture "a" Int -> Capture "b" Int -> Get (Resp Int)
```

## Summary

In this chapter we have learned how to tweak our swagger servers and make them more user-friendly.
