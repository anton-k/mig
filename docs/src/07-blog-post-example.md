# HTML example: Blog site

We have seen many examples of JSON applications. But we can send data in other formats too.
Most common forma t for human-readable information for HTTP applications
is HTML. Any site that we visit sends HTML pages to us. Let's create a simple
site that shows blog posts and quotes and we can add new blog posts to it.
Complete code for example is in the [`mig` repo](https://github.com/anton-k/mig/tree/main/examples/mig-example-apps/Html)
We will cover the most interesting parts of the application.

As in previous example we are going to use IO-based server. 
The common actions that change internal state of the site or query useful data
are expressed as collection of interfaces. All handlers accept interfaces as first argument.

The structure of the application is similar to the weather forecast example.
The modules with their purpose:

* `Types` - types that cover domain of our application
* `Server` - defines server API and handlers
* `Main` - runs the server
* `View` - renders types as HTML-pages
* `Interface` - actions that can be performed on shared internal state of the app
* `Init` - initialization of the interfaces
* `Content` - some mock data to show on page
* `Internal` - internal state of the site

## Using preset module for HTLM servers

As we define server based on `IO` and `HTML` we will use the module `Mig.Html.IO`.
Note that there is a difference in types from `JSON` brother of that module.
We will not see the `Resp` type in the signatures of the handlers.

Because for `HTML` the type synonyms to `Send` fix the output of `Send`-type
so that it always has value wrapped in `Resp`:

```haskell
type Get a = Send GET IO (Resp a)
```

The `Resp`-type is built in the type because for HTML we almost never need the `RespOr`
with different type of errors. Our errors are going to be HTML-pages to as the result.
So we report errors to user in HTML-pages.

## Run a server

Let's define a function that will start an empty server:

```haskell
module Main (
  main,
) where

import Mig.Html.IO (runServer)

main :: IO ()
main = do
  site <- initSite
  runServer port (server site)
  where
    port = 8085

-- | Placeholder for collection of interfaces
data Site = Site

-- | Initialize site's interfaces
initSite :: IO Site
initSite = pure Site

-- | Create a server that works in terms of interfaces
server :: Site -> Server IO
server _ = mempty
```

## Define server routes

Let's define the server and its routes.
For our blog post site we are going to show the pages:

* main page with list of blog posts and greeting

* show random blog post

* show random quote

* save new blog post

* list all blog posts

Also our site serves static files which contain CSS-style file and images
We keep them in separate directory called `resources`.

Here is the definition of the server:

```haskell
-- | Server definition. Note how we assemble it from parts with monoid method mconcat.
server :: Site -> Server IO
server site =
  logRoutes $
    mconcat
      [ "blog"
          /. [ readServer
             , writeServer
             ]
      , defaultPage
      , addFavicon $ "static" /. staticFiles resourceFiles
      ]
```

The site serves several sub-servers:

* `readServer` - pages that fetch content for us
* `writeServer` - pages to update the content. To save new blog post
* `defaultPage` - main page of the app
* `staticFiles` with `addFavicon` - 
      serves static files and adds icon for our site that is shown in the tab of the browser

### default page server

Let's define the simplest server, the default page:

```haskell
-- default main page
defaultPage =
  mconcat
    [ "/" /. handleGreeting site
    , "index.html" /. handleGreeting site
    ]

handleGreeting :: Site -> Get (Page Greeting)
```
It links two routes to the `handleGreeting` handler. We serve two routs: the route
of the site and default route for main page "/index.html".

For now we can think of `Page` as main template of our site which contains
some value that is renderable to HTML. Renderer converts the value to HTML
and injects it to the site's template.

### Read server

Let's define read-only pages for our site.

```haskell
-- server to read info.
-- We can read blog posts and quotes.
readServer =
  mconcat
    [ "read"
        /. [ "post" /. handleBlogPost site
           , "quote" /. handleQuote site
           ]
    , "list" /. handleListPosts site
    ]

handleBlogPost :: Site -> Optional "id" BlogPostId -> Get (Page BlogPost)

handleQuote :: Site -> Get (Page Quote)

handleListPosts :: Site -> Get (Page ListPosts)
```

We have three pages to read in our site:

* `handleBlogPost` - shows random blog post
* `handleQuote` - shows random quote
* `handleListPosts` - shows a list of all posts

### Write server

Let's define a route to add new blog posts to the site:

```haskell
    -- server to write new blog posts
    writeServer =
      "write"
        /. [ toServer $ handleWriteForm site
           , toServer $ handleWriteSubmit site
           ]

handleWriteForm :: Site -> Get (Page WritePost)

handleWriteSubmit :: Site -> Body FormUrlEncoded SubmitBlogPost -> Post (Page BlogPost)
```

We have two different routes which are served on the same path. 
The first route shows the page with form to fill the post data.
The second route is triggered when we hit the submit button.
The data in the form is send to the site as request body with `Content-Type`
set to corresponding `FormUrlEncoded` type.

We can serve several route handlers on the same path if they differ by
HTTP-method, input type or output type. In this example we use `toServer` function
to convert route handler function to `Server IO`.

As we can see all handlers expect the value `Site` as first argument.
This value contains interfaces for all actions that can be performed with our site.

### Static files

We serve static files with the line:

```haskell
  , addFavicon $ "static" /. staticFiles resourceFiles
```

Let's explain what happens here. The standard function `staticFiles`
takes a collection of files and turns them into server:

```haskell
staticFiels :: MonadIO m => [(FilePath, ByteString)] -> Server m
```

The list of pairs contains pair of `(path-to-file, byte-string-content-of-the-file)`.
It is convenient to use it with function `embedRecursiveDir`  which
embeds all files from directory to Haskell executable. It comes with library `file-embed-lzma`.
We use it like this:

```haskell
server = "static" /. staticFiles resourceFiles

resourceFiles :: [(FilePath, ByteString)]
resourceFiles = $(embedRecursiveDir "Html/resources")
```

For our site we use `milligram.css` framework for styling and couple of
pictures.

Also let's discuss `addFavicon` function:

```haskell
addFavicon = addPathLink "favicon.ico" "static/lambda-logo.png"
```

It shows interesting concept of linking to the parts of the server.
The standard function `addPathLink` serves all calls to `favicon.ico`
with handler on path `"static/lambda-logo.png"`.
This way we can create additional links to the server.

Also we can reuse the sub-parts of another server in our server with
function `atPath`:

```haskell
{-| Sub-server for a server on given path
it might be usefule to emulate links from one route to another within the server
or reuse part of the server inside another server.
-}
atPath :: forall m. Api.Path -> Server m -> Server m
```

It creates sub-server for all routes that match given path-prefix.
So it can be not only single route but the whole sub-server.
The function `addPathLink` rides on top of that useful function.

### Overview of the site

So for our site we have 5 handlers. Let's recall them:

```haskell
handleBlogPost :: Site -> Optional "id" BlogPostId -> Get (Page BlogPost)

handleQuote :: Site -> Get (Page Quote)

handleListPosts :: Site -> Get (Page ListPosts)

handleWriteForm :: Site -> Get (Page WritePost)

handleWriteSubmit :: Site -> Body FormUrlEncoded SubmitBlogPost -> Post (Page BlogPost)
```

We can see from type signatures all the types for our domain.
Let's define the types.

## Domain model of the blog site

We are going to show Blog posts. And we have a form to input a new content to the site.
We can define the types for domain as follows:

```haskell
-- | Web-page for our site
newtype Page a = Page a

-- | Greeting page
data Greeting = Greeting [BlogPost]

-- | Form to submit new post
data WritePost = WritePost

-- | List all posts
newtype ListPosts = ListPosts [BlogPost]

-- | Blog post id
newtype BlogPostId = BlogPostId {unBlogPostId :: UUID}
  deriving newtype (FromHttpApiData, ToHttpApiData, Eq, Show, FromJSON, ToParamSchema)

data BlogPostView
  = ViewBlogPost BlogPost
    -- | error: post not found by id
  | PostNotFound BlogPostId

-- | Blog post
data BlogPost = BlogPost
  { id :: BlogPostId
  , title :: Text
  , createdAt :: UTCTime
  , content :: Text
  }

-- | A quote
data Quote = Quote
  { content :: Text
  }

-- | Data to submit new blog post
data SubmitBlogPost = SubmitBlogPost
  { title :: Text
  , content :: Text
  }
  deriving (Generic, FromForm, ToSchema)
```

We need to define certain instances to be able to send the data over HTTP wires.
The types come from standard libraries for web development in Haskell: `openapi3`, `http-api-data`, `aeson`.

## Interfaces for the site

Our web site is going to support the following actions:

```haskell
{-| Web site actions. It defines interfaces that connect logic of our site
with outside world: DBs, logger.
-}
data Site = Site
  { readBlogPost :: BlogPostId -> IO (Maybe BlogPost)
  , writeBlogPost :: SubmitBlogPost -> IO BlogPostId
  , listBlogPosts :: IO [BlogPost]
  , readQuote :: IO Quote
  , logInfo :: Text -> IO ()
  , cleanup :: IO ()
  }
```

The actions are natural and follow the design of domain and server.

## Implement handlers

Let's implement the handlers in terms of interfaces.

### Default page

Let's greet the user:

```haskell
-- | Greet the user on main page
handleGreeting :: Site -> Get (Page Greeting)
handleGreeting site =
  Send $ ok . Page . Greeting <$> site.listBlogPosts
```

We get the list of all blog posts and wrap them in `Greeting` page.

### Read site

Let's read the blog post:

```haskell
-- | Read blog post by id
handleBlogPost :: Site -> Optional "id" BlogPostId -> Get (Page BlogPostView)
handleBlogPost site (Optional mBlogId) = Send $
  case mBlogId of
    Nothing -> ok . Page . ViewBlogPost <$> randomBlogPost site
    Just blogId ->
      maybe
        (bad notFound404 $ Page $ PostNotFound blogId)
        (ok . Page . ViewBlogPost)
      <$> site.readBlogPost blogId
```

We have an optional query parameter that contains `id` of the blog post.
If `id` is missing we return some random blog post.
If there is no blog post that corresponds to id we return error with page not found status.

Let's read the random quote:

```haskell
handleQuote :: Site -> Get (Page Quote)
handleQuote site = Send $ ok . Page <$> site.readQuote
```

Let's show all blog posts as a menu to the user:

```haskell
handleListPosts :: Site -> Get (Page ListPosts)
handleListPosts site = Send $ do
  ok . Page . ListPosts <$> site.listBlogPosts
```

### Write site

Let's show the form to the user to fill new post data:

```haskell
handleWriteForm :: Site -> Get (Page WritePost)
handleWriteForm _site =
  Send $ pure $ ok $ Page WritePost
```

As we can see we just return the tag of the page that encodes
the content with form. We will define the HTML-form in the rendering module `View.hs`.

Let's define the logic to save the submitted data to application:

```haskell

-- | Submit form with data provided by the user
handleWriteSubmit :: Site -> Body FormUrlEncoded SubmitBlogPost -> Post (Page BlogPostView)
handleWriteSubmit site (Body submitData) = Send $ do
  pid <- site.writeBlogPost submitData
  maybe
    (bad notFound404 $ Page $ PostNotFound pid)
    (ok . Page . ViewBlogPost)
    <$> site.readBlogPost pid
```

In this example we save the new blog post and read that post after saving to show 
it to the user.

As we can see the handlers follow the interfaces of the site. We just
wrap data in the content.

## View HTML pages

To render HTML pages we use `blaze-html` library. All we need to do is to define
instances for all types that show up in the result of handlers.

The type `Page` is a container for our web site main template:

```haskell
-- writes the template for main page
instance (ToMarkup a) => ToMarkup (Page a) where
  toMarkup (Page page) = siteTemplate (H.toMarkup page)

siteTemplate :: Html -> Html
```

Also we define `ToMarkup` instances for all elements in the site:

```haskell
instance ToMarkup Greeting where
instance ToMarkup WritePost where
instance ToMarkup BlogPostView where
...
```

You can find the full code in the [sources](https://github.com/anton-k/mig/tree/main/examples/mig-example-apps/Html).

## Internal state implementation

Our website server is ready to be launched.
We only need to define the interfaces. You can find the implementation
in the source code for [the example](https://github.com/anton-k/mig/tree/main/examples/mig-example-apps/Html). See the modules: 

* `Init` - initializes the interface
* `Content` - contains run-time mock data
* `Internal.State` - mutable state to save blog posts in memory

## Summary

We have learned how to build HTML-based servers. It goes almost the same as with JSON applications
only we have some twists regarding static files.
