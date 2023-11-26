# HTML goodies

In this chapter we will explore several techniques that make
development of the servers that serve HTML a bit more pleasant.

We will study how to:

* use cookies
* create type-safe stable URLs
* use templates to render HTML-code

## Cookies

A cookie is a special HTTP-header that asks the browser to save some information
in local store so that it can be accessed in the sequence of requests.
Often cookies are used to identify the logged in users and keep their sessions
without the need to re-login. 
For great in-depth explanation of the cookies you can read [this article](https://web.archive.org/web/20170122122852/https://www.nczonline.net/blog/2009/05/05/http-cookies-explained).

### How to save a cookie in the local storage

We can ask the browser to set the cookie with a function:

```haskell
setCookie :: (ToForm cookie, IsResp resp) => SetCookie cookie -> resp -> resp
```

We have a function to initialise basic cookies:

```haskell
-- | Create a cookie from content with default settings
defCookie :: a -> SetCookie a
```

The auxiliary parameters let us specify expiration time and other useful 
parameters that control cookie life-cycle (see the type `SetCookie` for details).
Note that the cookie should be an instance of `ToForm` class. It's easy to derive
the instance with generics:

```haskell
data MyCookie = MyCookie
  { token        :: Text
  , secretNumber :: Int
  } deriving (Generic, ToForm, FromForm)
```

We need `FromForm` instance to read the cookie.

### How to read the cookies 

The cookie is just an HTTP-Header with special name.
To fetch the cookie on request we have a special input `newtype`-wrapper:

```haskell
newtype Cookie = Cookie (Maybe a)
```

And we can use it just as any other input (like query parameter or capture) in the
argument list of a handler function:

```haskell
import Data.Text qualified as Text
...

showCookieHandler :: Cookie MyCookie -> Get IO (Resp Html Text)
showCookieHandler (Cookie mValue) = 
  pure $ ok $ case mValue of
    Just value -> cookieToText value
    Nothing -> "No cookie is set"
  where
    cookieToText = 
      Text.unwords 
        [ "The cookie is:"
        , fromString $ show (value.token, value.secretNumber)
        ]
```

Note that value of cookie is optional (wrapped in `Maybe`) as
often on the first visit to page no cookie is set.

## Type-safe stable URLs

In the previous chapter we have discussed HTML-example of prototype for a blog-post site.
And while rendering page data to HTML we typed links to pages as text constants.
For example:

```haskell
    menu = do
      H.div $ do
        H.img H.! HA.src "/static/haskell-logo.png" H.! HA.alt "blog logo" H.! HA.width "100pt" H.! HA.style "margin-bottom: 15pt"
        H.ul H.! HA.style "list-style: none" $ do
          item "/index.html" "main page"
          item "/blog/read/post" "next post"
          item "/blog/read/quote" "next quote"
          item "/blog/write" "write new post"
          item "/blog/list" "list all posts"

    item ref name =
      H.li $ H.a H.! HA.href ref $ H.text name
```

This code is fragile because we can change the name of some path in 
the server definition and forget to update it in the View-functions.

To make it a bit more stable the safe URLs are introduced.
Let's start explanation with most basic type `Url`:

```haskell
-- | Url-template type.
data Url = Url
  { path :: Path
  -- ^ relative path
  , queries :: [(Text, Text)]
  -- ^ queries in the URL
  , captures :: Map Text Text
  -- ^ map of captures
  }
```

It encodes the typical URL. It has static part and two containers
for query and capture parameters.

There is a class `ToUrl` that let us derive proper URL correspondence for
a give server definition.

Let's explain it on hello-world server. It has two routes:

```haskell
-- | The server definition
server :: Server IO
server = 
  "api/v1" /. 
    [ "hello" /. hello
    , "bye" /. bye
    ]

-- | The handler definition as a function
hello :: Get (Resp Text)

-- | The handler with a query parameter to ask for the user name
bye :: Query "user" Text -> Get (Resp Text)
```

This example is for `JSON` server but let's pretend that it is an HTML-server
and we would like to generate `URL`'s from server definition for our handlers.
To do it first we will rewrite the definition a bit. We will place the handlers
to a record and create similar record for URLs. This is not strictly necessary but 
it will make our code more structured. Also we will create type synonyms for handler's
type-signatures:

```haskell
type HelloRoute = Get (Resp Text)
type ByeRoute = Query "user" Text -> Get (Resp Text)

data Routes = Routes
  { hello :: HelloRoute
  , bye :: ByeRoute
  }

-- | The server definition
server :: Server IO
server routes = 
  "api/v1" /. 
    [ "hello" /. routes.hello
    , "bye" /. routes.bye
    ]
```

Let's define the URLs:

```haskell
data Urls = Urls
  { hello :: UrlOf HelloRoute
  , bye :: UrlOf ByeRoute
  }
```

It resembles the handlers code only we use prefix `UrlOf`. This is a type-level
function that knows which URL-creation function corresponds to handler.

For a static route with no arguments it will produce just constant `Url`.
But for a route with arguments the result URL also is going to depend on those arguments
in case that input is either `Query`, `Optional`, `QueryFlag` or `Capture`.
All those inputs affect the look of the resulting URL.

For example for `ByeRoute` we get the type:

```haskell
Query "user" Text -> Url
```

Let's link URLs to the server definition:

```haskell
urls :: Urls
urls = Urls{..}
  where
    hello
      :| bye = toUrl (server undefined)
```

Here we use extension `RecordWildCards` to automatically assign proper fields
by name from the `where` expression. 
Also one new thing is `:|`-operator. It is a suffix synonym for ordinary pair type:

```haskell
data (:|) a b = a :| b
```

It let us bind to as many outputs as we like without the need for parens:

```haskell
  where
    a :| b :| c :| d = toUrl (server undefined)
```

But we should be cautious to use so many routes as there are in the server definition.
`URL`'s are matched against the server definition in the same order as they appear in the
server definition. In this way we get stable names for URL handles.

To use URL in the HTML we can use the function:

```haskell
renderUrl :: (IsString a) => Url -> a
```

Which can convert it to any string-like type. It's compatible with `blaze-html`
and we can use it as an argument for `href` attribute in the html link constructor.

We have to be careful on the order of `URL`'s in the definition and make sure that they match
with the order in the server. If inputs are incompatible a run-time error is produced
on the call to the link.

The great part of it is that query or capture arguments are preserved in the URL constructors.
And the proper corresponding URL text will be generated from arguments.

## How to use HTML-templates

In the previous examples we wrote HTML view code with `blaze-html` DSL. 
The HTML construction is a Haskell function in this style. But often it is
desirable to write HTML with the *holes* in it. So that holes can be substituted
with values at run-time. Those files are called templates. Often templates
are written by Web-designers. 

In this section we will study how to use [`mustache`](https://mustache.github.io/) templates with `mig` library.
The mustache is a very simple and popular templating engine. For Haskell we have 
a great library [`stache`](https://hackage.haskell.org/package/stache) that
makes it easy to use mustache templates in the Haskell.

I recommend to read the [tutorial](https://www.stackbuilders.com/blog/mustache-templates/) on how to use the library.

### Overview of the mustache

The main idea of the templates is very simple. The arguments are marked with
double curly braces:

```
Hello {{name}}!
Nice to meet you in the {{place}}.
```

This template expects a `JSON` input to be completed with two fields:

```js
{
  "name": "John",
  "place": "Garden"
}
```

Also we can render lists of things with special syntax:


```
Items:

{{#items}}
  * [name](ref) 
{{/items}}
```
It expects a `JSON` object:

```js
{
  "items":
     [ { "name": "foo", "ref": "http://foo.com" }
     , { "name": "bar", "ref": "http://bar.com" }
     ]
}
```

As we can see it's format-agnostic and can work for any text.
For HTML there are special marks that let us prevent escaping
of HTML special symbols:

```
{{{content}}}
```

Triple curly braces mean that we do not need escaping of special symbols
and HTML code is trusted and inlined directly.
This type of input is often useful for the template for the main page
where we define header, footer, menus and we have a single place for the main content
of the page which is inlined as plain HTML. Without tripling of the braces w
the HTML code will be rendered as text.


### How to load templates

To load templates we can use the normal functions from the `stache` library.
For this example we will inline them at compile-time in the code:

```haskell
import Text.Mustache
import Text.Mustache.Compile.TH qualified as TH

mainTemplate :: Template
mainTemplate = $(TH.compileMustacheFile "HtmlTemplate/templates/main.html")
```

We need `TemplateHaskell` language extension activated for that.
Also we include the directory with templates in our cabal file as extra-source-files:

```
extra-source-files:
    HtmlTemplate/templates/main.html
    HtmlTemplate/templates/post.html
    HtmlTemplate/templates/postNotFound.html
```

By the way all the code is taken from example in the `mig` repo called 
[`HtmlTemplate`](https://github.com/anton-k/mig/tree/main/examples/mig-example-apps/HtmlTemplate).

After we have loaded the templates we can apply them with `JSON` values.
We get the data from the handler and convert it to HTML with templates.
We will make a helper function for that:

```haskell
import Text.Mustache
import Text.Blaze.Html.Renderer.Text qualified as H
import Text.Blaze.Html5 qualified as H
...

renderMustacheHtml :: (ToJSON a) => Template -> a -> Html
renderMustacheHtml template value =
  H.preEscapedLazyText $ renderMustache template (toJSON value)
```

It applies templates to `JSON`-like values. Let's look at the simple example:

```haskell
-- Rendering of a single quote
instance ToMarkup Quote where
  toMarkup quote = renderMustacheHtml templates.quote quote
```


The template `templates.quote` is applied to value `quote` of the type:

```haskell
-- | A quote
data Quote = Quote
  { content :: Text
  }
  deriving (Generic, ToJSON)
```

Note the deriving of `ToJSON` to make it convertible to `JSON`.
Let's look at the `template.quote`. It is defined in the file `HtmlTemplate/templates/quote.html`:


```html
<div> <h2> Quote of the day: </h2> </div>
<div> <p> {{content}} </p> </div>
```

So it has one argument `content` and exactly that is produced from `Quote` value
as `JSON`.

### Rendering links

Often we would like to render links in the HTML and they have the same structure
with two arguments `href` and `name`. Also we can use stable `URl`'s as links
which we have just studied. For that the helper type was created:

```haskell
data Link = Link
  { href :: Url
  , name :: Text
  }
  deriving (Generic, ToJSON)
```

It's convenient to use it with mustache templates. We can define
template:


```html
<a href="{{href}}">{{name}}</a> 
```

And apply the value of the `Link` type to it.

For example let's look at the template that lists all available blog-posts:

```html
<div> 
  <h2> Posts: </h2>
  <ul>
    {{#posts}}
    <li>
      <a href="{{href}}"> {{name}} </a>
    </li>
    {{/posts}}
  </ul>
</div>
```

It expects a `JSON` object:

```js
{
  "posts":
     [ { "href": "foo", "name": "foo" }
     , { "href": "bar", "name": "bar" }
     ]
}
```

And for that we have a Haskell type that matches this definition:

```haskell
-- | List all posts
newtype ListPosts = ListPosts [BlogPostLink]
```

To render it we only need to add top level object with `"post"`-field.

```haskell
-- | Rendering of all submited posts
instance ToMarkup ListPosts where
  toMarkup (ListPosts posts) = 
    renderMustacheHtml templates.listPosts $ toPostLinks posts

toPostLinks :: [BlogPostLink] -> Json.Value
toPostLinks posts =
  Json.object ["posts" Json..= fmap toLink posts]
  where
    toLink :: BlogPostLink -> Link
    toLink post =
      Link
        { href = urls.blogPost (Optional $ Just post.blogPostId)
        , name = post.title
        }
```  

Note how we use the URL constructor as a function.

### Other template engines

We are not limited with `mustache` for templates. The Haskell has many
great templating libraries which also can be used like [`shakespeare`](https://hackage.haskell.org/package/shakespeare)
or [heist](https://hackage.haskell.org/package/heist) and many others.

I've chosen `stache` as it ports very widespread and simple solution `mustache` to Haskell.
But other template engines can be used in the same way. The `mig` library is not tied
to any of those libraries. Although I've tried `stache` and highly recommend it.
It's easy to use and versatile.

## Summary

We have studied several features that can make HTML-servers more
easy to build. 

* We have discussed how to work with cookies
* How to create stable type-safe URLs
* How to use template engine `stache` (aka mustache) for our sites and make 
   HTML-pages friendly for WEB-designers.

You can study the source code of the example 
[`HtmlTemplate`](https://github.com/anton-k/mig/tree/main/examples/mig-example-apps/HtmlTemplate)
in the `mig` repo to see how those concepts are used in action.


