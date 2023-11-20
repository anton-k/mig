module Api (
  Routes (..),
  Urls (..),
  GreetingRoute,
  BlogPostRoute,
  QuoteRoute,
  WriteFormRoute,
  WriteSubmitRoute,
  ListPostsRoute,
  urls,
  server,
) where

import Mig.Html.IO
import Types

-- routes

type GreetingRoute = Get Html
type BlogPostRoute = Optional "id" BlogPostId -> Get Html
type QuoteRoute = Get Html
type WriteFormRoute = Get Html
type WriteSubmitRoute = Body FormUrlEncoded SubmitBlogPost -> Post Html
type ListPostsRoute = Get Html

data Routes = Routes
  { greeting :: GreetingRoute
  , blogPost :: BlogPostRoute
  , quote :: QuoteRoute
  , listPosts :: ListPostsRoute
  , writeForm :: WriteFormRoute
  , writeSubmit :: WriteSubmitRoute
  }

-- URLs

data Urls = Urls
  { greeting :: UrlOf GreetingRoute
  , blogPost :: UrlOf BlogPostRoute
  , quote :: UrlOf QuoteRoute
  , listPosts :: UrlOf ListPostsRoute
  , writeForm :: UrlOf WriteFormRoute
  , writeSubmit :: UrlOf WriteSubmitRoute
  }

{-| Site URL's
URL's should be listed in the same order as they appear in the server
-}
urls :: Urls
urls = Urls{..}
  where
    greeting
      :| blogPost
      :| quote
      :| listPosts
      :| writeForm
      :| writeSubmit =
        toUrl (server undefined)

-- server definition

-- | Server definition. Note how we assemble it from parts with monoid method mconcat.
server :: Routes -> Server IO
server routes =
  addIndex $
    mconcat
      [ defaultPage
      , "blog"
          /. [ readServer
             , writeServer
             ]
      ]
  where
    addIndex = addPathLink "index.html" "/"

    -- default main page
    defaultPage =
      "/" /. routes.greeting

    -- server to read info.
    -- We can read blog posts and quotes.
    readServer =
      toServer
        [ "read"
            /. mconcat
              [ "post" /. routes.blogPost
              , "quote" /. routes.quote
              ]
        , "list" /. routes.listPosts
        ]

    -- server to write new blog posts
    writeServer =
      "write"
        /. [ toServer routes.writeForm
           , toServer routes.writeSubmit
           ]
