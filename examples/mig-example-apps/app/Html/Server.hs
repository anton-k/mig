-- server
module Server
  ( server
  ) where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Mig.Html.IO
import System.Random

import Interface
import View ()
import Types

-- | Server definition. Note how we assemble it from parts with monoid method mconcat.
server :: Site -> Server IO
server site = logRoutes $
  mconcat
    [ "blog" /.
        mconcat
          [ readServer
          , writeServer
          ]
    , defaultPage
    ]
  where
    -- server to read info.
    -- We can read blog posts and quotes.
    readServer =
      mconcat
        [ "read" /.
              mconcat
                [ "post" /. handleBlogPost site
                , "quote" /. handleQuote site
                ]
        , "list" /. handleListPosts site
        ]

    -- server to write new blog posts
    writeServer =
      "write" /.
          mconcat
            [ toServer $ handleWriteForm site
            , toServer $ handleWriteSubmit site
            ]

    -- default main page
    defaultPage =
      mconcat
        [ "index.html" /. handleGreeting site
        , toServer (handleGreeting site)
        ]

    -- log all requests to the server
    logRoutes srv = toServer $ \(PathInfo path) -> withServerAction srv $ do
      when (path /= ["favicon.ico"]) $ do
        logRoute site (Text.intercalate "/" path)

-------------------------------------------------------------------------------------
-- server handlers

-- | Greet the user on main page
handleGreeting :: Site -> Get (Page Greeting)
handleGreeting _site = Get $ pure (Page Greeting)

-- | Read blog post by id
handleBlogPost :: Site -> Optional "id" BlogPostId -> Get (Page BlogPost)
handleBlogPost site (Optional mBlogId) = Get $
  case mBlogId of
    Nothing -> Page <$> randomBlogPost site
    Just blogId -> maybe (PostNotFound blogId) Page <$> site.readBlogPost blogId

-- | Read random quote
handleQuote :: Site -> Get (Page Quote)
handleQuote site = Get $ Page <$> site.readQuote

-- | Show form to the user to fill new post data
handleWriteForm :: Site -> Get (Page WritePost)
handleWriteForm _site = Get $
  pure $ Page WritePost

-- | Submit form with data provided by the user
handleWriteSubmit :: Site -> FormJson SubmitBlogPost -> Post (Page BlogPost)
handleWriteSubmit site (FormJson (SubmitBlogPost title content)) = Post $ do
  pid <- site.writeBlogPost title content
  maybe (PostNotFound pid) Page <$> site.readBlogPost pid

-- | List all posts so far
handleListPosts :: Site -> Get (Page ListPosts)
handleListPosts site = Get $ do
  Page . ListPosts <$> site.listBlogPosts

-- | Logs the route info
logRoute :: Site -> Text -> IO ()
logRoute site route = do
  time <- getCurrentTime
  site.logInfo $ route <> " page visited at: " <> Text.pack (show time)

-- | Get random blog post
randomBlogPost :: Site -> IO BlogPost
randomBlogPost site =
  oneOf =<< site.listBlogPosts

-------------------------------------------------------------------------------------
-- utils

-- pick random element from a list
oneOf :: [a] -> IO a
oneOf as = (as !! ) . (`mod` len) <$> randomIO
  where
    len = length as
