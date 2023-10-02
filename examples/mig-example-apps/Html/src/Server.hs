{-# LANGUAGE TemplateHaskell #-}

-- server
module Server (
  server,
) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import Data.Time
import FileEmbedLzma
import Safe (headMay)

import Mig.Html.IO
import System.Random

import Interface
import Types
import View ()

-- import ToMarkup instances for Html rendering

-- | Server definition. Note how we assemble it from parts with monoid method mconcat.
server :: Site -> Server IO
server site =
  logRoutes $
    mconcat
      [ "blog"
          /. mconcat
            [ readServer
            , writeServer
            ]
      , defaultPage
      , addFavicon $ "static" /. staticFiles resourceFiles
      ]
  where
    addFavicon = addPathLink "favicon.ico" "static/lambda-logo.png"

    -- server to read info.
    -- We can read blog posts and quotes.
    readServer =
      mconcat
        [ "read"
            /. mconcat
              [ "post" /. handleBlogPost site
              , "quote" /. handleQuote site
              ]
        , "list" /. handleListPosts site
        ]

    -- server to write new blog posts
    writeServer =
      "write"
        /. mconcat
          [ toServer $ handleWriteForm site
          , toServer $ handleWriteSubmit site
          ]

    -- default main page
    defaultPage =
      mconcat
        [ "/" /. handleGreeting site
        , "index.html" /. handleGreeting site
        ]

    logRoutes :: Server IO -> Server IO
    logRoutes = applyMiddleware $ \(PathInfo path) -> prependServerAction $
      when (path /= ["favicon.ico"] && headMay path /= Just "static") $ do
        logRoute site (Text.intercalate "/" path)

-------------------------------------------------------------------------------------
-- server handlers

-- | Greet the user on main page
handleGreeting :: Site -> Get (Page Greeting)
handleGreeting site =
  Send $ ok . Page . Greeting <$> site.listBlogPosts

-- | Read blog post by id
handleBlogPost :: Site -> Optional "id" BlogPostId -> Get (Page BlogPost)
handleBlogPost site (Optional mBlogId) = Send $
  case mBlogId of
    Nothing -> ok . Page <$> randomBlogPost site
    Just blogId -> bad notFound404 . maybe (PostNotFound blogId) Page <$> site.readBlogPost blogId

-- | Read random quote
handleQuote :: Site -> Get (Page Quote)
handleQuote site = Send $ ok . Page <$> site.readQuote

-- | Show form to the user to fill new post data
handleWriteForm :: Site -> Get (Page WritePost)
handleWriteForm _site =
  Send $
    pure $
      ok $
        Page WritePost

-- | Submit form with data provided by the user
handleWriteSubmit :: Site -> ReqBody FormUrlEncoded SubmitBlogPost -> Post (Page BlogPost)
handleWriteSubmit site (ReqBody (SubmitBlogPost title content)) = Send $ do
  pid <- site.writeBlogPost title content
  bad notFound404 . maybe (PostNotFound pid) Page <$> site.readBlogPost pid

-- | List all posts so far
handleListPosts :: Site -> Get (Page ListPosts)
handleListPosts site = Send $ do
  ok . Page . ListPosts <$> site.listBlogPosts

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
-- static files

resourceFiles :: [(FilePath, ByteString)]
resourceFiles = $(embedRecursiveDir "Html/resources")

-------------------------------------------------------------------------------------
-- utils

-- pick random element from a list
oneOf :: [a] -> IO a
oneOf as = (as !!) . (`mod` len) <$> randomIO
  where
    len = length as
