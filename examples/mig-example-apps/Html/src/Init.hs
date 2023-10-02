module Init (
  initSite,
) where

import Data.IORef
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import System.Log.FastLogger
import System.Random

import Content
import Interface
import Internal.State
import Types

{-| Initialise the logic for our website.
we read the posts from some poems and fill the site with them.

Also we init all actions. Note how we hide the mutable state Env with interface for Site.
-}
initSite :: IO Site
initSite = do
  env <- initEnv
  (writeLog, closeLogger) <- newFastLogger (LogStdout defaultBufSize)
  let
    logInfo msg = do
      now <- getCurrentTime
      writeLog $
        toLogStr $
          Text.unwords
            [ "[INFO]:"
            , msg <> "."
            , "at"
            , Text.pack (show now) <> "\n"
            ]
  pure $
    Site
      { readBlogPost = mockRead env
      , writeBlogPost = mockWriteBlogPost env
      , listBlogPosts = readIORef env.blogPosts
      , readQuote = Quote <$> randomQuote
      , logInfo = logInfo
      , cleanup = do
          logInfo "Blog site shutdown"
          closeLogger
      }

-------------------------------------------------------------------------------------
-- implementation of the site interfaces.
-- It defines how to read blog posts and quotes and how to create new posts
-- note how we use IO-actions. We can also read from DB or do all sorts of things here.

-- | Read the blog post
mockRead :: Env -> BlogPostId -> IO (Maybe BlogPost)
mockRead env postId = do
  blogPosts <- readIORef env.blogPosts
  pure (List.find (\post -> post.id == postId) blogPosts)

-- | Write new blog post
mockWriteBlogPost :: Env -> Text -> Text -> IO BlogPostId
mockWriteBlogPost env title content = do
  pid <- randomBlogPostId
  time <- getCurrentTime
  -- unsafe in concurrent, it is here just for example (use TVar or atomicModifyIORef)
  modifyIORef' env.blogPosts (BlogPost pid title time content :)
  pure pid

randomQuote :: IO Text
randomQuote = oneOf quotes

-------------------------------------------------------------------------------------
-- utils

-- pick random element from a list
oneOf :: [a] -> IO a
oneOf as = (as !!) . (`mod` len) <$> randomIO
  where
    len = length as
