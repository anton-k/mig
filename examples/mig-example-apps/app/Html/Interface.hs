-- | Site interfaces as abstractions over interaction with outside world.
-- For example logging, storing new posts in DB, etc.
module Interface
  ( Site (..)
  , initSite
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.IORef
import System.Random
import Data.List qualified as List

import Content
import Types
import Data.Time

-- | Web site actions. It defines interfaces that connect logic of our site
-- with outside world: DBs, logger.
data Site = Site
  { readBlogPost :: BlogPostId -> IO (Maybe BlogPost)
  , writeBlogPost :: Text -> Text -> IO BlogPostId
  , listBlogPosts :: IO [BlogPost]
  , readQuote :: IO Quote
  , logInfo :: Text -> IO ()
  }

-- | Site mutable state
data Env = Env
  { blogPosts :: IORef [BlogPost]
    -- ^ for example we store posts in memory but it also can become a DB.
  }

-- | Initialise the logic for our website.
-- we read the posts from some poems and fill the site with them.
--
-- Also we init all actions. Note how we hide the mutable state Env with interface for Site.
initSite :: IO Site
initSite = do
  posts <- mapM poemToBlogPost poems
  env <- Env <$> newIORef posts
  pure $ Site
    { readBlogPost = mockRead env
    , writeBlogPost = mockWriteBlogPost env
    , listBlogPosts = readIORef env.blogPosts
    , readQuote = Quote <$> randomQuote
    , logInfo = Text.putStrLn . mappend "[INFO]: "
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
  modifyIORef' env.blogPosts (BlogPost pid title time content : )
  pure pid


-- | allocates fresh id for blog post
randomBlogPostId :: IO BlogPostId
randomBlogPostId = BlogPostId . Text.pack . show @Int <$> randomIO

randomQuote :: IO Text
randomQuote = oneOf quotes

poemToBlogPost :: Text -> IO BlogPost
poemToBlogPost poem = do
  pid <- randomBlogPostId
  time <- getCurrentTime
  pure $ BlogPost
    { id = pid
    , createdAt = time
    , title =
        let
          ls = Text.lines poem
        in
          mconcat [ last ls, ": ", head ls]
    , content = poem
    }

-------------------------------------------------------------------------------------
-- utils

-- pick random element from a list
oneOf :: [a] -> IO a
oneOf as = (as !! ) . (`mod` len) <$> randomIO
  where
    len = length as



