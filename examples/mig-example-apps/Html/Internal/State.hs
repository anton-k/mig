module Internal.State (
  Env (..),
  initEnv,
  randomBlogPostId,
) where

import Content
import Types

import Data.IORef
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import System.Random

-- | Site mutable state
data Env = Env
  { blogPosts :: IORef [BlogPost]
  -- ^ for example we store posts in memory but it also can become a DB.
  }

initEnv :: IO Env
initEnv = do
  posts <- mapM poemToBlogPost poems
  Env <$> newIORef posts

poemToBlogPost :: Text -> IO BlogPost
poemToBlogPost poem = do
  pid <- randomBlogPostId
  time <- getCurrentTime
  pure $
    BlogPost
      { id = pid
      , createdAt = time
      , title =
          let
            ls = Text.lines poem
           in
            mconcat [last ls, ": ", head ls]
      , content = poem
      }

-- | allocates fresh id for blog post
randomBlogPostId :: IO BlogPostId
randomBlogPostId = BlogPostId . Text.pack . show @Int <$> randomIO
