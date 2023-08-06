-- | Site interfaces as abstractions over interaction with outside world.
-- For example logging, storing new posts in DB, etc.
module Interface
  ( Site (..)
  ) where

import Data.Text (Text)
import Types

-- | Web site actions. It defines interfaces that connect logic of our site
-- with outside world: DBs, logger.
data Site = Site
  { readBlogPost :: BlogPostId -> IO (Maybe BlogPost)
  , writeBlogPost :: Text -> Text -> IO BlogPostId
  , listBlogPosts :: IO [BlogPost]
  , readQuote :: IO Quote
  , logInfo :: Text -> IO ()
  }
