-- | types
module Types
  ( Page (..)
  , Greeting (..)
  , WritePost (..)
  , ListPosts (..)
  , BlogPostId (..)
  , BlogPost (..)
  , Quote (..)
  , SubmitBlogPost (..)
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Mig (FromHttpApiData, FromForm)

-- | Web-page for our site
data Page a
  = Page a
    -- ^ page with some content
  | PostNotFound BlogPostId
    -- ^ error: post not found by id

-- | Greeting page
data Greeting = Greeting

-- | Form to submit new post
data WritePost = WritePost

-- | List all posts
newtype ListPosts = ListPosts [BlogPost]

-- | Blog post id
newtype BlogPostId = BlogPostId { unBlogPostId :: Text }
  deriving newtype  (FromHttpApiData, Eq, Show, FromJSON)

-- | Blog post
data BlogPost = BlogPost
  { id ::BlogPostId
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
  deriving (Generic, FromForm)
