-- | types
module Types (
  Page (..),
  Greeting (..),
  WritePost (..),
  ListPosts (..),
  BlogPostId (..),
  BlogPost (..),
  Quote (..),
  SubmitBlogPost (..),
) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Time
import Data.UUID
import GHC.Generics
import Mig (FromForm, FromHttpApiData, ToHttpApiData, ToParamSchema, ToSchema)

-- | Web-page for our site
data Page a
  = -- | page with some content
    Page a
  | -- | error: post not found by id
    PostNotFound BlogPostId

-- | Greeting page
data Greeting = Greeting [BlogPost]

-- | Form to submit new post
data WritePost = WritePost

-- | List all posts
newtype ListPosts = ListPosts [BlogPost]

-- | Blog post id
newtype BlogPostId = BlogPostId {unBlogPostId :: UUID}
  deriving newtype (FromHttpApiData, ToHttpApiData, Eq, Show, FromJSON, ToParamSchema)

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
