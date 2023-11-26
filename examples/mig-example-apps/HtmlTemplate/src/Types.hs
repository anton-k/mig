-- | types
module Types (
  Page (..),
  Greeting (..),
  WritePost (..),
  ListPosts (..),
  BlogPostId (..),
  BlogPostView (..),
  BlogPost (..),
  BlogPostLink (..),
  toBlogPostLink,
  Quote (..),
  SubmitBlogPost (..),
) where

import Data.Time
import Data.UUID
import Mig.Html.IO

-- | Web-page for our site
newtype Page a = Page a

-- | Form to submit new post
data WritePost = WritePost

-- | Blog post id
newtype BlogPostId = BlogPostId {unBlogPostId :: UUID}

mapDerive deriveNewtypeParam [''BlogPostId]

data Link = Link
  { href :: Text
  , name :: Text
  }
  deriving (Generic, ToJSON)

data BlogPostView
  = ViewBlogPost BlogPost
  | -- | error: post not found by id
    PostNotFound BlogPostId

data BlogPostLink = BlogPostLink
  { blogPostId :: BlogPostId
  , title :: Text
  }

toBlogPostLink :: BlogPost -> BlogPostLink
toBlogPostLink post = BlogPostLink post.id post.title

-- | Greeting page
newtype Greeting = Greeting [BlogPostLink]

-- | List all posts
newtype ListPosts = ListPosts [BlogPostLink]

-- | Blog post
data BlogPost = BlogPost
  { id :: BlogPostId
  , title :: Text
  , createdAt :: UTCTime
  , content :: Text
  }
  deriving (Generic, ToJSON)

-- | A quote
data Quote = Quote
  { content :: Text
  }
  deriving (Generic, ToJSON)

-- | Data to submit new blog post
data SubmitBlogPost = SubmitBlogPost
  { title :: Text
  , content :: Text
  }

--------------------------------------------
-- derivings

deriveForm ''SubmitBlogPost
