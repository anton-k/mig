-- | types
module Types (
  Page (..),
  Greeting (..),
  WritePost (..),
  ListPosts (..),
  BlogPostId (..),
  BlogPostView (..),
  BlogPost (..),
  Quote (..),
  SubmitBlogPost (..),
) where

import Data.Text (Text)
import Data.Time
import Data.UUID
import Mig (deriveForm, deriveNewtypeParam, mapDerive)

-- | Web-page for our site
newtype Page a = Page a

-- | Greeting page
data Greeting = Greeting [BlogPost]

-- | Form to submit new post
data WritePost = WritePost

-- | List all posts
newtype ListPosts = ListPosts [BlogPost]

-- | Blog post id
newtype BlogPostId = BlogPostId {unBlogPostId :: UUID}

data BlogPostView
  = ViewBlogPost BlogPost
  | -- | error: post not found by id
    PostNotFound BlogPostId

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

--------------------------------------------
-- derivings

mapDerive deriveNewtypeParam [''BlogPostId]
deriveForm ''SubmitBlogPost
