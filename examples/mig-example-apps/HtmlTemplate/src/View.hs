{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | html renderers. View for all pages
module View () where

import Api (Urls (..), urls)
import Data.Aeson qualified as Json
import Data.Text.Lazy qualified as LazyText
import Mig
import Mig.Html (Link (..))
import Text.Blaze.Html.Renderer.Text qualified as H
import Text.Blaze.Html5 qualified as H
import Text.Mustache
import Text.Mustache.Compile.TH qualified as TH
import Types

renderMustacheHtml :: (ToJSON a) => Template -> a -> Html
renderMustacheHtml template value =
  H.preEscapedLazyText $ renderMustache template (toJSON value)

-- | Templates for the site
data Templates = Templates
  { main :: Template
  , greeting :: Template
  , post :: Template
  , quote :: Template
  , writeForm :: Template
  , listPosts :: Template
  , postNotFound :: Template
  }

-- | Loads templates with template haskell as pure values
templates :: Templates
templates =
  Templates
    { main = $(TH.compileMustacheFile "HtmlTemplate/templates/main.html")
    , greeting = $(TH.compileMustacheFile "HtmlTemplate/templates/greeting.html")
    , post = $(TH.compileMustacheFile "HtmlTemplate/templates/post.html")
    , quote = $(TH.compileMustacheFile "HtmlTemplate/templates/quote.html")
    , writeForm = $(TH.compileMustacheFile "HtmlTemplate/templates/writeForm.html")
    , listPosts = $(TH.compileMustacheFile "HtmlTemplate/templates/listPosts.html")
    , postNotFound = $(TH.compileMustacheFile "HtmlTemplate/templates/postNotFound.html")
    }

data MainPage = MainPage
  { title :: Text
  , menuLinks :: [Link]
  , content :: LazyText.Text
  }
  deriving (Generic, ToJSON)

-- writes the template for main page
instance (ToMarkup a) => ToMarkup (Page a) where
  toMarkup page = case page of
    Page a ->
      renderMustacheHtml templates.main $
        MainPage
          { title = "Blog example"
          , menuLinks = siteMenuLinks
          , content = H.renderHtml (H.toMarkup a)
          }

siteMenuLinks :: [Link]
siteMenuLinks =
  [ Link
      { name = "main page"
      , href = urls.greeting
      }
  , Link
      { name = "next post"
      , href = urls.blogPost $ Optional Nothing
      }
  , Link
      { name = "next quote"
      , href = urls.quote
      }
  , Link
      { name = "write new post"
      , href = urls.writeForm
      }
  , Link
      { name = "list all posts"
      , href = urls.listPosts
      }
  ]

-- Rendering of the greeting page
instance ToMarkup Greeting where
  toMarkup (Greeting posts) = renderMustacheHtml templates.greeting $ toPostLinks posts

-- Rendering of the form to submit the post
instance ToMarkup WritePost where
  toMarkup WritePost = renderMustacheHtml templates.writeForm $ Link urls.writeForm "Submit"

instance ToMarkup BlogPostView where
  toMarkup = \case
    ViewBlogPost post -> toMarkup post
    PostNotFound _pid -> renderMustacheHtml templates.postNotFound ()

-- | Rendering of a single blog post
instance ToMarkup BlogPost where
  toMarkup post = renderMustacheHtml templates.post post

-- Rendering of a single quote
instance ToMarkup Quote where
  toMarkup quote = renderMustacheHtml templates.quote quote

-- | Rendering of all submited posts
instance ToMarkup ListPosts where
  toMarkup (ListPosts posts) = renderMustacheHtml templates.listPosts $ toPostLinks posts

toPostLinks :: [BlogPostLink] -> Json.Value
toPostLinks posts =
  Json.object ["posts" Json..= fmap toLink posts]
  where
    toLink :: BlogPostLink -> Link
    toLink post =
      Link
        { href = urls.blogPost (Optional $ Just post.blogPostId)
        , name = post.title
        }
