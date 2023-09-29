{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | html renderers. View for all pages
module View () where

import Data.List qualified as List
import Data.Text (Text)
import Text.Blaze.Html (Html, ToMarkup)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Types

-- writes the template for main page
instance (ToMarkup a) => ToMarkup (Page a) where
  toMarkup page = case page of
    Page a -> siteTemplate (H.toMarkup a)
    PostNotFound _pid -> siteTemplate $ H.p (H.text "Post not found")

-- | Main site template
siteTemplate :: Html -> Html
siteTemplate content = H.html $ do
  H.head $ do
    H.link H.! HA.rel "stylesheet" H.! HA.href "https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
    H.link H.! HA.rel "stylesheet" H.! HA.href "/static/milligram.min.css"
  H.body $ H.div H.! HA.style "margin-left:4%; margin-top: 3%; font-size: 110%" $ do
    H.div H.! HA.class_ "container" $ do
      H.div H.! HA.class_ "row" $ do
        H.div H.! HA.class_ "column column-20" $ menu
        H.div H.! HA.class_ "column column-75 column-offset-5" $ content
  where
    menu = do
      H.div $
        H.ul H.! HA.style "list-style: none" $ do
          item "/index.html" "main page"
          item "/blog/read/post" "next post"
          item "/blog/read/quote" "next quote"
          item "/blog/write" "write new post"
          item "/blog/list" "list all posts"

    item ref name =
      H.li $ H.a H.! HA.href ref $ H.text name

-- Rendering of the greeting page
instance ToMarkup Greeting where
  toMarkup Greeting = do
    H.div $ do
      H.h2 "Welcome to blog site example"
      H.p "You can get random poem or random quote from menu bar"

-- Rendering of the form to submit the post
instance ToMarkup WritePost where
  toMarkup WritePost = do
    H.div $ do
      H.h2 "Write new post"
      H.form H.! HA.method "POST" H.! HA.action "/blog/write" $ do
        inputText "title"
        inputContent "content"
        submit "Save blog post"
    where
      inputText name = H.div $ do
        H.p (H.text $ "Input " <> name)
        H.textarea H.! HA.rows "1" H.! HA.cols "100" H.! HA.id (H.toValue name) H.! HA.name (H.toValue name) $ pure ()

      inputContent name = H.div $ do
        H.p (H.text $ "Input " <> name)
        H.textarea H.! HA.rows "10" H.! HA.cols "100" H.! HA.id (H.toValue name) H.! HA.name (H.toValue name) $ pure ()

      submit :: Text -> Html
      submit name = H.div $ H.input H.! HA.type_ "submit" H.! HA.value (H.toValue name)

-- | Rendering of a single blog post
instance ToMarkup BlogPost where
  toMarkup post =
    H.div $ do
      H.div $ H.h2 $ H.toHtml post.title
      H.div $ H.p $ H.toHtml ("Created at: " <> show post.createdAt)
      H.div H.! HA.style "white-space: pre-wrap" $
        H.text post.content

-- Rendering of a single quote
instance ToMarkup Quote where
  toMarkup quote = do
    H.div $ H.h2 "Quote of the day:"
    H.div $ H.p $ H.text quote.content

-- | Rendering of all submited posts
instance ToMarkup ListPosts where
  toMarkup (ListPosts posts) =
    H.div $ do
      H.h2 $ H.text "Posts:"
      H.ul $ mapM_ (\p -> H.li $ toPostSummary p) $ List.sortOn (.createdAt) posts
    where
      toPostSummary post =
        H.a H.! HA.href (H.toValue $ "/blog/read/post?id=" <> post.id.unBlogPostId) $
          H.text $
            post.title
