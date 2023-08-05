{-# Language
      OverloadedStrings,
      OverloadedRecordDot,
      DeriveAnyClass,
      DerivingStrategies,
      DuplicateRecordFields,
      DataKinds
#-}
-- | Example on how to serve Html
--
-- We create a simple blog post site which offers two types of content:
--
-- * blog posts
-- * quotes
--
-- We can choose between the two. Also we can create new blog posts and list them.
-- all posts are stored in memory.
module Example.Html
  ( main
  ) where

import Mig.Html.IO
import Control.Monad
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

import Text.Blaze.Html (Html)
import Text.Blaze.Html (ToMarkup)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Data.Time
import System.Random
import Data.List qualified as List
import Data.IORef
import GHC.Generics
import Data.Aeson (FromJSON)

-- run blog post server
main :: IO ()
main = do
  site <- initSite
  runServer 8085 $ server site

-------------------------------------------------------------------------------------
-- server

-- | Server definition. Note how we assemble it from parts with monoid method mconcat.
server :: Site -> Server IO
server site = logRoutes $
  mconcat
    [ "blog" /.
        mconcat
          [ readServer
          , writeServer
          ]
    , defaultPage
    ]
  where
    -- server to read info.
    -- We can read blog posts and quotes.
    readServer =
      mconcat
        [ "read" /.
              mconcat
                [ "post" /. handleBlogPost site
                , "quote" /. handleQuote site
                ]
        , "list" /. handleListPosts site
        ]

    -- server to write new blog posts
    writeServer =
      "write" /.
          mconcat
            [ toServer $ handleWriteForm site
            , toServer $ handleWriteSubmit site
            ]

    -- default main page
    defaultPage =
      mconcat
        [ "index.html" /. handleGreeting site
        , toServer (handleGreeting site)
        ]

    -- log all requests to the server
    logRoutes srv = toServer $ \(PathInfo path) -> withServerAction srv $ do
      when (path /= ["favicon.ico"]) $ do
        logRoute site (Text.intercalate "/" path)

-------------------------------------------------------------------------------------
-- server handlers

-- | Greet the user on main page
handleGreeting :: Site -> Get (Page Greeting)
handleGreeting _site = Get $ pure (Page Greeting)

-- | Read blog post by id
handleBlogPost :: Site -> Optional "id" BlogPostId -> Get (Page BlogPost)
handleBlogPost site (Optional mBlogId) = Get $
  case mBlogId of
    Nothing -> Page <$> randomBlogPost site
    Just blogId -> maybe (PostNotFound blogId) Page <$> site.readBlogPost blogId

-- | Read random quote
handleQuote :: Site -> Get (Page Quote)
handleQuote site = Get $ Page <$> site.readQuote

-- | Show form to the user to fill new post data
handleWriteForm :: Site -> Get (Page WritePost)
handleWriteForm site = Get $
  pure $ Page WritePost

-- | Submit form with data provided by the user
handleWriteSubmit :: Site -> FormJson SubmitBlogPost -> Post (Page BlogPost)
handleWriteSubmit site (FormJson (SubmitBlogPost title content)) = Post $ do
  pid <- site.writeBlogPost title content
  maybe (PostNotFound pid) Page <$> site.readBlogPost pid

-- | List all posts so far
handleListPosts :: Site -> Get (Page ListPosts)
handleListPosts site = Get $ do
  Page . ListPosts <$> site.listBlogPosts

-- | Logs the route info
logRoute :: Site -> Text -> IO ()
logRoute site route = do
  time <- getCurrentTime
  site.logInfo $ route <> " page visited at: " <> Text.pack (show time)

-------------------------------------------------------------------------------------
-- types

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
  deriving newtype  (FromText, Eq, Show, FromJSON)

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

-- | Web site actions. It defines interfaces that connect logic of our site
-- with outside world: DBs, logger.
data Site = Site
  { readBlogPost :: BlogPostId -> IO (Maybe BlogPost)
  , writeBlogPost :: Text -> Text -> IO BlogPostId
  , listBlogPosts :: IO [BlogPost]
  , readQuote :: IO Quote
  , logInfo :: Text -> IO ()
  }

-- | Data to submit new blog post
data SubmitBlogPost = SubmitBlogPost
  { title :: Text
  , content :: Text
  }
  deriving (Generic, FromJSON)

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
-- html renderers. View for all pages

-- writes the template for main page
instance ToMarkup a => ToMarkup (Page a) where
  toMarkup page = case page of
    Page a -> siteTemplate (H.toMarkup a)
    PostNotFound _pid -> siteTemplate $ H.p (H.text "Post not found")

-- | Main site template
siteTemplate :: Html -> Html
siteTemplate content = H.html $ do
  H.head $ do
    H.style H.! HA.type_ "text/css" $ H.text "a { text-decoration: none } "
  H.body $ H.div H.! HA.style "margin-left:4%; margin-top: 3%; font-size: 120%" $ do
    H.div menu
    H.div content
  where
    menu = do
      H.div (H.h2 "Menu:")
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
        H.p $ H.a H.! HA.href (H.toValue $ "/blog/read/post?id=" <> post.id.unBlogPostId) $
          H.text $ post.title

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

-- | Get random blog post
randomBlogPost :: Site -> IO BlogPost
randomBlogPost site =
  oneOf =<< site.listBlogPosts

-- | allocates fresh id for blog post
randomBlogPostId :: IO BlogPostId
randomBlogPostId = BlogPostId . Text.pack . show @Int <$> randomIO

-------------------------------------------------------------------------------------
-- utils

-- pick random element from a list
oneOf :: [a] -> IO a
oneOf as = (as !! ) . (`mod` len) <$> randomIO
  where
    len = length as

-------------------------------------------------------------------------------------
-- random content for blog posts

randomPoem :: IO Text
randomPoem = oneOf poems

poems :: [Text]
poems = [lukomorye, tweedle, dream, pie, toSeeAWorld]

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

lukomorye :: Text
lukomorye =
  Text.unlines
    [ "У лукоморья дуб зелёный"
    , "Златая цепь на дубе том"
    , "И днём и ночью кот учёный"
    , "Всё ходит по цепи кругом;"
    , "Идёт направо — песнь заводит,"
    , "Налево — сказку говорит."
    , "Там чудеса: там леший бродит,"
    , "Русалка на ветвях сидит;"
    , "Там на неведомых дорожках"
    , "Следы невиданных зверей;"
    , "Избушка там на курьих ножках"
    , "Стоит без окон, без дверей;"
    , "Там лес и дол видений полны;"
    , "Там о заре прихлынут волны"
    , "На брег песчаный и пустой,"
    , "И тридцать витязей прекрасных"
    , "Чредой из вод выходят ясных,"
    , "И с ними дядька их морской;"
    , "Там королевич мимоходом"
    , "Пленяет грозного царя;"
    , "Там в облаках перед народом"
    , "Через леса, через моря"
    , "Колдун несёт богатыря;"
    , "В темнице там царевна тужит,"
    , "А бурый волк ей верно служит;"
    , "Там ступа с Бабою Ягой"
    , "Идёт, бредёт сама собой,"
    , "Там царь Кащей над златом чахнет;"
    , "Там русский дух… там Русью пахнет!"
    , "И там я был, и мёд я пил;"
    , "У моря видел дуб зелёный;"
    , "Под ним сидел, и кот учёный"
    , "Свои мне сказки говорил."
    , ""
    , "Александр Пушкин"
    ]

dream :: Text
dream =
  Text.unlines
    [ "И это снилось мне, и это снится мне,"
    , "И это мне ещё когда-нибудь приснится,"
    , "И повторится всё, и всё довоплотится,"
    , "И вам приснится всё, что видел я во сне."
    , ""
    , "Там, в стороне от нас, от мира в стороне"
    , "Волна идёт вослед волне о берег биться,"
    , "А на волне звезда, и человек, и птица,"
    , "И явь, и сны, и смерть — волна вослед волне."
    , ""
    , "Не надо мне числа: я был, и есмь, и буду,"
    , "Жизнь — чудо из чудес, и на колени чуду"
    , "Один, как сирота, я сам себя кладу,"
    , "Один, среди зеркал — в ограде отражений"
    , "Морей и городов, лучащихся в чаду."
    , "И мать в слезах берёт ребёнка на колени."
    , ""
    , "Арсений Тарковский"
    ]

pie :: Text
pie = Text.unlines
    [ "Cottleston Cottleston Cottleston Pie,"
    , "A fly can't bird, but a bird can fly."
    , "Ask me a riddle and I reply"
    , "Cottleston Cottleston Cottleston Pie."
    , ""
    , "Cottleston Cottleston Cottleston Pie,"
    , "Why does a chicken? I don't know why."
    , "Ask me a riddle and I reply"
    , "Cottleston Cottleston Cottleston Pie."
    , ""
    , "Alan Milne"
    ]

toSeeAWorld :: Text
toSeeAWorld =
  Text.unlines
    [ "To see a World in a Grain of Sand"
    , "And a Heaven in a Wild Flower,"
    , "Hold Infinity in the palm of your hand"
    , "And Eternity in an hour"
    , ""
    , "William Blake"
    ]

tweedle :: Text
tweedle =
  Text.unlines
    [ "Tweedledum and Tweedledee"
    , "Agreed to have a battle;"
    , "For Tweedledum said Tweedledee"
    , "Had spoiled his nice new rattle."
    , "Just then flew down a monstrous crow,"
    , "As black as a tar-barrel;"
    , "Which frightened both the heroes so,"
    , "They quite forgot their quarrel."
    , ""
    , "Lewis Carroll"
    ]

-------------------------------------------------------------------------------------
-- random content for quotes

randomQuote :: IO Text
randomQuote = oneOf quotes

quotes :: [Text]
quotes =
  [ "“We ascribe beauty to that which is simple; which has no superfluous parts; which exactly answers its end; which stands related to all things; which is the mean of many extremes.” - Ralph Waldo Emerson"
  , "“Each day a few more lies eat into the seed with which we are born, little institutional lies from the print of newspapers, the shock waves of television, and the sentimental cheats of the movie screen.” - Norman Mailer"
  , "“Let us so live that when we come to die even the undertaker will be sorry.” - Mark Twain"
  , "“When we seek to discover the best in others, we somehow bring out the best in ourselves.” - William Arthur Ward"
  , "“Make things as simple as possible, but not simpler.” - Albert Einstein"
  , "“Go often to the house of thy friend, for weeds choke the unused path.” - Ralph Waldo Emerson"
  , "“Hating people is like burning down your own house to get rid of a rat.” - Henry Emerson Fosdick"
  , "“The most pitiful among men is he who turns his dreams into silver and gold.” - Kahlil Gibran"
  , "“Persistent people begin their success where others end in failures.” - Edward Eggleston"
  , "“Pick battles big enough to matter, small enough to win.” - Jonathan Kozol"
  , "“Show me a man with both feet on the ground, and I’ll show you a man who can’t put his pants on.” - Arthur K. Watson"
  , "“The more original a discovery, the more obvious it seems afterward.” - Arthur Koestler"
  , "“Loyalty to a petrified opinion never yet broke a chain or freed a human soul.” - Mark Twain"
  , "“O senseless man, who cannot possibly make a worm and yet will make Gods by the dozen!” - Michel de Montaigne"
  , "“Talent develops in tranquility, character in the full current of human life.” - Johann Wolfgang von Goethe"
  , "“The happiness of your life depends upon the quality of your thoughts: therefore, guard accordingly, and take care that you entertain no notions unsuitable to virtue and reasonable nature.” - Marcus Aurelius"
  , "“Character cannot be developed in ease and quiet. Only through experience of trial and suffering can the soul be strengthened, ambition inspired, and success achieved.” - Helen Adams Keller"
  , "“There is nothing with which every man is so afraid as getting to know how enormously much he is capable of doing and becoming.” - Soren Kierkegaard"
  , "“If you want to build a ship, don’t drum up people together to collect wood and don’t assign them tasks and work, but rather teach them to long for the endless immensity of the sea.” - Antoine de Saint-Exupéry"
  , "“Inside every large program is a small program struggling to get out.” - Tony Hoare"
  , "“Man is most nearly himself when he achieves the seriousness of a child at play.” - Heraclitus"
  , "“Most of us are just about as happy as we make up our minds to be.” - Abraham Lincoln"
  ]
