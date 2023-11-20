module Mig.Core.Class.Url (
  Url (..),
  UrlOf,
  renderUrl,
  ToUrl (..),
) where

import Data.Bifunctor
import Data.Kind
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeLits
import Mig.Core.Api (Path (..), PathItem (..), flatApi, fromFlatApi)
import Mig.Core.Server (Server (..), getServerPaths)
import Mig.Core.Types.Pair
import Mig.Core.Types.Route
import Web.HttpApiData

-- | Url-template type.
data Url = Url
  { path :: Path
  -- ^ relative path
  , queries :: [(Text, Text)]
  -- ^ queries in the URL
  , captures :: Map Text Text
  -- ^ map of captures
  }

{-| Render URL to string-like value.

TODO: use Text.Builder
-}
renderUrl :: (IsString a) => Url -> a
renderUrl url =
  fromString $ Text.unpack $ appendQuery $ mappend "/" $ Text.intercalate "/" $ fmap fromPathItem url.path.unPath
  where
    fromPathItem :: PathItem -> Text
    fromPathItem = \case
      StaticPath text -> text
      CapturePath name -> fromMaybe ("{" <> name <> "}") $ Map.lookup name url.captures

    appendQuery = case url.queries of
      [] -> id
      _ -> \res -> mconcat [res, "?", query]

    query = Text.intercalate "&" $ fmap (\(name, val) -> mconcat [name, "=", val]) url.queries

-------------------------------------------------------------------------------------
-- render routes to safe URLs

-- | Converts route type to URL function
type family UrlOf a :: Type where
  UrlOf (Send method m a) = Url
  UrlOf (a -> b) = (a -> UrlOf b)
  UrlOf (a, b) = (UrlOf a, UrlOf b)
  UrlOf (a, b, c) = (UrlOf a, UrlOf b, UrlOf c)
  UrlOf (a, b, c, d) = (UrlOf a, UrlOf b, UrlOf c, UrlOf d)
  UrlOf (a, b, c, d, e) = (UrlOf a, UrlOf b, UrlOf c, UrlOf d, UrlOf e)
  UrlOf (a, b, c, d, e, f) = (UrlOf a, UrlOf b, UrlOf c, UrlOf d, UrlOf e, UrlOf f)

{-| Converts server to safe url. We can use it to generate
safe URL constructors to be used in HTML templates
An example of how we can create safe URL's. Note
that order of URL's should be the same as in server definition:

> type GreetingRoute = Get Html
> type BlogPostRoute = Optional "id" BlogPostId -> Get Html
> type ListPostsRoute = Get Html
>
> data Routes = Routes
>   { greeting :: GreetingRoute
>   , blogPost :: BlogPostRoute
>   , listPosts :: ListPostsRoute
>   }
>
> -- URLs
>
> data Urls = Urls
>   { greeting :: UrlOf GreetingRoute
>   , blogPost :: UrlOf BlogPostRoute
>   , listPosts :: UrlOf ListPostsRoute
>   }
>
> {\-| Site URL's
> URL's should be listed in the same order as they appear in the server
> -\}
> urls :: Urls
> urls = Urls{..}
>   where
>     greeting
>       :| blogPost
>       :| listPosts
>         toUrl (server undefined)
-}
class ToUrl a where
  toUrl :: Server m -> a
  mapUrl :: (Url -> Url) -> a -> a
  urlArity :: Int

instance (ToUrl a, ToUrl b) => ToUrl (a :| b) where
  toUrl api = a :| b
    where
      (a, b) = toUrl api
  mapUrl f (a :| b) = (mapUrl f a :| mapUrl f b)
  urlArity = urlArity @(a, b)

instance (ToUrl a, ToUrl b) => ToUrl (a, b) where
  toUrl (Server api) = (toUrl (Server apiA), toUrl (Server apiB))
    where
      (apiA, apiB) = bimap fromFlatApi fromFlatApi $ Prelude.splitAt (urlArity @a) (flatApi api)

  mapUrl f (a, b) = (mapUrl f a, mapUrl f b)
  urlArity = urlArity @a + urlArity @b

instance ToUrl Url where
  toUrl server = case getServerPaths server of
    url : _ -> Url url [] mempty
    _ -> Url mempty mempty mempty

  mapUrl f a = f a
  urlArity = 1

-- query

instance (KnownSymbol sym, ToHttpApiData a, ToUrl b) => ToUrl (Query sym a -> b) where
  toUrl server = \(Query val) ->
    mapUrl (insertQuery (getName @sym) (toUrlPiece val)) (toUrl @b server)

  mapUrl f a = \query -> mapUrl f (a query)
  urlArity = urlArity @b

insertQuery :: Text -> Text -> Url -> Url
insertQuery name val url = url{queries = (name, val) : url.queries}

-- optional query

instance (KnownSymbol sym, ToHttpApiData a, ToUrl b) => ToUrl (Optional sym a -> b) where
  toUrl server = \(Optional mVal) ->
    mapUrl (maybe id (insertQuery (getName @sym) . toUrlPiece) mVal) (toUrl @b server)

  mapUrl f a = \query -> mapUrl f (a query)
  urlArity = urlArity @b

-- query flag

instance (KnownSymbol sym, ToUrl b) => ToUrl (QueryFlag sym -> b) where
  toUrl server = \(QueryFlag val) ->
    mapUrl (insertQuery (getName @sym) (toUrlPiece val)) (toUrl @b server)

  mapUrl f a = \query -> mapUrl f (a query)
  urlArity = urlArity @b

-- capture

instance (KnownSymbol sym, ToHttpApiData a, ToUrl b) => ToUrl (Capture sym a -> b) where
  toUrl server = \(Capture val) ->
    mapUrl (insertCapture (getName @sym) (toUrlPiece val)) (toUrl @b server)

  mapUrl f a = \capture -> mapUrl f (a capture)
  urlArity = urlArity @b

insertCapture :: Text -> Text -> Url -> Url
insertCapture name val url = url{captures = Map.insert name val url.captures}

-- body

instance (ToUrl b) => ToUrl (Body media a -> b) where
  toUrl server = const $ toUrl @b server
  mapUrl f a = \body -> mapUrl f (a body)
  urlArity = urlArity @b

-- header

instance (ToUrl b) => ToUrl (Header sym a -> b) where
  toUrl server = const $ toUrl @b server
  mapUrl f a = \header -> mapUrl f (a header)
  urlArity = urlArity @b

-- optional header

instance (ToUrl b) => ToUrl (OptionalHeader sym a -> b) where
  toUrl server = const $ toUrl @b server
  mapUrl f a = \header -> mapUrl f (a header)
  urlArity = urlArity @b

-- cookie

instance (ToUrl b) => ToUrl (Cookie a -> b) where
  toUrl server = const $ toUrl @b server
  mapUrl f a = \header -> mapUrl f (a header)
  urlArity = urlArity @b

-- path info
instance (ToUrl b) => ToUrl (PathInfo -> b) where
  toUrl server = const $ toUrl @b server
  mapUrl f a = \input -> mapUrl f (a input)
  urlArity = urlArity @b

-- full path info
instance (ToUrl b) => ToUrl (FullPathInfo -> b) where
  toUrl server = const $ toUrl @b server
  mapUrl f a = \input -> mapUrl f (a input)
  urlArity = urlArity @b

-- request
instance (ToUrl b) => ToUrl (RawRequest -> b) where
  toUrl server = const $ toUrl @b server
  mapUrl f a = \input -> mapUrl f (a input)
  urlArity = urlArity @b

-- is secure
instance (ToUrl b) => ToUrl (IsSecure -> b) where
  toUrl server = const $ toUrl @b server
  mapUrl f a = \input -> mapUrl f (a input)
  urlArity = urlArity @b

-------------------------------------------------------------------------------------
-- utils

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))
