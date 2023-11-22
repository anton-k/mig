module Mig.Core.Class.Url (
  Url (..),
  UrlOf,
  renderUrl,
  ToUrl (..),
) where

import Data.Aeson (ToJSON (..))
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

instance ToJSON Url where
  toJSON = toJSON . renderUrl @Text

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
  UrlOf (Query name value -> b) = (Query name value -> UrlOf b)
  UrlOf (Optional name value -> b) = (Optional name value -> UrlOf b)
  UrlOf (Capture name value -> b) = (Capture name value -> UrlOf b)
  UrlOf (QueryFlag name -> b) = (QueryFlag name -> UrlOf b)
  UrlOf (Header name value -> b) = UrlOf b
  UrlOf (OptionalHeader name value -> b) = UrlOf b
  UrlOf (Body media value -> b) = UrlOf b
  UrlOf (Cookie value -> b) = UrlOf b
  UrlOf (PathInfo -> b) = UrlOf b
  UrlOf (FullPathInfo -> b) = UrlOf b
  UrlOf (RawRequest -> b) = UrlOf b
  UrlOf (IsSecure -> b) = UrlOf b
  UrlOf (a, b) = (UrlOf a, UrlOf b)
  UrlOf (a, b, c) = (UrlOf a, UrlOf b, UrlOf c)
  UrlOf (a, b, c, d) = (UrlOf a, UrlOf b, UrlOf c, UrlOf d)
  UrlOf (a, b, c, d, e) = (UrlOf a, UrlOf b, UrlOf c, UrlOf d, UrlOf e)
  UrlOf (a, b, c, d, e, f) = (UrlOf a, UrlOf b, UrlOf c, UrlOf d, UrlOf e, UrlOf f)
  UrlOf (a :| b) = UrlOf a :| UrlOf b

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

instance (ToUrl a, ToUrl b, ToUrl c) => ToUrl (a, b, c) where
  toUrl server = fromPair $ toUrl @(a, (b, c)) server
    where
      fromPair (a, (b, c)) = (a, b, c)

  mapUrl f (a, b, c) = (mapUrl f a, mapUrl f b, mapUrl f c)
  urlArity = urlArity @a + urlArity @b + urlArity @c

instance (ToUrl a, ToUrl b, ToUrl c, ToUrl d) => ToUrl (a, b, c, d) where
  toUrl server = fromPair $ toUrl @(a, (b, c, d)) server
    where
      fromPair (a, (b, c, d)) = (a, b, c, d)

  mapUrl f (a, b, c, d) = (mapUrl f a, mapUrl f b, mapUrl f c, mapUrl f d)
  urlArity = urlArity @a + urlArity @b + urlArity @c + urlArity @d

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

-------------------------------------------------------------------------------------
-- utils

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))
