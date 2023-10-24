-- | Core API description
module Mig.Core.Api (
  Api (..),
  Path (..),
  ApiNormal (..),
  toNormalApi,
  fromNormalApi,
  PathItem (..),
  getPath,
  CaptureMap,
  flatApi,
  fromFlatApi,
  OutputMediaMap (..),
  InputMediaMap (..),
  MediaMap (..),
) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Mig.Core.Class.Route qualified as Route
import Mig.Core.Types (RouteInfo (..), RouteOutput (..), getInputType)
import Network.HTTP.Media (mapAcceptMedia, mapContentMedia)
import Network.HTTP.Media.MediaType (MediaType)
import Network.HTTP.Types.Method
import System.FilePath
import Web.HttpApiData

-- | HTTP API container
data Api a
  = -- | alternative between two API's
    Append (Api a) (Api a)
  | -- | an empty API that does nothing
    Empty
  | -- | path prefix for an API
    WithPath Path (Api a)
  | -- | handle route
    HandleRoute a
  deriving (Functor, Foldable, Traversable, Show, Eq)

instance Monoid (Api a) where
  mempty = Empty

instance Semigroup (Api a) where
  (<>) = Append

-- | Filters routes in the API with predicate
filterApi :: (a -> Bool) -> Api a -> Api a
filterApi check = \case
  HandleRoute a -> if check a then HandleRoute a else Empty
  Append a b ->
    case rec a of
      Empty -> rec b
      otherA ->
        case rec b of
          Empty -> otherA
          otherB -> Append otherA otherB
  Empty -> Empty
  WithPath path a -> case rec a of
    Empty -> Empty
    other -> WithPath path other
  where
    rec = filterApi check

-- | converts API to efficient representation to fetch the route handlers by path
toNormalApi :: forall m. Api (Route.Route m) -> ApiNormal (Api (Route.Route m))
toNormalApi api = ApiNormal $ fmap (fmap toInputMediaMap . toOutputMediaMap) (toMethodMap api)
  where
    filterEmpty :: Map key (Api val) -> Map key (Api val)
    filterEmpty = Map.filter $ \case
      Empty -> False
      _ -> True

    toMethodMap :: Api (Route.Route m) -> MethodMap (Api (Route.Route m))
    toMethodMap a =
      filterEmpty $
        Map.fromList $
          fmap
            ( \m ->
                (m, filterApi (\route -> route.info.method == Just m) a)
            )
            methods
      where
        methods = foldMap (\route -> maybe [] pure route.info.method) a

    toInputMediaMap :: Api (Route.Route m) -> InputMediaMap (Api (Route.Route m))
    toInputMediaMap = InputMediaMap . toMediaMapBy getInputType

    toOutputMediaMap :: Api (Route.Route m) -> OutputMediaMap (Api (Route.Route m))
    toOutputMediaMap = OutputMediaMap . toMediaMapBy (\routeInfo -> Just routeInfo.output.media)

    toMediaMapBy :: (RouteInfo -> Maybe MediaType) -> Api (Route.Route m) -> MediaMap (Api (Route.Route m))
    toMediaMapBy getMedia a =
      MediaMap (filterAnyCases $ toMediaApi <$> medias) a
      where
        medias = Set.toList $ foldMap (\route -> maybe Set.empty Set.singleton (getMedia route.info)) a

        toMediaApi media = (media, filterApi (\route -> getMedia route.info == Just media) a)

        -- filter out any cases as they are covered by second argument of MediaMap value
        filterAnyCases = filter (("*/*" /=) . fst)

-- | Read sub-api by HTTP method, accept-type and content-type
fromNormalApi :: Method -> ByteString -> ByteString -> ApiNormal a -> Maybe a
fromNormalApi method outputAccept inputContentType (ApiNormal methodMap) = do
  OutputMediaMap outputMediaMap <- Map.lookup method methodMap
  InputMediaMap inputMediaMap <- lookupMediaMapBy mapAcceptMedia outputMediaMap outputAccept
  lookupMediaMapBy mapContentMedia inputMediaMap inputContentType

-- | Efficient representation of API to fetch routes
newtype ApiNormal a = ApiNormal (MethodMap (OutputMediaMap (InputMediaMap a)))
  deriving (Show, Eq, Functor)

-- | Mthod map
type MethodMap a = Map Method a

-- | filter by Content-Type header
newtype InputMediaMap a = InputMediaMap (MediaMap a)
  deriving (Show, Eq, Functor)

-- | filter by Accept header
newtype OutputMediaMap a = OutputMediaMap (MediaMap a)
  deriving (Show, Eq, Functor)

-- | Map by media type
data MediaMap a = MediaMap
  { mapValues :: [(MediaType, a)]
  , matchAll :: a
  }
  deriving (Show, Eq, Functor)

-- | Lookup value by media type key
lookupMediaMapBy :: ([(MediaType, a)] -> ByteString -> Maybe a) -> MediaMap a -> ByteString -> Maybe a
lookupMediaMapBy getter (MediaMap m matchAll) media
  | media == "*/*" = Just matchAll
  | otherwise = getter m media

{-| Path is a chain of elements which can be static types or capture.
There is @IsString@ instance which allows us to create paths from strings. Examples:

> "api/v1/foo" ==> Path [StaticPath "api", StaticPath "v1", StaticPath "foo"]
> "api/v1/*" ==> Path [StaticPath "api", StaticPath "v1", CapturePath "*"]
-}
newtype Path = Path {unPath :: [PathItem]}
  deriving newtype (Show, Eq, Ord, Semigroup, Monoid)

instance ToHttpApiData Path where
  toUrlPiece (Path ps) = Text.intercalate "/" $ fmap toUrlPiece ps

instance ToHttpApiData PathItem where
  toUrlPiece = \case
    StaticPath txt -> txt
    CapturePath txt -> "{" <> txt <> "}"

instance IsString Path where
  fromString =
    Path . fmap (replaceCapture . Text.pack) . filter (not . any isPathSeparator) . splitDirectories
    where
      replaceCapture :: Text -> PathItem
      replaceCapture path
        | Text.head path == '$' = CapturePath (Text.tail path)
        | path == "*" = CapturePath path
        | otherwise = StaticPath path

-- | Path can be a static item or capture with a name
data PathItem
  = StaticPath Text
  | CapturePath Text
  deriving (Show, Eq, Ord)

{-| Map of capture values extracted from path.
Keys are capture names.
-}
type CaptureMap = Map Text Text

-- | Find an api item by path. Also it accumulates capture map along the way.
getPath :: [Text] -> Api a -> Maybe (a, CaptureMap)
getPath mainPath = go mempty (filter (not . Text.null) mainPath)
  where
    go :: CaptureMap -> [Text] -> Api a -> Maybe (a, CaptureMap)
    go !captureMap path api =
      case path of
        [] -> case api of
          HandleRoute a -> Just (a, captureMap)
          Append a b -> maybe (go captureMap path b) Just (go captureMap path a)
          _ -> Nothing
        p : rest -> case api of
          WithPath template restApi -> goPath captureMap p rest template restApi
          Append a b -> maybe (go captureMap path b) Just (go captureMap path a)
          _ -> Nothing

    goPath !captureMap !pathHead !pathTail (Path !template) restApi =
      case template of
        [] -> go captureMap (pathHead : pathTail) restApi
        StaticPath apiHead : templateRest ->
          if pathHead == apiHead
            then goPathNext captureMap pathTail templateRest restApi
            else Nothing
        CapturePath name : templateRest ->
          let nextCaptureMap = Map.insert name pathHead captureMap
           in goPathNext nextCaptureMap pathTail templateRest restApi

    goPathNext !captureMap !pathTail !templateRest restApi =
      case templateRest of
        [] -> go captureMap pathTail restApi
        _ -> case pathTail of
          nextPathHead : nextPathTail -> goPath captureMap nextPathHead nextPathTail (Path templateRest) restApi
          [] -> Nothing

-- | Flattens API. Creates a flat list of paths and route handlers.
flatApi :: Api a -> [(Path, a)]
flatApi = go mempty
  where
    go prefix = \case
      Empty -> mempty
      Append a b -> go prefix a <> go prefix b
      WithPath path a -> go (prefix <> path) a
      HandleRoute a -> [(prefix, a)]

-- | Constructs API from flat list of pairs of paths and route handlers.
fromFlatApi :: [(Path, a)] -> Api a
fromFlatApi = foldMap (\(path, route) -> WithPath path (HandleRoute route))
