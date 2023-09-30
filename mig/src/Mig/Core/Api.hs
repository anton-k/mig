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
) where

import Data.ByteString (ByteString)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Mig.Core.Info (RouteInfo (..), getInputType)
import Mig.Core.Route qualified as Route
import Network.HTTP.Media (mapContentMedia)
import Network.HTTP.Media.MediaType (MediaType)
import Network.HTTP.Types.Method
import System.FilePath
import Web.HttpApiData

data Api a
  = Append (Api a) (Api a)
  | Empty
  | WithPath Path (Api a)
  | HandleRoute a
  deriving (Functor, Foldable, Traversable, Show, Eq)

instance Monoid (Api a) where
  mempty = Empty

instance Semigroup (Api a) where
  (<>) = Append

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

toNormalApi :: forall m. Api (Route.Route m) -> ApiNormal (Route.Route m)
toNormalApi api = ApiNormal $ fmap toInputMediaMap (toMethodMap api)
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
                (m, filterApi (\r -> r.api.method == Just m) a)
            )
            methods
      where
        methods = foldMap (\r -> maybe [] pure r.api.method) a

    toInputMediaMap :: Api (Route.Route m) -> InputMediaMap (Api (Route.Route m))
    toInputMediaMap a =
      fmap toMediaApi medias
      where
        medias = List.nub $ foldMap (\r -> [getInputType r.api]) a

        toMediaApi media = (media, filterApi (\r -> getInputType r.api == media) a)

fromNormalApi :: Method -> ByteString -> ApiNormal a -> Maybe (Api a)
fromNormalApi method inputContentType (ApiNormal methodMap) = do
  inputMediaMap <- Map.lookup method methodMap
  mapContentMedia inputMediaMap inputContentType

data ApiNormal a = ApiNormal (MethodMap (InputMediaMap (Api a)))
  deriving (Show, Eq, Functor)

type MethodMap a = Map Method a

-- | filter by Content-Type header
type InputMediaMap a = [(MediaType, a)]

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
          let
            nextCaptureMap = Map.insert name pathHead captureMap
           in
            goPathNext nextCaptureMap pathTail templateRest restApi

    goPathNext !captureMap !pathTail !templateRest restApi =
      case templateRest of
        [] -> go captureMap pathTail restApi
        _ -> case pathTail of
          nextPathHead : nextPathTail -> goPath captureMap nextPathHead nextPathTail (Path templateRest) restApi
          [] -> Nothing

flatApi :: Api a -> [(Path, a)]
flatApi = go mempty
  where
    go prefix = \case
      Empty -> mempty
      Append a b -> go prefix a <> go prefix b
      WithPath path a -> go (prefix <> path) a
      HandleRoute a -> [(prefix, a)]

fromFlatApi :: [(Path, a)] -> Api a
fromFlatApi = foldMap (\(path, route) -> WithPath path (HandleRoute route))
