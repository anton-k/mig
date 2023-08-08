-- | Internal API description
module Mig.Internal.Api
  ( Api (..)
  , (/.)
  , (/$)
  , Path
  , ApiNormal (..)
  , toNormalApi
  , fromNormalApi
  , PathItem (..)
  , getPath
  , CaptureMap
  ) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String
import Mig.Internal.Info (MediaInputType (..), RouteInfo (..), RouteOutput (..))
import Network.HTTP.Types.Method
import Mig.Internal.Route qualified as Route
import Data.List qualified as List

data Api a
  = Append (Api a) (Api a)
  | Empty
  | WithPath Path (Api a)
  | Route a
  deriving (Functor, Foldable, Traversable, Show, Eq)

instance Monoid (Api a) where
  mempty = Empty

instance Semigroup (Api a) where
  (<>) = Append

filterApi :: (a -> Bool) -> Api a -> Api a
filterApi check = \case
  Route a -> if check a then Route a else Empty
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

toNormalApi :: Api (Route.Route m) -> ApiNormal (Route.Route m)
toNormalApi api = ApiNormal $ fmap toMethodApi $ case medias of
  [] -> MultiMedia mempty
  m : [] -> SingleMedia m api
  other -> MultiMedia $ filterEmpty $ Map.fromList $ fmap toMediaApi other
  where
    toMultiMedia m
      | Map.size m == 1 = case Map.toList m of
                            [(key, val)] -> SingleMedia key val
                            _ -> MultiMedia m
      | otherwise = MultiMedia m

    filterEmpty :: Map key (Api val) -> Map key (Api val)
    filterEmpty = Map.filter $ \case
      Empty -> False
      _ -> True

    medias = List.nub $ foldMap (\r -> [r.api.inputType]) api

    toMediaApi media = (media, filterApi (\r -> r.api.inputType == media) api)

    toMethodApi a = filterEmpty $ Map.fromList $ fmap
      (\m ->
          (m, filterApi (\r -> r.api.method == Just m) a)
      ) methods
      where
        methods = foldMap (\r -> maybe [] pure r.api.method) a

fromNormalApi :: Method -> MediaInputType -> ApiNormal a -> Maybe (Api a)
fromNormalApi method mediaType (ApiNormal mediaMap) = do
  methodMap <- lookupMedia mediaType mediaMap
  Map.lookup method methodMap

data ApiNormal a = ApiNormal (MediaMap (MethodMap (Api a)))
  deriving (Show, Eq, Functor)

type MethodMap a = Map Method a

data MediaMap a
  = SingleMedia MediaInputType a
  | MultiMedia (Map MediaInputType a)
  deriving (Functor, Show, Eq)

lookupMedia :: MediaInputType -> MediaMap a -> Maybe a
lookupMedia mediaType = \case
  SingleMedia ty a -> if ty == mediaType then Just a else Nothing
  MultiMedia as -> Map.lookup mediaType as


infixr 4 /.
infixr 4 /$

(/.) :: Text -> Api a -> Api a
(/.) path = \case
  WithPath rest a -> go rest a
  other -> go [] other
  where
    go rest a = WithPath (StaticPath path : rest) a

(/$) :: Text -> Api a -> Api a
(/$) path a = WithPath [CapturePath path] a

type Path = [PathItem]

data PathItem
  = StaticPath Text
  | CapturePath Text
  deriving (Show, Eq, Ord)

instance IsString Path where
  fromString = pure . StaticPath . fromString

type CaptureMap = Map Text Text

-- | Find an api item by path. Also it accumulates capture map along the way.
getPath :: [Text] -> Api a -> Maybe (a, CaptureMap)
getPath = go mempty
  where
    go :: CaptureMap -> [Text] -> Api a -> Maybe (a, CaptureMap)
    go !captureMap path api =
      case path of
        [] -> case api of
                Route a -> Just (a, captureMap)
                Append (Route a) _ -> Just (a, captureMap)
                _ -> Nothing
        p:rest -> case api of
                WithPath template restApi -> goPath captureMap p rest template restApi
                Append a b -> maybe (go captureMap path b) Just (go captureMap path a)
                _ -> Nothing

    goPath !captureMap !pathHead !pathTail !template restApi =
      case template of
        [] -> go captureMap (pathHead : pathTail) restApi
        StaticPath apiHead : templateRest ->
          if pathHead == apiHead
            then goPathNext captureMap pathTail templateRest restApi
            else Nothing

        CapturePath name : templateRest ->
          let nextCaptureMap = Map.insert name pathHead captureMap
          in  goPathNext nextCaptureMap pathTail templateRest restApi

    goPathNext !captureMap !pathTail !templateRest restApi =
      case templateRest of
        [] -> go captureMap pathTail restApi
        _ -> case pathTail of
              nextPathHead : nextPathTail -> goPath captureMap nextPathHead nextPathTail templateRest restApi
              [] -> Nothing


{-
-------------------------------------------------------------------------------------
-- client lib

class ToClient a where
  -- | converts to client function
  toClient :: Api RouteInfo -> a

  -- | how many routes client has
  clientArity :: Proxy a -> Int


data (:|) a b = a :| b

instance (ToClient a, ToClient b) => ToClient (a :| b) where
  toClient api = a :| b
    where
      (a, b) = toClient api

instance (ToClient a, ToClient b) => ToClient (a, b) where
  toClient api = (toClient (fromFlatApi apiA), toClient (fromFlatApi apiB))
    where
      (apiA, apiB) = splitAt (clientArity (Proxy @a)) (flatApi api)

instance (ToClient a, ToClient b, ToClient c) => ToClient (a, b, c) where
  toClient api =
    case toClient api of
      (a, (b, c)) -> (a, b, c)

instance (ToClient a, ToClient b, ToClient c, ToClient d) => ToClient (a, b, c, d) where
  toClient api =
    case toClient api of
      (a, (b, c, d)) -> (a, b, c, d)

flatApi :: Api a -> [(Path, a)]
flatApi = undefined

fromFlatApi :: [(Path, a)] -> Api a
fromFlatApi = undefined

atApi :: Api a -> Path -> Api a
atApi = undefined
-}
