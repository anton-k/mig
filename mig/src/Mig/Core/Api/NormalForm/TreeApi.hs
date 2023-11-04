{-| Normal form where on handler search API is
traversed in tree like facion without retraversal of the paths.
-}
module Mig.Core.Api.NormalForm.TreeApi (
  TreeApi (..),
  CaptureCase (..),
  getPath,
  toTreeApi,
) where

import Data.List qualified as List
import Data.List.Extra qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

import Mig.Core.Api (Api (..), Path (..), PathItem (..))

type CaptureMap = Map Text Text

{-| This form of API encodes path switch points as Map's so
it does not retraverse the routes and can find the right
branch on switch. In the plain api it tries the routes one by one
until it finds the right one.
-}
data TreeApi a
  = WithStaticPath [Text] (TreeApi a)
  | WithCapturePath [Text] (TreeApi a)
  | SwitchApi (Maybe a) (Map Text (TreeApi a)) (Maybe (CaptureCase a))
  deriving (Eq, Show, Functor)

-- | Capture case alternative
data CaptureCase a = CaptureCase
  { name :: Text
  , api :: TreeApi a
  }
  deriving (Eq, Show, Functor)

-- | Get a route by path, also extracts capture map
getPath :: [Text] -> TreeApi a -> Maybe (a, CaptureMap)
getPath mainPath = go mempty (filter (not . Text.null) mainPath)
  where
    go :: CaptureMap -> [Text] -> TreeApi a -> Maybe (a, CaptureMap)
    go !captures !path !api =
      case path of
        [] ->
          case api of
            SwitchApi (Just result) _ _ -> Just (result, captures)
            _ -> Nothing
        headPath : tailPath ->
          case api of
            WithStaticPath static subApi -> onStaticPath captures (headPath : tailPath) static subApi
            WithCapturePath names subApi -> onCapturePath captures (headPath : tailPath) names subApi
            SwitchApi _ alternatives mCapture -> onSwitch captures headPath tailPath alternatives mCapture

    onStaticPath captures pathQuery staticPath subApi = do
      rest <- checkPrefix staticPath pathQuery
      go captures rest subApi

    onCapturePath captures pathQuery names subApi = do
      (nextCaptures, nextPath) <- accumCapture captures names pathQuery
      go nextCaptures nextPath subApi

    onSwitch captures headPath tailPath alternatives mCapture =
      case Map.lookup headPath alternatives of
        Just subApi -> go captures tailPath subApi
        Nothing -> do
          captureCase <- mCapture
          go (Map.insert captureCase.name headPath captures) tailPath captureCase.api

checkPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
checkPrefix (a : as) (b : bs)
  | a == b = checkPrefix as bs
  | otherwise = Nothing
checkPrefix [] b = Just b
checkPrefix _ _ = Nothing

accumCapture :: CaptureMap -> [Text] -> [Text] -> Maybe (CaptureMap, [Text])
accumCapture !captures !names !path =
  case names of
    [] -> Just (captures, path)
    name : rest ->
      case path of
        pathHead : pathTail -> accumCapture (Map.insert name pathHead captures) rest pathTail
        [] -> Nothing

-------------------------------------------------------------------------------------

-- | Converts api to tree normal form
toTreeApi :: Api a -> TreeApi a
toTreeApi =
  joinPaths . \case
    Empty -> SwitchApi Nothing mempty Nothing
    WithPath path subApi -> case fromPathPrefix path of
      Nothing -> toTreeApi subApi
      Just prefix -> case prefix of
        StaticPrefix ps rest -> WithStaticPath ps (toTreeApi $ WithPath rest subApi)
        CapturePrefix ps rest -> WithCapturePath ps (toTreeApi $ WithPath rest subApi)
    HandleRoute a -> SwitchApi (Just a) mempty Nothing
    Append a b -> fromAlts $ orderAppends (collectAppends a <> collectAppends b)

joinPaths :: TreeApi a -> TreeApi a
joinPaths = \case
  SwitchApi mRoute alts mCapture -> SwitchApi mRoute (fmap joinPaths alts) (fmap joinCapturePaths mCapture)
  WithStaticPath pathA (WithStaticPath pathB subApi) -> joinPaths (WithStaticPath (pathA ++ pathB) subApi)
  WithCapturePath namesA (WithCapturePath namesB subApi) -> joinPaths (WithCapturePath (namesA ++ namesB) subApi)
  WithStaticPath path subApi -> WithStaticPath path (joinPaths subApi)
  WithCapturePath names subApi -> WithCapturePath names (joinPaths subApi)
  where
    joinCapturePaths x = x{api = joinPaths x.api}

data Alts a = Alts
  { appends :: [(Text, Api a)]
  , capture :: Maybe (Text, Api a)
  , route :: Maybe a
  }

data AppendItem a
  = StaticAppend Text (Api a)
  | RouteAppend a
  | CaptureAppend Text (Api a)

collectAppends :: Api a -> [AppendItem a]
collectAppends = \case
  Empty -> []
  HandleRoute a -> [RouteAppend a]
  Append a b -> collectAppends a <> collectAppends b
  WithPath (Path items) subApi -> case items of
    [] -> collectAppends subApi
    StaticPath item : [] -> pure $ StaticAppend item subApi
    StaticPath item : rest -> pure $ StaticAppend item (WithPath (Path rest) subApi)
    CapturePath item : [] -> pure $ CaptureAppend item subApi
    CapturePath item : rest -> pure $ CaptureAppend item (WithPath (Path rest) subApi)

orderAppends :: [AppendItem a] -> Alts a
orderAppends items =
  Alts
    { appends = mapMaybe toAppend items
    , capture = List.firstJust toCapture items
    , route = List.firstJust toRoute items
    }
  where
    toAppend = \case
      StaticAppend name api -> Just (name, api)
      _ -> Nothing

    toCapture = \case
      CaptureAppend name api -> Just (name, api)
      _ -> Nothing

    toRoute = \case
      RouteAppend route -> Just route
      _ -> Nothing

fromAlts :: Alts a -> TreeApi a
fromAlts alts =
  case getStaticSingleton of
    Just (path, subApi) -> WithStaticPath [path] (toTreeApi subApi)
    Nothing ->
      case getCaptureSingleton of
        Just (names, subApi) -> WithCapturePath [names] (toTreeApi subApi)
        Nothing -> SwitchApi alts.route (fmap toTreeApi $ Map.fromList alts.appends) (fmap toCaptureCase alts.capture)
  where
    toCaptureCase (name, api) = CaptureCase name (toTreeApi api)

    getStaticSingleton =
      case alts.appends of
        [(path, subApi)] | isNothing alts.route && isNothing alts.capture -> Just (path, subApi)
        _ -> Nothing

    getCaptureSingleton =
      case alts.capture of
        Just (name, subApi) | isNothing alts.route && null alts.appends -> Just (name, subApi)
        _ -> Nothing

data PathPrefix
  = StaticPrefix [Text] Path
  | CapturePrefix [Text] Path

fromPathPrefix :: Path -> Maybe PathPrefix
fromPathPrefix (Path items) = case items of
  [] -> Nothing
  StaticPath item : rest -> Just (accumStatics [item] rest)
  CapturePath item : rest -> Just (accumCaptures [item] rest)
  where
    accumStatics res rest =
      case rest of
        StaticPath item : nextRest -> accumStatics (item : res) nextRest
        _ -> StaticPrefix (List.reverse res) (Path rest)

    accumCaptures res rest =
      case rest of
        CapturePath item : nextRest -> accumCaptures (item : res) nextRest
        _ -> CapturePrefix (List.reverse res) (Path rest)
