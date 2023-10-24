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
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Mig.Core.Api (Api (..), Path (..), PathItem (..))

type CaptureMap = Map Text Text

data TreeApi a
  = WithStaticPath [Text] (TreeApi a)
  | WithCapturePath [Text] (TreeApi a)
  | SwitchApi (Maybe a) (Map Text (TreeApi a)) (Maybe (CaptureCase a))

data CaptureCase a = CaptureCase
  { name :: Text
  , api :: TreeApi a
  }

-- | Get a route by path, also extracts capture map
getPath :: [Text] -> TreeApi a -> Maybe (CaptureMap, a)
getPath = go mempty
  where
    go :: CaptureMap -> [Text] -> TreeApi a -> Maybe (CaptureMap, a)
    go !captures !path !api =
      case path of
        [] ->
          case api of
            SwitchApi (Just result) _ _ -> Just (captures, result)
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
-- convert to normal form

toTreeApi :: Api a -> TreeApi a
toTreeApi = \case
  Empty -> SwitchApi Nothing mempty Nothing
  WithPath path subApi -> case fromPathPrefix path of
    Nothing -> toTreeApi subApi
    Just prefix -> case prefix of
      StaticPrefix ps rest -> WithStaticPath ps (toTreeApi $ WithPath rest subApi)
      CapturePrefix ps rest -> WithCapturePath ps (toTreeApi $ WithPath rest subApi)
  HandleRoute a -> SwitchApi (Just a) mempty Nothing
  Append a b -> fromAlts $ orderAppends (collectAppends a <> collectAppends b)

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
  SwitchApi alts.route (fmap toTreeApi $ Map.fromList alts.appends) (fmap toCaptureCase alts.capture)
  where
    toCaptureCase (name, api) = CaptureCase name (toTreeApi api)

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
