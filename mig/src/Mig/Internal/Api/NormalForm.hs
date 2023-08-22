module Mig.Internal.Api.NormalForm (
  ApiNormal (..),
  getPath,
  toApiNormal,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Mig.Internal.Api qualified as Api

data ApiNormal a
  = Empty
  | Route a
  | StaticPrefix [Text] (ApiNormal a)
  | CapturePrefix Text (ApiNormal a)
  | Switch (PathSwitch a) (Maybe (CapureSwitch a)) (Maybe a)

type PathSwitch a = Map Text (ApiNormal a)

data CapureSwitch a = CaptureSwitch Text (ApiNormal a)

type CaptureMap = Map Text Text

getPath :: [Text] -> ApiNormal a -> Maybe (a, CaptureMap)
getPath = go mempty
  where
    go :: CaptureMap -> [Text] -> ApiNormal a -> Maybe (a, CaptureMap)
    go !captureMap !path !api =
      case path of
        [] ->
          case api of
            Route a -> Just (a, captureMap)
            Switch _ _ (Just a) -> Just (a, captureMap)
            _ -> Nothing
        hd : tl ->
          case api of
            Empty -> Nothing
            Route _ -> Nothing
            StaticPrefix prefix a -> onStaticPrefix captureMap hd tl prefix a
            CapturePrefix name a -> go (Map.insert name hd captureMap) tl a
            Switch pathSwitch mCaptureSwitch _ -> onSwitch captureMap hd tl pathSwitch mCaptureSwitch

    onStaticPrefix !captureMap hd tl prefix api = case prefix of
      [] -> go captureMap (hd : tl) api
      prefixHd : prefixTl ->
        if (hd == prefixHd)
          then case tl of
            [] ->
              case prefixTl of
                [] -> go captureMap [] api
                _ -> Nothing
            nextHd : nextTl -> onStaticPrefix captureMap nextHd nextTl prefixTl api
          else Nothing

    onSwitch captureMap hd tl pathSwitch mCaptureSwitch =
      case Map.lookup hd pathSwitch of
        Just api -> go captureMap tl api
        Nothing -> do
          CaptureSwitch name api <- mCaptureSwitch
          go (Map.insert name hd captureMap) tl api

toApiNormal :: Api.Api a -> ApiNormal a
toApiNormal = \case
  Api.Empty -> Empty
  Api.Route a -> Route a
  Api.WithPath path a -> case parsePath path of
    Nothing -> toApiNormal a
    Just (prefix, rest) ->
      let
        nextApi = toApiNormal (Api.WithPath rest a)
       in
        case prefix of
          StaticPath ts -> StaticPrefix ts nextApi
          CapturePath name -> CapturePrefix name nextApi
  _ -> undefined

data ParsePath
  = StaticPath [Text]
  | CapturePath Text

parsePath :: Api.Path -> Maybe (ParsePath, Api.Path)
parsePath = go []
  where
    go result (Api.Path path) = case path of
      [] -> if null result then Nothing else Just (StaticPath $ reverse result, mempty)
      Api.CapturePath name : rest ->
        if null result then Just (CapturePath name, Api.Path rest) else Just (StaticPath $ reverse result, Api.Path $ Api.CapturePath name : rest)
      Api.StaticPath hd : rest -> go (hd : result) (Api.Path rest)
