-- | Generic response
module Mig.Core.Class.Response (
  Resp (..),
  RespOr (..),
  IsResp (..),
  badReq,
  internalServerError,
  notImplemented,
  redirect,
  setHeader,
) where

import Data.Bifunctor
import Data.ByteString.Lazy qualified as BL
import Data.Kind
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Network.HTTP.Media.RenderHeader (RenderHeader (..))
import Network.HTTP.Types.Header (HeaderName, ResponseHeaders)
import Network.HTTP.Types.Status (Status, internalServerError500, notImplemented501, ok200, status302, status400)
import Web.HttpApiData

import Mig.Core.Class.MediaType (MediaType, ToMediaType (..), ToRespBody (..))
import Mig.Core.Types.Http (Response, ResponseBody (..), noContentResponse)
import Mig.Core.Types.Http qualified as Response (Response (..))
import Mig.Core.Types.Http qualified as Types

data Resp media a = Resp
  { status :: Status
  , headers :: ResponseHeaders
  , body :: Maybe a
  }
  deriving (Show, Functor)

okResp :: a -> Resp ty a
okResp = Resp ok200 [] . Just

newtype RespOr ty err a = RespOr {unRespOr :: Either (Resp ty err) (Resp ty a)}

-------------------------------------------------------------------------------------
-- response class

class IsResp a where
  type RespBody a :: Type
  type RespError a :: Type

  ok :: RespBody a -> a
  bad :: Status -> RespError a -> a
  noContent :: Status -> a
  addHeaders :: ResponseHeaders -> a -> a
  setStatus :: Status -> a -> a

  setMedia :: MediaType -> a -> a
  setMedia media = addHeaders [("Content-Type", renderHeader media)]

  getMedia :: MediaType

  toResponse :: a -> Response

-- | Set header for response
setHeader :: (IsResp a, ToHttpApiData h) => HeaderName -> h -> a -> a
setHeader name val = addHeaders [(name, toHeader val)]

instance (ToRespBody ty a) => IsResp (Resp ty a) where
  type RespBody (Resp ty a) = a
  type RespError (Resp ty a) = a

  ok = Resp ok200 [] . Just
  bad status = Resp status [] . Just
  addHeaders hs x = x{headers = x.headers <> hs}
  noContent st = Resp st [] Nothing
  setStatus st x = x{status = st}
  getMedia = toMediaType @ty

  toResponse a = Response.Response a.status headers body
    where
      media = toMediaType @ty
      headers = a.headers <> [("Content-Type", renderHeader media)]
      body = RawResp media (maybe "" (toRespBody @ty) a.body)

instance IsResp Response where
  type RespBody Response = BL.ByteString
  type RespError Response = BL.ByteString

  ok = Response.Response ok200 [] . RawResp "*/*"
  bad st = Response.Response st [] . RawResp "*/*"
  addHeaders hs x = x{Response.headers = x.headers <> hs}
  noContent = noContentResponse
  setStatus st x = x{Response.status = st}
  getMedia = "*/*"

  toResponse = id

  setMedia media = addHeaders [("Content-Type", renderHeader media)] . updateBody
    where
      updateBody response = response{Response.body = setBodyMedia response.body}

      setBodyMedia = \case
        RawResp _ content -> RawResp media content
        other -> other

instance (ToRespBody ty err, ToRespBody ty a) => IsResp (RespOr ty err a) where
  type RespBody (RespOr ty err a) = a
  type RespError (RespOr ty err a) = err

  ok = RespOr . Right . okResp
  bad status = RespOr . Left . bad status
  addHeaders hs = RespOr . bimap (addHeaders hs) (addHeaders hs) . unRespOr
  noContent st = RespOr $ Right (noContent st)
  setStatus st = RespOr . bimap (setStatus st) (setStatus st) . unRespOr
  getMedia = toMediaType @ty

  toResponse = either toResponse toResponse . unRespOr

badReq :: (IsResp a) => RespError a -> a
badReq = bad status400

internalServerError :: (IsResp a) => RespError a -> a
internalServerError = bad internalServerError500

notImplemented :: (IsResp a) => RespError a -> a
notImplemented = bad notImplemented501

redirect :: (IsResp a) => Text -> a
redirect url = addHeaders [("Location", Text.encodeUtf8 url)] $ noContent status302

-------------------------------------------------------------------------------------
