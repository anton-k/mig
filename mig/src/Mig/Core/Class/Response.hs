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
  SetCookie (..),
  defCookie,
  setCookie,
) where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Kind
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time
import Network.HTTP.Media.RenderHeader (RenderHeader (..))
import Network.HTTP.Types.Header (HeaderName, ResponseHeaders, hSetCookie)
import Network.HTTP.Types.Status (Status, internalServerError500, notImplemented501, ok200, status302, status400)
import Web.HttpApiData
import Web.Internal.FormUrlEncoded

import Mig.Core.Class.MediaType (AnyMedia, MediaType, ToMediaType (..), ToRespBody (..))
import Mig.Core.Types.Http (Response, ResponseBody (..), noContentResponse)
import Mig.Core.Types.Http qualified as Response (Response (..))
import Mig.Core.Types.Http qualified as Types

-- | Response with info on the media-type encoded as type.
data Resp (media :: Type) a = Resp
  { status :: Status
  -- ^ response status
  , headers :: ResponseHeaders
  -- ^ response headers
  , body :: Maybe a
  -- ^ response body. Nothing means "no content" in the body
  }
  deriving (Show, Eq, Functor)

-- | Response that can contain an error. The error is represented with left case of an @Either@-type.
newtype RespOr ty err a = RespOr {unRespOr :: Either (Resp ty err) (Resp ty a)}
  deriving (Show, Eq, Functor)

-------------------------------------------------------------------------------------
-- response class

{-| Values that can be converted to low-level response.

The repsonse value is usually one of two cases:

* @Resp a@ -- for routes which always produce a value

* @RespOr err a@ - for routes that can also produce an error or value.

* @Response@ - low-level HTTP-response.
-}
class IsResp a where
  -- | the type of response body value
  type RespBody a :: Type

  -- | the type of an error
  type RespError a :: Type

  -- | the media tpye of resp
  type RespMedia a :: Type

  -- | Returns valid repsonse with 200 status
  ok :: RespBody a -> a

  -- | Returns an error with given status
  bad :: Status -> RespError a -> a

  -- | response with no content
  noContent :: Status -> a

  -- | Add some header to the response
  addHeaders :: ResponseHeaders -> a -> a

  -- | Get response headers
  getHeaders :: a -> ResponseHeaders

  -- | Sets repsonse status
  setStatus :: Status -> a -> a

  -- | Get response body
  getRespBody :: a -> Maybe (RespBody a)

  -- | Get response error
  getRespError :: a -> Maybe (RespError a)

  -- | Get response status
  getStatus :: a -> Status

  -- | Set the media type of the response
  setMedia :: MediaType -> a -> a
  setMedia media = addHeaders [("Content-Type", renderHeader media)]

  -- | Reads the media type by response type
  getMedia :: MediaType

  -- | Converts value to low-level response
  toResponse :: a -> Response

-- | Set header for response
setHeader :: (IsResp a, ToHttpApiData h) => HeaderName -> h -> a -> a
setHeader name val = addHeaders [(name, toHeader val)]

instance (ToRespBody ty a) => IsResp (Resp ty a) where
  type RespBody (Resp ty a) = a
  type RespError (Resp ty a) = a
  type RespMedia (Resp ty a) = ty

  ok = Resp ok200 [] . Just
  bad status = Resp status [] . Just
  addHeaders hs x = x{headers = x.headers <> hs}
  getHeaders x = x.headers
  noContent st = Resp st [] Nothing
  setStatus st x = x{status = st}
  getStatus x = x.status
  getMedia = toMediaType @ty
  getRespBody x = x.body
  getRespError x
    | x.status == ok200 = Nothing
    | otherwise = x.body

  toResponse a = Response.Response a.status headers body
    where
      media = toMediaType @ty
      headers = a.headers <> [("Content-Type", renderHeader media)]
      body = RawResp media (maybe "" (toRespBody @ty) a.body)

instance IsResp Response where
  type RespBody Response = BL.ByteString
  type RespError Response = BL.ByteString
  type RespMedia Response = AnyMedia

  ok = Response.Response ok200 [] . RawResp "*/*"
  bad st = Response.Response st [] . RawResp "*/*"
  addHeaders hs x = x{Response.headers = x.headers <> hs}
  noContent = noContentResponse
  setStatus st x = x{Response.status = st}
  getMedia = "*/*"
  getStatus x = x.status
  getHeaders x = x.headers
  getRespBody x = case x.body of
    RawResp _ res -> Just res
    _ -> Nothing
  getRespError x
    | x.status == ok200 = Nothing
    | otherwise = getRespBody x

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
  type RespMedia (RespOr ty err a) = ty

  ok = RespOr . Right . Resp ok200 [] . Just
  bad status = RespOr . Left . bad status
  addHeaders hs = RespOr . bimap (addHeaders hs) (addHeaders hs) . unRespOr
  noContent st = RespOr $ Right (noContent st)
  setStatus st = RespOr . bimap (setStatus st) (setStatus st) . unRespOr
  getMedia = toMediaType @ty
  getStatus (RespOr x) = either (.status) (.status) x
  getHeaders (RespOr x) = either (.headers) (headers) x
  getRespBody (RespOr x) = either (const Nothing) (.body) x
  getRespError (RespOr x) = either (.body) (const Nothing) x

  toResponse = either toResponse toResponse . unRespOr

-- | Bad request. The @bad@ response with 400 status.
badReq :: (IsResp a) => RespError a -> a
badReq = bad status400

-- | Internal server error. The @bad@ response with 500 status.
internalServerError :: (IsResp a) => RespError a -> a
internalServerError = bad internalServerError500

-- | Not implemented route. The @bad@ response with 501 status.
notImplemented :: (IsResp a) => RespError a -> a
notImplemented = bad notImplemented501

-- | Redirect to url. It is @bad@ response with 302 status and set header of "Location" to a given URL.
redirect :: (IsResp a) => Text -> a
redirect url = addHeaders [("Location", Text.encodeUtf8 url)] $ noContent status302

-- | Set cookie as http header from form url encoded value
setCookie :: (ToForm cookie, IsResp resp) => SetCookie cookie -> resp -> resp
setCookie cookie = addHeaders [(hSetCookie, renderSetCookie cookie)]

{-| Set cookie params. For explanation see an article
<https://web.archive.org/web/20170122122852/https://www.nczonline.net/blog/2009/05/05/http-cookies-explained/>
-}
data SetCookie a = SetCookie
  { cookie :: a
  , expires :: Maybe UTCTime
  , domain :: Maybe Text
  , path :: Maybe Text
  , secure :: Bool
  , httpOnly :: Bool
  }
  deriving (Show, Eq)

renderSetCookie :: (ToForm a) => SetCookie a -> ByteString
renderSetCookie value =
  mconcat $
    (BL.toStrict $ urlEncodeForm $ toForm value.cookie)
      : addColons
        ( catMaybes
            [ param "expires" . fmtTime <$> value.expires
            , param "domain" <$> value.domain
            , param "path" <$> value.path
            , flag "secure" value.secure
            , flag "httpOnly" value.httpOnly
            ]
        )
  where
    addColons xs
      | null xs = []
      | otherwise = ";" : List.intersperse ";" xs

    param name v = Text.encodeUtf8 $ name <> v

    flag name = \case
      True -> Just name
      False -> Nothing

    fmtTime :: UTCTime -> Text
    fmtTime = Text.pack . formatTime defaultTimeLocale expiresFormat

    expiresFormat :: String
    expiresFormat = "%a, %d-%b-%Y %X GMT"

-- | Default cookie which sets only the cookie itself.
defCookie :: a -> SetCookie a
defCookie val =
  SetCookie
    { cookie = val
    , expires = Nothing
    , domain = Nothing
    , path = Nothing
    , secure = False
    , httpOnly = False
    }
