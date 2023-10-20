{-# LANGUAGE UndecidableInstances #-}

-- | Server definition
module Mig.Core.Server (
  Server (..),
  mapServerFun,
  mapResponse,
  fromServer,
  fillCaptures,
  addTag,
  setDescription,
  setSummary,
  mapRouteInfo,
  staticFiles,
  describeInputs,
  atPath,
  filterPath,
  getServerPaths,
  addPathLink,
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Safe (atMay, headMay)
import System.FilePath (takeExtension)
import Web.HttpApiData

import Mig.Core.Api (Api, fromNormalApi, toNormalApi)
import Mig.Core.Api qualified as Api
import Mig.Core.Class.MediaType
import Mig.Core.Class.Response (IsResp (..), Resp (..))
import Mig.Core.Class.Route
import Mig.Core.ServerFun (ServerFun)
import Mig.Core.Types (Request (..), Response, setContent)
import Mig.Core.Types.Info (RouteInfo (..), RouteInput (..), describeInfoInputs, setOutputMedia)
import Mig.Core.Types.Info qualified as Describe (Describe (..))
import Mig.Core.Types.Route

{-| Server type. It is a function fron request to response.
Some servers does not return valid value. We use it to find right path.

Example:

> server :: Server IO
> server =
>   "api" /. "v1" /.
>      mconcat
>        [ "foo" /. handleFoo
>        , "bar" /. handleBar
>        ]
>
> handleFoo :: Query "name" Int -> Get IO (Resp Json Text)
> handleBar :: Post Json IO Text

Note that server is monoid and it can be constructed with Monoid functions and
path constructor @(/.)@. To pass inputs for handler we can use special newtype wrappers:

* @Query@ - for required query parameters
* @Optional@ - for optional query parameters
* @QueryFlag@ - for boolean query flags
* @Capture@ - for parsing elements of URI
* @Header@ - for parsing headers
* @OptionalHeader@ - for parsing optional headers
* @Body@ - fot request-body input

and other request types.

To distinguish by HTTP-method we use corresponding constructors: Get, Post, Put, etc.
Let's discuss the structure of the constructor. Let's take Get for example:

> type Get ty m a = Send GET ty m a
> newtype Send ty m a = Send (m a)

 Let's look at the arguments of he type

* @ty@ - media-type of the response. it can be: Text, Html, Json, OctetStream, AnyMedia, FormUrlEncoded
* @m@ - underlying server monad
* @a@ - response type. It should be convertible to the type of the response (see @IsResp@ class).
-}
newtype Server m = Server {unServer :: Api (Route m)}
  deriving newtype (Semigroup, Monoid)

-- | Applies server function to all routes
mapServerFun :: (ServerFun m -> ServerFun n) -> Server m -> Server n
mapServerFun f (Server server) = Server $ fmap (\x -> Route x.info (f x.run)) server

-- | Mapps response of the server
mapResponse :: (Functor m) => (Response -> Response) -> Server m -> Server m
mapResponse f = mapServerFun $ \fun -> fmap (fmap f) . fun

{-| Converts server to server function. Server function can be used to implement low-level handlers
in various server-libraries.
-}
fromServer :: (Monad m) => Server m -> ServerFun m
fromServer (Server server) = \req -> do
  case getRoute req of
    Just (routes, captureMap) -> routes.run req{capture = captureMap}
    Nothing -> pure Nothing
  where
    serverNormal = toNormalApi (fillCaptures server)

    getRoute req = do
      api <- fromNormalApi req.method (getMediaType "Accept" req) (getMediaType "Content-Type" req) serverNormal
      Api.getPath req.path api

    getMediaType name req = fromMaybe "*/*" $ Map.lookup name req.headers

{-| Substitutes all stars * for corresponding names in captures
if there are more captures in the route than in the path it adds
additional captures from the route to the path
-}
fillCaptures :: Api (Route m) -> Api (Route m)
fillCaptures = go mempty 0
  where
    go pathSoFar n = \case
      Api.WithPath path api ->
        let (pathNext, m) = goPath (pathSoFar <> path) n path api
         in Api.WithPath pathNext (go (pathSoFar <> path) m api)
      Api.Append a b -> Api.Append (go pathSoFar n a) (go pathSoFar n b)
      Api.Empty -> Api.Empty
      Api.HandleRoute a -> goRoute pathSoFar n a

    goPath :: Api.Path -> Int -> Api.Path -> Api (Route m) -> (Api.Path, Int)
    goPath pathSoFar n (Api.Path path) api = case path of
      [] -> (Api.Path path, n)
      Api.CapturePath "*" : rest ->
        let (nextRest, m) = goPath pathSoFar (n + 1) (Api.Path rest) api
         in case getCaptureName n api of
              Just name -> (Api.Path [Api.CapturePath name] <> nextRest, m)
              Nothing -> error $ "No capture argument for start in path " <> Text.unpack (toUrlPiece pathSoFar) <> " at the index: " <> show n
      a : rest ->
        let (nextRest, m) = goPath pathSoFar n (Api.Path rest) api
         in (Api.Path [a] <> nextRest, m)

    goRoute pathSoFar pathCaptureCount route
      | missingCapturesCount > 0 = withMissingCaptures pathSoFar [pathCaptureCount .. routeCaptureCount - 1] (Api.HandleRoute route)
      | otherwise = Api.HandleRoute route
      where
        missingCapturesCount = routeCaptureCount - pathCaptureCount

        routeCaptureCount = captureCount route.info

    withMissingCaptures pathSoFar indexes route =
      Api.WithPath (Api.Path $ Api.CapturePath <$> names) route
      where
        names =
          fromMaybe (error $ "Not enough captures at path: " <> Text.unpack (toUrlPiece pathSoFar)) $
            mapM (\index -> getCaptureName index route) indexes

    captureCount routeInfo = List.foldl' count 0 routeInfo.inputs
      where
        count res inp = case inp.content of
          CaptureInput _ _ -> 1 + res
          _ -> res

getCaptureName :: Int -> Api (Route m) -> Maybe Text
getCaptureName index = \case
  Api.Append a _b -> rec a
  Api.Empty -> Nothing
  Api.WithPath _ a -> rec a
  Api.HandleRoute a -> mapMaybe (toCapture . Describe.content) a.info.inputs `atMay` index
  where
    rec = getCaptureName index

    toCapture :: RouteInput -> Maybe Text
    toCapture = \case
      CaptureInput name _ -> Just name
      _ -> Nothing

-- | Adds tag to the route
addTag :: Text -> Server m -> Server m
addTag tag = mapRouteInfo (insertTag tag)

-- | Sets description of the route
setDescription :: Text -> Server m -> Server m
setDescription desc = mapRouteInfo $ \info -> info{description = desc}

-- | Sets summary of the route
setSummary :: Text -> Server m -> Server m
setSummary val = mapRouteInfo $ \info -> info{summary = val}

-- | Maps over route API-information
mapRouteInfo :: (RouteInfo -> RouteInfo) -> Server m -> Server m
mapRouteInfo f (Server srv) = Server $ fmap (\route -> route{info = f route.info}) srv

-- | Adds OpenApi tag to the route
insertTag :: Text -> RouteInfo -> RouteInfo
insertTag tag info = info{tags = tag : info.tags}

{-| Appends descriptiton for the inputs. It passes pairs for @(input-name, input-description)@.
special name request-body is dedicated to request body input
nd raw-input is dedicated to raw input
-}
describeInputs :: [(Text, Text)] -> Server m -> Server m
describeInputs descs = mapRouteInfo (describeInfoInputs descs)

{-| Serves static files. The file path is a path to where to server the file.
The media-type is derived from the extension. There is a special case if we need
to server the file from the rooot of the server we can omit everything from the path
but keep extension. Otherwise it is not able to derive the media type.

It is convenient to use it with function @embedRecursiveDir@ from the library @file-embed@ or @file-embed-lzma@.
-}
staticFiles :: forall m. (MonadIO m) => [(FilePath, ByteString)] -> Server m
staticFiles files =
  Server $ foldMap (uncurry serveFile) files
  where
    serveFile path content =
      unServer . mapRouteInfo (setOutputMedia media) . Server $
        ( if headMay path == Just '.'
            then id
            else ((fromString path) `Api.WithPath`)
        )
          (Api.HandleRoute (toRoute (getFile media content)))
      where
        media = getMediaType path

    getFile :: MediaType -> ByteString -> Get m (Resp AnyMedia BL.ByteString)
    getFile ty fileContent =
      Send $
        pure $
          addHeaders (setContent ty) $
            ok $
              BL.fromStrict fileContent

    getMediaType :: FilePath -> MediaType
    getMediaType path =
      fromMaybe "application/octet-stream" $
        Map.lookup (takeExtension path) extToMimeMap

extToMimeMap :: Map String MediaType
extToMimeMap =
  Map.fromList
    [ (".aac", "audio/aac") -- AAC audio
    , (".abw", "application/x-abiword") -- AbiWord document
    , (".arc", "application/x-freearc") -- Archive document (multiple files embedded)
    , (".avif", "image/avif") -- AVIF image
    , (".avi", "video/x-msvideo") -- 	AVI: Audio Video Interleave
    , (".azw", "application/vnd.amazon.ebook") -- 	Amazon Kindle eBook format
    , (".bin", "application/octet-stream") -- 	Any kind of binary data
    , (".bmp", "image/bmp") -- 	Windows OS/2 Bitmap Graphics
    , (".bz", "application/x-bzip") -- 	BZip archive
    , (".bz2", "application/x-bzip2") -- 	BZip2 archive
    , (".cda", "application/x-cdf") -- CD audio
    , (".csh", "application/x-csh") -- 	C-Shell script
    , (".css", "text/css") -- 	Cascading Style Sheets (CSS)
    , (".csv", "text/csv") -- 	Comma-separated values (CSV)
    , (".doc", "application/msword") -- 	Microsoft Word
    , (".docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document") -- 	Microsoft Word (OpenXML)
    , (".eot", "application/vnd.ms-fontobject") -- 	MS Embedded OpenType fonts
    , (".epub", "application/epub+zip") -- 	Electronic publication (EPUB)
    , (".gz", "application/gzip") -- 	GZip Compressed Archive
    , (".gif", "image/gif") -- 	Graphics Interchange Format (GIF)
    , (".htm", "text/html") -- , .html	HyperText Markup Language (HTML)
    , (".ico", "image/vnd.microsoft.icon") -- 	Icon format
    , (".ics", "text/calendar") -- 	iCalendar format
    , (".jar", "application/java-archive") -- 	Java Archive (JAR)
    , (".jpeg", "image/jpeg") -- JPEG images
    , (".jpg", "image/jpeg") -- JPEG images
    , (".js", "text/javascript") -- 	JavaScript	 (Specifications: HTML and RFC 9239)
    , (".json", "application/json") -- 	JSON format
    , (".jsonld", "application/ld+json") -- 	JSON-LD format
    , (".mid", "audio/midi") -- 	Musical Instrument Digital Interface (MIDI)	, audio/x-midi
    , (".midi", "audio/midi") -- 	Musical Instrument Digital Interface (MIDI)	, audio/x-midi
    , (".mjs", "text/javascript") -- 	JavaScript module
    , (".mp3", "audio/mpeg") -- 	MP3 audio
    , (".mp4", "video/mp4") -- MP4 video
    , (".mpeg", "video/mpeg") -- 	MPEG Video
    , (".mpkg", "application/vnd.apple.installer+xml") -- 	Apple Installer Package
    , (".odp", "application/vnd.oasis.opendocument.presentation") -- 	OpenDocument presentation document
    , (".ods", "application/vnd.oasis.opendocument.spreadsheet") -- 	OpenDocument spreadsheet document
    , (".odt", "application/vnd.oasis.opendocument.text") -- 	OpenDocument text document
    , (".oga", "audio/ogg") -- 	OGG audio
    , (".ogv", "video/ogg") -- 	OGG video
    , (".ogx", "application/ogg") -- 	OGG
    , (".opus", "audio/opus") -- Opus audio
    , (".otf", "font/otf") -- 	OpenType font
    , (".png", "image/png") -- 	Portable Network Graphics
    , (".pdf", "application/pdf") -- 	Adobe Portable Document Format (PDF)
    , (".php", "application/x-httpd-php") -- 	Hypertext Preprocessor (Personal Home Page)
    , (".ppt", "application/vnd.ms-powerpoint") -- 	Microsoft PowerPoint
    , (".pptx", "application/vnd.openxmlformats-officedocument.presentationml.presentation") -- 	Microsoft PowerPoint (OpenXML)
    , (".rar", "application/vnd.rar") -- 	RAR archive
    , (".rtf", "application/rtf") -- 	Rich Text Format (RTF)
    , (".sh", "application/x-sh") -- 	Bourne shell script
    , (".svg", "image/svg+xml") -- 	Scalable Vector Graphics (SVG)
    , (".tar", "application/x-tar") -- 	Tape Archive (TAR)
    , (".tif", "image/tiff") -- 	Tagged Image File Format (TIFF)
    , (".tiff", "image/tiff") -- 	Tagged Image File Format (TIFF)
    , (".ts", "video/mp2t") -- 	MPEG transport stream
    , (".ttf", "font/ttf") -- 	TrueType Font
    , (".txt", "text/plain") -- 	Text, (generally ASCII or ISO 8859-n)
    , (".vsd", "application/vnd.visio") -- 	Microsoft Visio
    , (".wav", "audio/wav") -- 	Waveform Audio Format
    , (".weba", "audio/webm") -- 	WEBM audio
    , (".webm", "video/webm") -- 	WEBM video
    , (".webp", "image/webp") -- 	WEBP image
    , (".woff", "font/woff") -- 	Web Open Font Format (WOFF)
    , (".woff2", "font/woff2") -- 	Web Open Font Format (WOFF)
    , (".xhtml", "application/xhtml+xml") -- 	XHTML
    , (".xls", "application/vnd.ms-excel") -- 	Microsoft Excel
    , (".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") -- 	Microsoft Excel (OpenXML)
    , (".xml", "application/xml") -- 	XML	 is recommended as of RFC 7303 (section 4.1), but text/xml is still used sometimes. You can assign a specific MIME type to a file with .xml extension depending on how its contents are meant to be interpreted. For instance, an Atom feed is application/atom+xml, but application/xml serves as a valid default.
    , (".xul", "application/vnd.mozilla.xul+xml") -- 	XUL
    , (".zip", "application/zip") -- 	ZIP archive
    , (".3gp", "video/3gpp") -- 	3GPP audio/video container	; audio/3gpp if it doesn't contain video
    , (".3g2", "video/3gpp2") -- 	3GPP2 audio/video container	; audio/3gpp2 if it doesn't contain video
    , (".7z", "application/x-7z-compressed") -- 	7-zip archive
    ]

{- i wonder what is analog of this function?
-- | Handle errors
handleError ::(Exception a, MonadCatch m) => (a -> Server m) -> Server m -> Server m
handleError handler (Server act) = Server $ \req ->
  (act req) `catch` (\err -> unServer (handler err) req)
-}

{-| Sub-server for a server on given path
it might be usefule to emulate links from one route to another within the server
or reuse part of the server inside another server.
-}
atPath :: forall m. Api.Path -> Server m -> Server m
atPath rootPath rootServer = maybe mempty Server $ find rootPath rootServer.unServer
  where
    find :: Api.Path -> Api (Route m) -> Maybe (Api (Route m))
    find (Api.Path path) server = case path of
      [] -> Just server
      _ ->
        case server of
          Api.Empty -> Nothing
          Api.HandleRoute _ -> Nothing
          Api.Append a b -> find (Api.Path path) a <|> find (Api.Path path) b
          Api.WithPath (Api.Path pathB) serverB ->
            flip find serverB =<< matchPath pathB path

    matchPath :: [Api.PathItem] -> [Api.PathItem] -> Maybe Api.Path
    matchPath prefix path = case prefix of
      [] -> Just (Api.Path path)
      prefixHead : prefixTail -> do
        (pathHead, pathTail) <- List.uncons path
        guard (prefixHead == pathHead)
        matchPath prefixTail pathTail

filterPath :: (Api.Path -> Bool) -> Server m -> Server m
filterPath cond (Server a) =
  Server (Api.fromFlatApi $ filter (cond . fst) $ Api.flatApi a)

getServerPaths :: Server m -> [Api.Path]
getServerPaths (Server a) = fmap fst $ Api.flatApi a

{-| Links one route of the server to another
so that every call to first path is redirected to the second path
-}
addPathLink :: Api.Path -> Api.Path -> Server m -> Server m
addPathLink from to server =
  server <> Server (Api.WithPath from (atPath to server).unServer)
