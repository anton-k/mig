{-# LANGUAGE UndecidableInstances #-}

module Mig.Internal.Server (
  Server,
  route,
  fromServer,
  toApplication,
  runServer,
  fillCaptures,
  addTag,
  setDescription,
  setSummary,
  mapRouteInfo,
  staticFiles,
) where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Either.Extra
import Data.List qualified as List
import Data.List.Extra (firstJust)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding as Text
import Mig.Internal.Api (Api, fromNormalApi, toNormalApi)
import Mig.Internal.Api qualified as Api
import Mig.Internal.Info (MediaType (..), RouteInfo (..), RouteInput (..))
import Mig.Internal.Route
import Mig.Internal.Types (Req (..))
import Mig.Internal.Wai (ServerConfig (..))
import Mig.Internal.Wai qualified as Wai
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Safe (atMay)
import System.FilePath (takeExtension, (</>))
import Text.Read (readMaybe)

-- import Debug.Trace

type Server m = Api (Route m)

route :: (ToRoute a) => a -> Server (RouteMonad a)
route a = Api.Route (toRoute a)

fromServer :: (Monad m) => Server m -> ServerFun m
fromServer server = ServerFun $ \req ->
  case Api.getPath req.path =<< firstJust (\outputMedia -> fromNormalApi req.method outputMedia (getInputMediaType req) serverNormal) (getOutputMediaType req) of
    Just (routes, captureMap) -> unServerFun routes.run (req{capture = captureMap})
    Nothing -> pure Nothing
  where
    serverNormal = toNormalApi (fillCaptures server)

-- | Substitutes all stars * for corresponding names in captures
fillCaptures :: Api (Route m) -> Api (Route m)
fillCaptures = go 0
  where
    go n = \case
      Api.WithPath path api ->
        let
          (pathNext, m) = goPath n path api
         in
          Api.WithPath pathNext (go m api)
      Api.Append a b -> Api.Append (go n a) (go n b)
      Api.Empty -> Api.Empty
      Api.Route a -> Api.Route a

    goPath :: Int -> Api.Path -> Api (Route m) -> (Api.Path, Int)
    goPath n (Api.Path path) api = case path of
      [] -> (Api.Path path, n)
      Api.CapturePath "*" : rest ->
        let
          (nextRest, m) = goPath (n + 1) (Api.Path rest) api
         in
          case getCaptureName n api of
            Just name -> (Api.Path [Api.CapturePath name] <> nextRest, m)
            Nothing -> error $ "No capture argument for start in path at the index: " <> show n
      a : rest ->
        let
          (nextRest, m) = goPath n (Api.Path rest) api
         in
          (Api.Path [a] <> nextRest, m)

getCaptureName :: Int -> Api (Route m) -> Maybe Text
getCaptureName index = \case
  Api.Append a _b -> rec a
  Api.Empty -> Nothing
  Api.WithPath _ a -> rec a
  Api.Route a -> mapMaybe toCapture a.api.inputs `atMay` index
  where
    rec = getCaptureName index

    toCapture :: RouteInput -> Maybe Text
    toCapture = \case
      CaptureInput name _ -> Just name
      _ -> Nothing

getInputMediaType :: Req -> MediaType
getInputMediaType req =
  maybe (MediaType "*/*") (MediaType . Text.strip . Text.takeWhile (/= ';')) $
    eitherToMaybe . Text.decodeUtf8' =<< Map.lookup "Content-Type" req.headers

getOutputMediaType :: Req -> [MediaType]
getOutputMediaType req =
  fromMaybe [MediaType "*/*"] $
    parseMedias =<< eitherToMaybe . Text.decodeUtf8' =<< Map.lookup "Content-Type" req.headers
  where
    parseMedias :: Text -> Maybe [MediaType]
    parseMedias txt =
      fmap (fmap snd . List.sortOn fst) $ mapM (toMediaWithWeight . Text.strip) $ Text.splitOn "," txt

    toMediaWithWeight :: Text -> Maybe (Float, MediaType)
    toMediaWithWeight txt =
      case Text.splitOn ";" txt of
        [name] -> Just (1, MediaType name)
        name : weightTxt : [] -> (,MediaType name) <$> parseWeight weightTxt
        _ -> Nothing
      where
        parseWeight weightTxt
          | Text.isPrefixOf "q=" weightTxt = readMaybe $ Text.unpack $ Text.drop 2 weightTxt
          | otherwise = Nothing

runServer :: Int -> Server IO -> IO ()
runServer port server = Warp.run port (toApplication config server)
  where
    config = ServerConfig{maxBodySize = Nothing}

toApplication :: ServerConfig -> Server IO -> Wai.Application
toApplication config server = Wai.toApplication config (fromServer server)

addTag :: Text -> Server m -> Server m
addTag tag = mapRouteInfo (insertTag tag)

setDescription :: Text -> Server m -> Server m
setDescription desc = mapRouteInfo $ \info -> info{description = desc}

setSummary :: Text -> Server m -> Server m
setSummary val = mapRouteInfo $ \info -> info{summary = val}

mapRouteInfo :: (RouteInfo -> RouteInfo) -> Server m -> Server m
mapRouteInfo f = fmap $ \x -> x{api = f x.api}

insertTag :: Text -> RouteInfo -> RouteInfo
insertTag tag info = info{tags = tag : info.tags}

staticFiles :: forall m. (MonadIO m) => [(FilePath, ByteString)] -> FilePath -> Server m
staticFiles files root =
  foldMap (uncurry serveFile) files
  where
    serveFile path content =
      (fromString $ withRoot path) Api./. route (getFile path content)

    getFile :: FilePath -> ByteString -> Get BL.ByteString m (AddHeaders BL.ByteString)
    getFile path fileContent = Send $ pure $ AddHeaders contentHeaders $ BL.fromStrict fileContent
      where
        contentHeaders :: ResponseHeaders
        contentHeaders =
          case mimeType of
            Just ty -> [("Content-Type", ty)]
            Nothing -> []

        mimeType = Map.lookup (takeExtension path) extToMimeMap

    withRoot path
      | null root = path
      | otherwise = root </> path

extToMimeMap :: Map String ByteString
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