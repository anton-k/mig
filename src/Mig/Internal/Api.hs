-- | Internal API description
module Mig.Internal.Api
  ( Api (..)
  , Path
  , PathItem (..)
  , (/.)
  , (/$)
  , getPath
  , CaptureMap
  ) where

import Data.Text (Text)
import Data.Map.Strict (Map)

data Api a
  = Append (Api a) (Api a)
  | Empty
  | WithPath Path (Api a)
  | Route a
  deriving (Functor, Foldable, Traversable)

instance Monoid (Api a) where
  mempty = Empty

instance Semigroup (Api a) where
  (<>) = Append

type Path = [PathItem]

data PathItem
  = StaticPath Text
  | CapturePath Text

infixr 4 /.
infixr 4 /$

(/.) :: Text -> Api a -> Api a
(/.) path a = WithPath [StaticPath path] a

(/$) :: Text -> Api a -> Api a
(/$) path a = WithPath [CapturePath path] a

type CaptureMap = Map Text Text

getPath :: Api a -> Path -> (a, CaptureMap)
getPath = undefined


{-
example :: Server IO
example =
  "api" /.
      mconcat
        [ "foo" /. getJson foo
        , "bat" /$ "arg" /. "path" /. getJson bar
        ]
    where
      foo :: Fun IO
      foo = emptyRoute

      bar :: Capture "arg" Int -> Query "a" Int -> Fun IO
      bar = emptyRoute

getJson ::
  forall a . (ToRoute a) => a -> Server (RouteMonad a)
getJson a = Route $ route { api = setMethod methodGet JsonType route.api }
  where
    route = toRoute a

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
