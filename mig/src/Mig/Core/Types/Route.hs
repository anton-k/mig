-- | Newtype wrappers for route DSL
module Mig.Core.Types.Route (
  -- * inputs
  Body (..),
  Query (..),
  QueryFlag (..),
  Optional (..),
  Capture (..),
  Header (..),
  OptionalHeader (..),
  Cookie (..),
  PathInfo (..),
  FullPathInfo (..),
  RawRequest (..),
  IsSecure (..),

  -- * outputs
  Send (..),
  Get,
  Post,
  Put,
  Delete,
  Options,
  Head,
  Patch,
  Trace,

  -- ** Method tags
  IsMethod (..),
  GET,
  POST,
  PUT,
  DELETE,
  OPTIONS,
  HEAD,
  PATCH,
  TRACE,
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Text (Text)
import GHC.TypeLits
import Network.HTTP.Types.Method

import Mig.Core.Types.Http (Request)

-------------------------------------------------------------------------------------
-- inputs

-- | Generic case for request body. The type encodes a media type and value of the request body.
newtype Body media a = Body a

{-| Required URL parameter query.

> "api/route?foo=bar" ==> (Query bar) :: Query "foo" a
-}
newtype Query (sym :: Symbol) a = Query a

{-| Optional URL parameter query.

> "api/route?foo=bar" ==> (Optional maybeBar) :: Query "foo" a
-}
newtype Optional (sym :: Symbol) a = Optional (Maybe a)

{-| Query flag. It is a boolean value in the URL-query. If it is missing
it is @False@ if it is in the query but does not have any value it is @True@.
Also it can have values @true/false@ in the query.
-}
newtype QueryFlag (sym :: Symbol) = QueryFlag Bool

{-| Argument of capture from the query.

> "api/route/{foo} if api/route/bar passed"  ==> (Capture bar) :: Capture "Foo" barType
-}
newtype Capture (sym :: Symbol) a = Capture a

{-| Reads value from the required header by name. For example if the request has header:

> "foo": "bar"

It reads the value:

> (Header bar) :: Header "foo" barType
-}
newtype Header (sym :: Symbol) a = Header a

{-| Reads value from the optional header by name. For example if the request has header:

> "foo": "bar"

It reads the value:

> (OptionalHeader (Just bar)) :: OptionalHeader "foo" barType
-}
newtype OptionalHeader (sym :: Symbol) a = OptionalHeader (Maybe a)

{-| Reads a cookie. It's an optional header with name "Cookie".
The cookie is URL-encoded and read with instnace of FromForm class.

> data MyCookie = MyCookie
>   { secret :: Text
>   , count :: Int
>   }
>   deriving (Generic, FromForm)
>
> > "secret=lolkek&count=101"
>
> (Cookie (Just (MyCookie { secret = "lolkek", count = 101 }))) :: Cookie MyCookie
-}
newtype Cookie a = Cookie (Maybe a)

{-| Reads current path info.

> "api/foo/bar" ==> PathInfo ["foo", "bar"]
-}
newtype PathInfo = PathInfo [Text]

{-| Reads current full-path info with queries.

> "api/foo/bar?param=value" ==> FullPathInfo "api/foo/bar?param=value"
-}
newtype FullPathInfo = FullPathInfo Text

-- | Read low-level request. Note that it does not affect the API schema
newtype RawRequest = RawRequest Request

-- | Reads info on weather the connection is secure (made over SSL).
newtype IsSecure = IsSecure Bool

-------------------------------------------------------------------------------------
-- outputs

{-| Route response type. It encodes the route method in the type
and which monad is used and which type the response has.

The repsonse value is usually one of two cases:

* @Resp media a@ -- for routes which always produce a value

* @RespOr media err a@ - for routes that can also produce an error or value.

See the class @IsResp@ for more details on response types.
-}
newtype Send method m a = Send {unSend :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (Send method) where
  lift = Send

-- | type-level GET-method tag
data GET

-- | type-level POST-method tag
data POST

-- | type-level PUT-method tag
data PUT

-- | type-level DELETE-method tag
data DELETE

-- | type-level OPTIONS-method tag
data OPTIONS

-- | type-level HEAD-method tag
data HEAD

-- | type-level PATCH-method tag
data PATCH

-- | type-level TRACE-method tag
data TRACE

-- | Get request
type Get m a = Send GET m a

-- | Post request
type Post m a = Send POST m a

-- | Put request
type Put m a = Send PUT m a

-- | Delete request
type Delete m a = Send DELETE m a

-- | Options request
type Options m a = Send OPTIONS m a

-- | Head request
type Head m a = Send HEAD m a

-- | Path request
type Patch m a = Send PATCH m a

-- | trace request
type Trace m a = Send TRACE m a

-- | Converts type-level tag for methods to value
class IsMethod a where
  toMethod :: Method

instance IsMethod GET where
  toMethod = methodGet

instance IsMethod POST where
  toMethod = methodPost

instance IsMethod PUT where
  toMethod = methodPut

instance IsMethod DELETE where
  toMethod = methodDelete

instance IsMethod OPTIONS where
  toMethod = methodOptions

instance IsMethod HEAD where
  toMethod = methodHead

instance IsMethod PATCH where
  toMethod = methodPatch

instance IsMethod TRACE where
  toMethod = methodTrace
