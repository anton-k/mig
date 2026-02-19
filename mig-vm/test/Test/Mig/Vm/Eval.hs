-- | Simple interpreter for VM commands
module Test.Mig.Vm.Eval 
  ( Req (..)
  , Resp (..)
  , Header (..)
  , QueryParam (..)
  , eval
  ) where

import Mig.Vm.Types 
import Data.Text (Text)
import Data.IORef
import Data.IntMap (IntMap)
import Data.Map.Strict (Map)
import Data.IntMap qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.List qualified as List

import Test.Mig.Vm.Eval.Types
import Test.Mig.Vm.Eval.Types qualified as Resp (Resp (..))

newtype Funs = Funs (IntMap Fun)

lookupFun :: FunIndex -> Funs -> Maybe Fun
lookupFun (FunIndex index) (Funs funs) =
  IntMap.lookup index funs

initFuns :: Ctx -> Funs
initFuns ctx = Funs $ IntMap.fromList $ zip [0..] (ctxGetFuns ctx)

type Stack = [Val]

data Refs = Refs 
  { stack :: StackRef 
  , ops :: OpsRef 
  , uri :: UriRef
  , resp :: RespRef
  , captures :: CaptureRef
  }

newtype CaptureRef = CaptureRef (IORef (Map Text Text))

newCaptureRef :: IO CaptureRef
newCaptureRef = 
  CaptureRef <$> newIORef Map.empty

readCapture :: CaptureRef -> Text -> IO (Maybe Text)
readCapture (CaptureRef ref) name = 
  Map.lookup name <$> readIORef ref

writeCapture :: CaptureRef -> Text -> Text -> IO ()
writeCapture (CaptureRef ref) key val = 
  modifyIORef' ref $ Map.insert key val

newtype RespRef = RespRef { unRespRef :: IORef Resp}

newRespRef :: IO RespRef 
newRespRef = RespRef <$> newIORef 
  (Resp { code = 200, headers = [], body = Nothing })

setRespCode :: RespRef -> Int -> IO ()
setRespCode (RespRef ref) code = modifyIORef' ref $ \resp ->
  resp { Resp.code = code }

setRespBody :: RespRef -> Val -> IO ()
setRespBody (RespRef ref) body = modifyIORef' ref $ \resp ->
  resp { Resp.body = Just body }

setRespHeader :: RespRef -> Header -> IO ()
setRespHeader (RespRef ref) header = modifyIORef'  ref $ \resp ->
  resp { Resp.headers = update resp.headers }
  where
    update hs = header : (List.filter (\h -> h.name /= header.name) hs)

newRefs :: Ops -> Req -> IO Refs
newRefs operators req = do
  stack <- newStackRef
  ops <- newOpsRef operators
  uri <- newUriRef req.uri
  resp <- newRespRef 
  captures <- newCaptureRef 
  pure Refs{..}

newtype UriRef = UriRef (IORef Path)

newUriRef :: Path -> IO UriRef
newUriRef path = UriRef <$> newIORef path

newtype StackRef = StackRef (IORef Stack)
newtype OpsRef = OpsRef (IORef Ops)

newStackRef :: IO StackRef 
newStackRef = StackRef <$> newIORef [] 

newOpsRef :: Ops -> IO OpsRef
newOpsRef ops = OpsRef <$> newIORef ops

initMemory :: StackRef -> OpsRef -> Memory
initMemory (StackRef stackRef) (OpsRef opsRef) = Memory
  { readStack = do
      stack <- readIORef stackRef
      case stack of
        v:vs -> do
          writeIORef stackRef vs
          pure (Just v)
        [] -> pure Nothing

  , writeStack = \val -> 
      modifyIORef' stackRef (val : )

  , putOps = \(Ops val) ->
      modifyIORef' opsRef (\(Ops ops) -> Ops (val <> ops))
  }

readOp :: OpsRef -> IO (Maybe Op)
readOp (OpsRef opsRef) = do
  Ops ops <- readIORef opsRef
  case ops of
    v:vs -> do
      writeIORef opsRef (Ops vs)
      pure (Just v)
    [] -> pure Nothing
  
eval :: Ctx -> Ops -> Req -> IO (Either Text Resp)
eval ctx operators req = do
  evalCtx <- newEvalCtx ctx operators req
  eval' evalCtx req 

data EvalCtx = EvalCtx
  { memory :: Memory
  , funs :: Funs
  , refs :: Refs
  }

newEvalCtx :: Ctx -> Ops -> Req -> IO EvalCtx
newEvalCtx ctx operators req = do
  refs <- newRefs operators req
  pure $ EvalCtx
    { memory = initMemory refs.stack refs.ops
    , funs = initFuns ctx
    , refs 
    }

splitUri :: Int -> Text -> (Text, Text)
splitUri size uri = 
  (Text.intercalate "/" pre, Text.intercalate "/" post)
  where
    parts = Text.split (== '/') uri
    (pre, post) = List.splitAt size parts

eval' :: EvalCtx -> Req -> IO (Either Text Resp)
eval' ctx req = do
  mOp <- readOp ctx.refs.ops  
  case mOp of
    Nothing -> pure (Left "no operators left")
    Just op -> do
      case op of
        GetUri -> getUri
        GetUriPart n -> getUriPart n
        SaveCapture name -> saveCapture name
        GetMethod -> getMethod
        GetQuery name -> getQuery name
        GetBody -> getBody
        GetHeader name -> getHeader name
        GetCapture name -> getCapture name
        SetHeader name val -> setHeader name val
        SetBody bytes -> setBody bytes
        SetCode code -> setCode code
        -- handler
        Fun n -> fun n
        -- response
        SendError msg -> sendError msg
        SendText val -> sendText val
        SendByteString bytes -> sendByteString bytes
        SendResp -> sendResp
        -- switch
        WhenMethod method arity -> whenMethod method arity
        Case val arity -> onCase val arity
  where
    next = eval' ctx req 
    emptyStackError = pure $ Left $ "Stack is empty"
    noBodyError = pure $ Left "No body in request"
    noQueryError name = pure $ Left $ "No value for query: " <> name
    noHeaderError name = pure $ Left $ "No value for header: " <> name
    noFunError index = pure $ Left $ "No function by the index: " <> Text.show index
    noCaptureError name = pure $ Left $ "No capture by the name: " <> name
    wrongCaptureArgError name = pure $ Left $ "Wrong capture state: " <> name

    ok hs val = Resp 
      { code = 200
      , headers = hs
      , body = val
      }

    -- puts full URI on stack
    getUri = do 
      ctx.memory.writeStack (TVal $ pathToText req.uri)
      next

    -- reads from stack part of URI
    -- splits it and puts parts on the stack 
    getUriPart n = do 
      mVal <- ctx.memory.readStack
      case mVal of
        Just (TVal uri) -> do
          let
            (uriA, uriB) = splitUri n uri
          ctx.memory.writeStack (TVal uriB)
          ctx.memory.writeStack (TVal uriA)
          next

        _ -> emptyStackError

    getBody = do
      case req.body of
        Just val -> do
          ctx.memory.writeStack val
          next
        Nothing -> noBodyError

    getCapture name = do
      mCapture <- readCapture ctx.refs.captures name
      case mCapture of
        Just capture -> do
          ctx.memory.writeStack (TVal capture)
          next 
        Nothing -> noCaptureError name 

    saveCapture name = do
      mVal <- ctx.memory.readStack
      case mVal of
        Just (TVal pathItem) -> do
          writeCapture ctx.refs.captures name pathItem
          next 
        _ -> wrongCaptureArgError name

    getMethod = do
      ctx.memory.writeStack (MVal req.method)
      next

    -- put on top of the stack QueryParam by name
    getQuery name = do
      let
        mVal = fmap (.value) $ List.find (\query -> query.name == name) req.queries
      case mVal of
        Just val -> do
          ctx.memory.writeStack (TVal val)
          next
        Nothing -> noQueryError name 

    getHeader name = do
      let
        mVal = fmap (.value) $ List.find (\header -> header.name == name) req.headers
      case mVal of
        Just val -> do
          ctx.memory.writeStack (BVal val)
          next
        Nothing -> noHeaderError name 

    setHeader name val = setRespHeader ctx.refs.resp (Header name val) >> next
    setBody val = setRespBody  ctx.refs.resp (BVal val) >> next
    setCode code = setRespCode ctx.refs.resp code >> next

    fun index = do
      case lookupFun index ctx.funs of
        Just f -> do
          f ctx.memory 
          next
        Nothing -> noFunError index

    sendError msg = pure $ Left msg

    sendText txt = pure $ Right $ ok [Header "Content-Type" "text/plain"] (Just (TVal txt))

    sendByteString bytes = pure $ Right $ ok [Header "Content-Type" "text/plain"] (Just (BVal bytes))

    sendResp = Right <$> readIORef ctx.refs.resp.unRespRef

    whenMethod method size 
      | req.method == method = next
      | otherwise = do
          dropOps ctx.refs.ops size 
          next

    onCase expectedVal size = do
      mVal <- ctx.memory.readStack 
      case mVal of
        Just val -> do
          if (expectedVal == val)
            then next 
            else do
              ctx.memory.writeStack val
              dropOps ctx.refs.ops size
              next
              
        Nothing -> emptyStackError
      

dropOps :: OpsRef -> Int -> IO ()
dropOps (OpsRef opsRef) size = 
  modifyIORef' opsRef $ \(Ops ops) -> Ops (List.drop size ops)
