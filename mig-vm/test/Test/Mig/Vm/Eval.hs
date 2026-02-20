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
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import Test.Mig.Vm.Eval.Types

newtype Funs = Funs (IntMap Fun)

lookupFun :: FunIndex -> Funs -> Maybe Fun
lookupFun (FunIndex index) (Funs funs) =
  IntMap.lookup index funs

initFuns :: Ctx -> Funs
initFuns ctx = Funs $ IntMap.fromList $ zip [0..] (ctxGetFuns ctx)

type Stack = [Val]

data Refs = Refs
  { stack :: StackRef
  , uri :: UriRef
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

newRefs :: Req -> IO Refs
newRefs req = do
  stack <- newStackRef
  uri <- newUriRef req.uri
  captures <- newCaptureRef
  pure Refs{..}

newtype UriRef = UriRef (IORef Path)

newUriRef :: Path -> IO UriRef
newUriRef path = UriRef <$> newIORef path

newtype StackRef = StackRef (IORef Stack)

newStackRef :: IO StackRef
newStackRef = StackRef <$> newIORef []

initMemory :: StackRef -> Memory
initMemory (StackRef stackRef) = Memory
  { readStack = do
      stack <- readIORef stackRef
      case stack of
        v:vs -> do
          writeIORef stackRef vs
          pure (Just v)
        [] -> pure Nothing

  , writeStack = \val ->
      modifyIORef' stackRef (val : )
  }

data EvalCtx = EvalCtx
  { memory :: Memory
  , ops :: Vector Op
  , funs :: Funs
  , refs :: Refs
  , codeIndex :: IORef Int
  }

checkPathEmpty :: EvalCtx -> IO Bool
checkPathEmpty ctx = (null . unPath) <$> readIORef ref
  where
    UriRef ref = ctx.refs.uri

matchPath :: EvalCtx -> Text -> IO Bool
matchPath = error "TODO"

newEvalCtx :: Ctx -> Ops -> Req -> IO EvalCtx
newEvalCtx ctx (Ops operations) req = do
  refs <- newRefs req
  codeIndex <- newIORef 0
  pure $ EvalCtx
    { memory = initMemory refs.stack
    , funs = initFuns ctx
    , ops = Vector.fromList operations
    , refs
    , codeIndex
    }

splitUri :: Int -> Text -> (Text, Text)
splitUri size uri =
  (Text.intercalate "/" pre, Text.intercalate "/" post)
  where
    parts = Text.split (== '/') uri
    (pre, post) = List.splitAt size parts

readOp :: EvalCtx -> IO (Maybe Op)
readOp ctx = do
  index <- readIORef ctx.codeIndex
  writeIORef ctx.codeIndex (index + 1)
  pure (ctx.ops Vector.!? index)

moveCodePointerToLabel :: EvalCtx -> CodeLabel -> IO ()
moveCodePointerToLabel ctx (CodeLabel index) =
  writeIORef ctx.codeIndex index

eval :: Ctx -> Ops -> Req -> IO (Either Text Resp)
eval ctx ops req = do
  evalCtx <- newEvalCtx ctx ops req
  eval' evalCtx req

eval' :: EvalCtx -> Req -> IO (Either Text Resp)
eval' ctx req = do
  mOp <- readOp ctx
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
        -- handler
        Fun n -> fun n
        -- response
        SendResp -> sendResp
        -- switch
        Label _ -> next
        Goto label -> goto label
        Ifeq val label -> ifeq val label
        IfPathEq path label -> ifPathEq path label
        -- generic stack
        Push val -> push val
        Pop -> pop
        Dup -> dup
  where
    next = eval' ctx req
    emptyStackError = pure $ Left $ "Stack is empty"
    noBodyError = pure $ Left "No body in request"
    noQueryError name = pure $ Left $ "No value for query: " <> name
    noHeaderError name = pure $ Left $ "No value for header: " <> name
    noFunError index = pure $ Left $ "No function by the index: " <> Text.show index
    noCaptureError name = pure $ Left $ "No capture by the name: " <> name
    noResponseError = pure $ Left $ "No response sent"
    wrongCaptureArgError name = pure $ Left $ "Wrong capture state: " <> name

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

    fun index = do
      case lookupFun index ctx.funs of
        Just f -> do
          f ctx.memory
          next
        Nothing -> noFunError index

    sendResp = do
      eResp <- ctx.memory.readStack
      case eResp of
        Just (RespVal resp) -> pure (Right resp)
        _ -> noResponseError

    goto label = moveCodePointerToLabel ctx label >> next

    ifeq expectedVal label = do
      mVal <- ctx.memory.readStack
      case mVal of
        Just val ->
          if (val == expectedVal)
            then next
            else goto label
        Nothing -> emptyStackError

    -- TODO: check media
    ifPathEq path label = do
      ok <- matchPath ctx path
      if ok
        then next
        else goto label

    push val = ctx.memory.writeStack val >> next

    pop = do
      _ <- ctx.memory.readStack
      next

    dup = do
      mVal <- ctx.memory.readStack
      case mVal of
        Just val -> do
          ctx.memory.writeStack val
          ctx.memory.writeStack val
          next
        Nothing -> emptyStackError
