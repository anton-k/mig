{-# LANGUAGE DataKinds #-}

-- | Test case for ReaderT based server
module Test.Server.Counter (spec) where

import Control.Monad.Reader
import Data.Aeson qualified as Json
import Data.IORef
import Data.Maybe
import Data.Text qualified as Text
import Mig.Core
import Network.HTTP.Types.Method (methodPost)
import Test.Hspec
import Test.Server.Common

-------------------------------------------------------------------------------------
-- server definition

newtype Env = Env (IORef Int)

initEnv :: IO Env
initEnv = Env <$> newIORef 0

newtype App a = App (ReaderT Env IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)

runApp :: Env -> App a -> IO a
runApp env (App act) = runReaderT act env

{-| Server has two routes:

* get - to querry current state
* put - to add some integer to the state
-}
server :: Server App
server =
  "counter"
    /. [ "get" /. handleGet
       , "put" /. handlePut
       ]

-- | Get handler. It logs the call and returns current state
handleGet :: Get App (Resp Json Int)
handleGet = Send $ do
  Env ref <- ask
  liftIO $ ok <$> readIORef ref

-- | Put handler. It logs the call and updates the state with integer which is read from URL
handlePut :: Capture "arg" Int -> Post App (Resp Json ())
handlePut (Capture val) = Send $ do
  Env ref <- ask
  liftIO $ ok <$> atomicModifyIORef' ref (\cur -> (cur + val, ()))

-------------------------------------------------------------------------------------
-- test cases

spec :: Spec
spec = describe "counter server (ReaderT)" $ do
  describe "plain route finder" $ specBy plainApiStrategy
  describe "tree route finder" $ specBy treeApiStrategy

specBy :: FindRoute normalForm App -> Spec
specBy findRoute =
  it "run accumulator script" $
    script serverFun [1, 2, 3, 4] `shouldReturn` [1, 3, 6, 10]
  where
    serverFun = fromServer findRoute server

{-| Puts inputs to server and returns result of "counter/get" method call
on each increment
-}
script :: ServerFun App -> [Int] -> IO [Int]
script f inputs = do
  env <- initEnv
  runApp env $ catMaybes <$> mapM go inputs
  where
    go :: Int -> App (Maybe Int)
    go n = fmap (parseInt =<<) $ do
      mRes <- f (putReq n)
      if (isJust mRes)
        then f getReq
        else pure Nothing

    putReq :: Int -> Request
    putReq increment =
      emptyReq
        { method = methodPost
        , path = ["counter", "put", Text.pack (show increment)]
        }

    getReq :: Request
    getReq = emptyReq{path = ["counter", "get"]}

    parseInt :: Response -> Maybe Int
    parseInt resp = case resp.body of
      RawResp "application/json" bsResp -> Json.decode bsResp
      _ -> Nothing
