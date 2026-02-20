module Test.Mig.Vm.Test where

import Mig.Vm.Class hiding (Header)
import Mig.Vm.Class qualified as Mig
import Mig.Vm.Types
import Control.Monad.State.Strict
import Test.Hspec
import Test.Mig.Vm.Eval
import Data.Text (Text)
import Data.Text qualified as Text

handlerA :: Send GET IO Text
handlerA = Send (pure "Hello world")

handlerB :: Query "arg" Int -> Send GET IO Int
handlerB (Query arg) = Send (pure (arg + 1))

handlerB2 :: 
  Query "a" Text -> Query "b" Text -> Query "c" Text -> Send GET IO Text
handlerB2 (Query a) (Query b) (Query c) = Send (pure (mconcat [a,b,c]))

handlerC :: Query "a" Int -> Query "b" Int -> Send GET IO Int
handlerC (Query a) (Query b) = Send (pure (a + b))

handlerC2 :: Query "a" Int -> Query "b" Int -> Send GET IO Int
handlerC2 (Query a) (Query b) = Send (pure (a - b))

handlerD :: 
  Mig.Header "greet" Text -> Query "a" Int -> Query "b" Int -> 
  Send GET IO Text
handlerD (Mig.Header greet) (Query a) (Query b) = Send (pure $ toResp (a + b))
  where
    toResp n = greet <> ": " <> Text.show n

spec :: Spec
spec = describe "Simple handlers" $ do
  checkA
  checkB
  checkB2
  checkC
  checkC2
  checkD

checkA :: Spec
checkA = 
  it "hello world handler" $ do
    (ops, ctx) <- runStateT (toRoute handlerA:: StateT Ctx IO Ops) emptyCtx 
    eResp <- eval ctx ops req
    eResp `shouldBe` Right resp
  where
    req = emptyReq
    resp = okText "Hello world"

checkB :: Spec
checkB = 
  it "increment handler" $ do
    (ops, ctx) <- runStateT (toRoute handlerB:: StateT Ctx IO Ops) emptyCtx 
    eResp <- eval ctx ops req
    eResp `shouldBe` Right resp
  where
    req = emptyReq { queries = [QueryParam "arg" "1"]}

    resp = okText "2"

checkB2 :: Spec
checkB2 = 
  it "concat handler" $ do
    (ops, ctx) <- runStateT (toRoute handlerB2 :: StateT Ctx IO Ops) emptyCtx 
    eResp <- eval ctx ops req
    eResp `shouldBe` Right resp
  where
    req = emptyReq
      { queries = [QueryParam "a" "A", QueryParam "b" "B", QueryParam "c" "C"]
      }

    resp = okText "ABC"

checkC :: Spec
checkC = 
  it "addition handler" $ do
    (ops, ctx) <- runStateT (toRoute handlerC :: StateT Ctx IO Ops) emptyCtx 
    eResp <- eval ctx ops req
    eResp `shouldBe` Right resp
  where
    req = emptyReq
      { queries = [QueryParam "a" "2", QueryParam "b" "2"]
      }

    resp = okText "4"

checkC2 :: Spec
checkC2 = 
  it "subtraction handler" $ do
    (ops, ctx) <- runStateT (toRoute handlerC2 :: StateT Ctx IO Ops) emptyCtx 
    eResp <- eval ctx ops req
    eResp `shouldBe` Right resp
  where
    req = emptyReq
      { queries = [QueryParam "a" "7", QueryParam "b" "2"]
      }

    resp = okText "5"


checkD :: Spec
checkD = 
  it "addition handler with greeting" $ do
    (ops, ctx) <- runStateT (toRoute handlerD :: StateT Ctx IO Ops) emptyCtx 
    eResp <- eval ctx ops req
    eResp `shouldBe` Right resp
  where
    req = emptyReq 
      { headers = [Header "greet" "Result is"]
      , queries = [QueryParam "a" "2", QueryParam "b" "2"]
      }

    resp = okText "Result is: 4"

okText :: Text -> Resp 
okText msg = Resp
      { code = 200
      , headers = [Header "Content-Type" "text/plain"]
      , body = Just (TVal msg)
      }

emptyReq :: Req
emptyReq = Req
  { uri = Path []
  , method = Get 
  , headers = []
  , body = Nothing
  , queries = []
  }
