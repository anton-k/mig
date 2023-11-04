module Test.Api (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Mig.Core.Api
import Test.Hspec

spec :: Spec
spec = describe "api" $ do
  checkRoutes
  checkCaptures
  checkFlatApi

notFound :: (Show a, Eq a) => Maybe a -> Expectation
notFound a = a `shouldBe` Nothing

-- static routes

checkRoutes :: Spec
checkRoutes = do
  it "enter route (positive cases)" $ do
    getPath ["api", "v1", "hello"] helloApi `shouldBe` Just ("hello", mempty)
    getPath ["api", "v1", "bye"] helloApi `shouldBe` Just ("bye", mempty)
  it "enter route (negative cases)" $
    mapM_
      notFound
      [ getPath ["api", "v1"] helloApi
      , getPath [] helloApi
      , getPath ["api", "v1", "hello", "there"] helloApi
      , getPath ["api", "v1"] (mempty @(Api Text))
      , getPath [] (mempty @(Api Text))
      ]

helloApi :: Api Text
helloApi =
  WithPath "api/v1" $
    mconcat
      [ WithPath "hello" (HandleRoute "hello")
      , WithPath "bye" (HandleRoute "bye")
      ]

-- captures

checkCaptures :: Spec
checkCaptures = do
  it "captures (positive cases)" $ do
    getPath ["api", "capture1", "hello"] captureApi
      `shouldBe` Just ("capture1", Map.fromList [("name1", "hello")])
    getPath ["api", "capture2", "hello", "bye"] captureApi
      `shouldBe` Just ("capture2", Map.fromList [("name1", "hello"), ("name2", "bye")])

  it "captures (negative cases)" $
    mapM_
      (notFound . flip getPath captureApi)
      [ ["api", "capture1"]
      , ["api", "capture2", "hello"]
      , ["api", "capture2", "hello", "bye", "error"]
      ]

captureApi :: Api Text
captureApi =
  WithPath "api" $
    mconcat
      [ WithPath ("capture1" <> Path [CapturePath "name1"]) (HandleRoute "capture1")
      , WithPath ("capture2" <> Path [CapturePath "name1", CapturePath "name2"]) (HandleRoute "capture2")
      ]

-- flat api

checkFlatApi :: Spec
checkFlatApi =
  it "flat api" $
    flatApi helloApi
      `shouldBe` [ ("api/v1/hello", "hello")
                 , ("api/v1/bye", "bye")
                 ]
