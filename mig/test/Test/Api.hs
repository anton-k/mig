module Test.Api (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Mig.Core.Api
import Test.Hspec

spec :: Spec
spec = describe "Api tests" $ do
  checkRoutes
  checkCaptures
  checkFlatApi

-- static routes

checkRoutes :: Spec
checkRoutes = do
  it "enter route (positive cases)" $ do
    getPath ["api", "v1", "hello"] helloApi `shouldBe` Just ("hello", mempty)
    getPath ["api", "v1", "bye"] helloApi `shouldBe` Just ("bye", mempty)
  it "enter route (negative cases)" $ do
    getPath ["api", "v1"] helloApi `shouldBe` Nothing
    getPath [] helloApi `shouldBe` Nothing
    getPath ["api", "v1", "hello", "there"] helloApi `shouldBe` Nothing
    getPath ["api", "v1"] (mempty @(Api Text)) `shouldBe` Nothing
    getPath [] (mempty @(Api Text)) `shouldBe` Nothing

helloApi :: Api Text
helloApi =
  WithPath "api/v1" $
    mconcat
      [ WithPath "hello" (HandleRoute "hello")
      , WithPath "bye" (HandleRoute "bye")
      ]

-- flat api

checkFlatApi :: Spec
checkFlatApi =
  it "flat api" $
    flatApi helloApi
      `shouldBe` [ ("api/v1/hello", "hello")
                 , ("api/v1/bye", "bye")
                 ]

-- captures

checkCaptures :: Spec
checkCaptures = do
  it "captures (positive cases)" $ do
    getPath ["api", "capture1", "hello"] captureApi
      `shouldBe` Just ("capture1", Map.fromList [("name1", "hello")])
    getPath ["api", "capture2", "hello", "bye"] captureApi
      `shouldBe` Just ("capture2", Map.fromList [("name1", "hello"), ("name2", "bye")])

  it "captures (negative cases)" $ do
    getPath ["api", "capture1"] captureApi `shouldBe` Nothing
    getPath ["api", "capture2", "hello"] captureApi `shouldBe` Nothing
    getPath ["api", "capture2", "hello", "bye", "error"] captureApi `shouldBe` Nothing

captureApi :: Api Text
captureApi =
  WithPath "api" $
    mconcat
      [ WithPath ("capture1" <> Path [CapturePath "name1"]) (HandleRoute "capture1")
      , WithPath ("capture2" <> Path [CapturePath "name1", CapturePath "name2"]) (HandleRoute "capture2")
      ]
