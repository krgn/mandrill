{-# LANGUAGE OverloadedStrings #-}
module Network.Mandrill.WebhooksSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.Mandrill.Types
import qualified Data.Text                 as Text 
import qualified Network.Mandrill.Webhooks as Webhooks
import           System.Environment

spec :: Spec
spec = do
  test_list
  test_add
  test_info
  test_update
  test_delete

test_delete :: Spec
test_delete = 
  describe "/webhooks/delete.json" $
    it "delete a webhook" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Webhooks.delete key 1234
      resp `shouldSatisfy` isRight

test_update :: Spec
test_update = 
  describe "/webhooks/update.json" $
    it "should update a webhook" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Webhooks.update key 1234 "example.com" "it is what it is" []
      resp `shouldSatisfy` isRight

test_info :: Spec
test_info = 
  describe "/webhooks/info.json" $
    it "should return some detailed info about a webhook" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Webhooks.info key 1234
      resp `shouldSatisfy` isRight

test_add :: Spec
test_add = 
  describe "/webhooks/add.json" $
    it "should add a webhook" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Webhooks.add key "example.com" "it is what it is" []
      resp `shouldSatisfy` isRight

test_list :: Spec
test_list = 
  describe "/webhooks/list.json" $
    it "should list all webhooks" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Webhooks.list key 
      resp `shouldSatisfy` isRight
