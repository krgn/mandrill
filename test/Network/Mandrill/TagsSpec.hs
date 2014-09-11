{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.TagsSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Network.Mandrill.Types
import qualified Network.Mandrill.Tags as Tags
import System.Environment
import qualified Data.Text as Text

spec :: Spec
spec = do
  test_list
  test_delete
  test_info
  test_timeSeries
  test_allTimeSeries 

test_list :: Spec
test_list = 
  describe "/tags/list.json" $
    it "list all tags created" $ do
       raw <- getEnv "MANDRILL_API_KEY"
       let key = ApiKey { _ApiKey =  Text.pack raw }
       resp <- Tags.list key
       resp `shouldSatisfy` isRight


test_delete :: Spec
test_delete = 
  describe "/tags/delete.json" $
    it "should delete a tag" $ do
       raw <- getEnv "MANDRILL_API_KEY"
       let key = ApiKey { _ApiKey =  Text.pack raw }
       resp <- Tags.delete key "hello"
       resp `shouldSatisfy` isRight


test_info :: Spec
test_info = 
  describe "/tags/info.json" $
    it "should return some info about a tag" $ do
       raw <- getEnv "MANDRILL_API_KEY"
       let key = ApiKey { _ApiKey =  Text.pack raw }
       resp <- Tags.info key "hello"
       resp `shouldSatisfy` isRight


test_timeSeries :: Spec
test_timeSeries = 
  describe "/tags/time-series.json" $
    it "should return a list of stats for a certain tag" $ do
       raw <- getEnv "MANDRILL_API_KEY"
       let key = ApiKey { _ApiKey =  Text.pack raw }
       resp <- Tags.timeSeries key "hello"
       resp `shouldSatisfy` isRight


test_allTimeSeries :: Spec
test_allTimeSeries = 
  describe "/tags/all-time-series.json" $
    it "should return stats for all tags" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Tags.timeSeries key "hello"
      resp `shouldSatisfy` isRight
