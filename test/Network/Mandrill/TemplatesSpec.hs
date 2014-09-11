{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.TemplatesSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.Mandrill.Types
import qualified Data.Text                  as Text 
import qualified Network.Mandrill.Templates as Templates
import           System.Environment

spec :: Spec
spec = do
  test_add
  test_info
  test_update
  test_publish
  test_delete
  test_list
  test_timeSeries
  test_render

test_add :: Spec
test_add = 
  describe "/templates/add.json" $
    it "should add the template" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Templates.add key def { _tmpl_name = "test1" } False
      resp `shouldSatisfy` isRight

test_timeSeries :: Spec
test_timeSeries = 
  describe "/templates/time-series.json" $
    it "should return some stats for a template" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Templates.timeSeries key "test1"
      resp `shouldSatisfy` isRight

test_render :: Spec
test_render = 
  describe "/templates/render.json" $
    it "should render a template to html" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Templates.render key "test1" [] []
      resp `shouldSatisfy` isRight

test_delete :: Spec
test_delete = 
  describe "/templates/delete.json" $
    it "should delete a template" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Templates.delete key "test1"
      resp `shouldSatisfy` isRight

test_publish :: Spec
test_publish = 
  describe "/templates/publish.json" $
    it "should publish a template" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Templates.publish key "test1"
      resp `shouldSatisfy` isRight

test_update :: Spec
test_update = 
  describe "/templates/update.json" $
    it "should update a template" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Templates.publish key "test1"
      resp `shouldSatisfy` isRight

test_info :: Spec
test_info = 
  describe "/templates/info.json" $
    it "should return some info about a template" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Templates.info key "test1"
      resp `shouldSatisfy` isRight

test_list :: Spec
test_list = 
  describe "/templates/list.json" $
    it "should list all templates" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Templates.list key "test1"
      resp `shouldSatisfy` isRight
