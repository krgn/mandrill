{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.MetadataSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.Mandrill.Types
import qualified Data.Text                 as Text 
import qualified Network.Mandrill.Metadata as Metadata
import           System.Environment

spec :: Spec
spec = do
  test_list
  test_add
  test_update
  test_delete  

test_delete :: Spec
test_delete = 
  describe "/metadata/delete.json" $
    it "should delete a metadata entry" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Metadata.delete key "meta-1"
      resp `shouldSatisfy` isRight

test_update :: Spec
test_update = 
  describe "/metadata/update.json" $
    it "should update a piece of metadata" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
          met = Metadata { _metadata_name = "hello"
                         , _metadata_state = STATE_active 
                         , _metadata_view_template = "bla"
                         }

      resp <- Metadata.update key met
      resp `shouldSatisfy` isRight

test_add :: Spec
test_add = 
  describe "/metadata/add.json" $
    it "should add a metadata field" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
          met = Metadata { _metadata_name = "hello"
                         , _metadata_state = STATE_active 
                         , _metadata_view_template = "bla"
                         }

      resp <- Metadata.add key met
      resp `shouldSatisfy` isRight

test_list :: Spec
test_list = 
  describe "/metadata/list.json" $
    it "should list all metadata fields[" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
      resp <- Metadata.list key 
      resp `shouldSatisfy` isRight
