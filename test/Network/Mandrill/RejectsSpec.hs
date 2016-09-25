{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.RejectsSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import           Network.Mandrill.Types
import           Network.Mandrill.Utils
import qualified Network.Mandrill.Rejects as Rejects
import qualified Data.Text                as Text 
import           System.Environment
 
spec :: Spec
spec = parallel $ do
  test_add
  test_list
  test_delete

test_add :: Spec
test_add = 
  describe "/rejects/add.json" $
    it "should add something to the blacklist" $ do
       raw <- getEnv "MANDRILL_API_KEY"
       resp <- runMandrill (ApiKey $ Text.pack raw) $
         Rejects.add  "hummel@hammel.de" "ain't no good" "default"
       resp `shouldSatisfy` isRight


test_list :: Spec
test_list = 
  describe "/rejects/list.json" $
    it "should list all entries in blacklist" $ do
       raw <- getEnv "MANDRILL_API_KEY"
       resp <- runMandrill (ApiKey $ Text.pack raw) $
         Rejects.list "hummel@hammel.de" True "default"
       resp `shouldSatisfy` isRight

test_delete :: Spec
test_delete = 
  describe "/rejects/delete.json" $
    it "should delete a blacklisted email" $ do
       raw <- getEnv "MANDRILL_API_KEY"
       resp <- runMandrill (ApiKey $ Text.pack raw) $
         Rejects.delete "hummel@hammel.de" "default"
       resp `shouldSatisfy` isRight
