module Network.Mandrill.MetadataSpec where

import Test.Hspec

-- import Network.Mandrill.Metadata

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

spec :: Spec
spec = do
  test_list
  test_add
  test_update
  test_delete  

test_delete :: Spec
test_delete = 
  describe "/metadata/delete.json" $
    it "" $ do
      --response <- info key
      pending

test_update :: Spec
test_update = 
  describe "/metadata/update.json" $
    it "" $ do
      --response <- info key
      pending

test_add :: Spec
test_add = 
  describe "/metadata/add.json" $
    it "" $ do
      --response <- info key
      pending

test_list :: Spec
test_list = 
  describe "/metadata/list.json" $
    it "" $ do
      --response <- info key
      pending
