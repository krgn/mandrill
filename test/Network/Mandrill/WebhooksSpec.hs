module Network.Mandrill.WebhooksSpec where

import Test.Hspec

-- import Network.Mandrill.Webhooks

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

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
    it "" $ do
      --response <- info key
      pending

test_update :: Spec
test_update = 
  describe "/webhooks/update.json" $
    it "" $ do
      --response <- info key
      pending

test_info :: Spec
test_info = 
  describe "/webhooks/info.json" $
    it "" $ do
      --response <- info key
      pending

test_add :: Spec
test_add = 
  describe "/webhooks/add.json" $
    it "" $ do
      --response <- info key
      pending

test_list :: Spec
test_list = 
  describe "/webhooks/list.json" $
    it "" $ do
      --response <- info key
      pending
