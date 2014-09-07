module Network.Mandrill.WhitelistsSpec where

import Test.Hspec
-- import Network.Mandrill.Whitelists

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

spec :: Spec
spec = do
  test_add
  test_list
  test_delete

test_add :: Spec
test_add = 
  describe "/whitelists/add.json" $
    it "" $ do
      --response <- info key
      pending

test_list :: Spec
test_list = 
  describe "/whitelists/list.json" $
    it "" $ do
      --response <- info key
      pending

test_delete :: Spec
test_delete = 
  describe "/whitelists/delete.json" $
    it "" $ do
      --response <- info key
      pending
