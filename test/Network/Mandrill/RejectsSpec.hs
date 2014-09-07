module Network.Mandrill.RejectsSpec where

import Test.Hspec

-- import Network.Mandrill.Rejects

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

spec :: Spec
spec = do
  test_add
  test_list
  test_delete

test_add :: Spec
test_add = 
  describe "/rejects/add.json" $
    it "" $ do
      --response <- info key
      pending

test_list :: Spec
test_list = 
  describe "/rejects/list.json" $
    it "" $ do
      --response <- info key
      pending

test_delete :: Spec
test_delete = 
  describe "/rejects/delete.json" $
    it "" $ do
      --response <- info key
      pending
