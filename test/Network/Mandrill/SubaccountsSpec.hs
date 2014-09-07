module Network.Mandrill.SubaccountsSpec where

import Test.Hspec

-- import Network.Mandrill.Subaccounts

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

spec :: Spec
spec = do
  test_list
  test_add
  test_info
  test_update
  test_delete
  test_pause
  test_resume

test_resume :: Spec
test_resume = 
  describe "/subaccounts/resume.json" $
    it "" $ do
      --response <- info key
      pending

test_pause :: Spec
test_pause = 
  describe "/subaccounts/pause.json" $
    it "" $ do
      --response <- info key
      pending

test_delete :: Spec
test_delete = 
  describe "/subaccounts/delete.json" $
    it "" $ do
      --response <- info key
      pending

test_update :: Spec
test_update = 
  describe "/subaccounts/update.json" $
    it "" $ do
      --response <- info key
      pending

test_info :: Spec
test_info = 
  describe "/subaccounts/info.json" $
    it "" $ do
      --response <- info key
      pending

test_add :: Spec
test_add = 
  describe "/subaccounts/add.json" $
    it "" $ do
      --response <- info key
      pending

test_list :: Spec
test_list = 
  describe "/subaccounts/list.json" $
    it "" $ do
      --response <- info key
      pending
