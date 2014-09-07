module Network.Mandrill.ExportsSpec where

import Test.Hspec

--import Network.Mandrill.Exports

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

spec :: Spec
spec = do
  test_info
  test_list
  test_rejects
  test_whitelist
  test_activity

test_activity :: Spec
test_activity = 
  describe "/exports/activity.json" $
    it "" $ do
      --response <- info key
      pending

test_whitelist :: Spec
test_whitelist = 
  describe "/exports/whitelist.json" $
    it "" $ do
      --response <- info key
      pending

test_rejects :: Spec
test_rejects = 
  describe "/exports/rejects.json" $
    it "" $ do
      --response <- info key
      pending

test_info :: Spec
test_info = 
  describe "/exports/info.json" $
    it "" $ do
      --response <- info key
      pending

test_list :: Spec
test_list = 
  describe "/exports/list.json" $
    it "" $ do
      --response <- info key
      pending
