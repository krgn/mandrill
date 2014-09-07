module Network.Mandrill.TagsSpec where

import Test.Hspec

-- import Network.Mandrill.Tags

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

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
    it "" $ do
      --response <- info key
      pending

test_delete :: Spec
test_delete = 
  describe "/tags/delete.json" $
    it "" $ do
      --response <- info key
      pending

test_info :: Spec
test_info = 
  describe "/tags/info.json" $
    it "" $ do
      --response <- info key
      pending

test_timeSeries :: Spec
test_timeSeries = 
  describe "/tags/time-series.json" $
    it "" $ do
      --response <- info key
      pending

test_allTimeSeries :: Spec
test_allTimeSeries = 
  describe "/tags/all-time-series.json" $
    it "" $ do
      --response <- info key
      pending

