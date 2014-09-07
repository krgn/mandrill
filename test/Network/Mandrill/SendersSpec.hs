module Network.Mandrill.SendersSpec where

import Test.Hspec

-- import Network.Mandrill.Senders

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

spec :: Spec
spec = do
  test_list
  test_domains
  test_addDomain
  test_checkDomain
  test_info
  test_timeSeries

test_list :: Spec
test_list = 
  describe "/senders/list.json" $
    it "" $ do
      --response <- info key
      pending

test_domains :: Spec
test_domains = 
  describe "/senders/domains.json" $
    it "" $ do
      --response <- info key
      pending

test_addDomain :: Spec
test_addDomain = 
  describe "/senders/add-domain.json" $
    it "" $ do
      --response <- info key
      pending

test_checkDomain :: Spec
test_checkDomain = 
  describe "/senders/check-domain.json" $
    it "" $ do
      --response <- info key
      pending

test_info :: Spec
test_info = 
  describe "/senders/info.json" $
    it "" $ do
      --response <- info key
      pending

test_timeSeries :: Spec
test_timeSeries = 
  describe "/senders/time-series.json" $
    it "" $ do
      --response <- info key
      pending
