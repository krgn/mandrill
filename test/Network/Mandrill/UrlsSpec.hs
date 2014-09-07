module Network.Mandrill.UrlsSpec where

import Test.Hspec

-- import Network.Mandrill.Urls

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

spec :: Spec
spec = do
  test_list
  test_search
  test_timeSeries
  test_trackingDomains
  test_addTrackingDomain
  test_checkTrackingDomain 

test_list :: Spec
test_list = 
  describe "/urls/list.json" $
    it "" $ do
      --response <- info key
      pending

test_search :: Spec
test_search = 
  describe "/urls/search.json" $
    it "" $ do
      --response <- info key
      pending

test_timeSeries :: Spec
test_timeSeries = 
  describe "/urls/time-series.json" $
    it "" $ do
      --response <- info key
      pending

test_trackingDomains :: Spec
test_trackingDomains = 
  describe "/urls/tracking-domains.json" $
    it "" $ do
      --response <- info key
      pending

test_addTrackingDomain :: Spec
test_addTrackingDomain = 
  describe "/urls/add-tracking-domain.json" $
    it "" $ do
      --response <- info key
      pending

test_checkTrackingDomain :: Spec
test_checkTrackingDomain = 
  describe "/urls/check-tracking-domain.json" $
    it "" $ do
      --response <- info key
      pending
