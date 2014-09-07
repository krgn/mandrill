module Network.Mandrill.MessagesSpec where

import Test.Hspec

-- import Network.Mandrill.Messages

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

spec :: Spec
spec = do
  test_send
  test_sendTemplate
  test_search
  test_searchTimeSeries
  test_info
  test_content
  test_parse
  test_sendRaw
  test_listScheduled
  test_cancelScheduled
  test_reschedule 

test_send :: Spec
test_send = 
  describe "/messages/send.json" $
    it "" $ do
      --response <- info key
      pending

test_sendTemplate :: Spec
test_sendTemplate = 
  describe "/messages/send-template.json" $
    it "" $ do
      --response <- info key
      pending

test_search :: Spec
test_search = 
  describe "/messages/search.json" $
    it "" $ do
      --response <- info key
      pending

test_searchTimeSeries :: Spec
test_searchTimeSeries = 
  describe "/messages/search-time-series.json" $
    it "" $ do
      --response <- info key
      pending

test_info :: Spec
test_info = 
  describe "/messages/info.json" $
    it "" $ do
      --response <- info key
      pending

test_content :: Spec
test_content = 
  describe "/messages/content.json" $
    it "" $ do
      --response <- info key
      pending

test_parse :: Spec
test_parse = 
  describe "/messages/parse.json" $
    it "" $ do
      --response <- info key
      pending

test_sendRaw :: Spec
test_sendRaw = 
  describe "/messages/send-raw.json" $
    it "" $ do
      --response <- info key
      pending

test_listScheduled :: Spec
test_listScheduled = 
  describe "/messages/list-scheduled.json" $
    it "" $ do
      --response <- info key
      pending

test_cancelScheduled :: Spec
test_cancelScheduled = 
  describe "/messages/cancel-scheduled.json" $
    it "" $ do
      --response <- info key
      pending

test_reschedule :: Spec
test_reschedule = 
  describe "/messages/reschedule.json" $
    it "" $ do
      --response <- info key
      pending
