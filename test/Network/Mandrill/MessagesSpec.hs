{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.MessagesSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib

import           Network.Mandrill.Types
import qualified Network.Mandrill.Messages as Messages
import           System.Environment
import qualified Data.Text as Text 


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
    it "should send a message with valid key" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }

      let rcpt = TO_single Recipient 
                   { _recipient_email = "karsten@null2.net"
                   , _recipient_name = "lotta luft"
                   , _recipient_type = Nothing
                   }

          msg = def { _msg_to      = rcpt 
                    , _msg_subject = "test"
                    } :: Message

          cfg = MessageConfig {
                  _conf_async   = False
                , _conf_ip_pool = ""
                , _conf_send_at = ""
                }

      resp <- Messages.send key msg cfg
      resp `shouldSatisfy` isRight


test_sendTemplate :: Spec
test_sendTemplate = 
  describe "/messages/send-template.json" $
    it "should send a template message with valid key" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }

      let rcpt = TO_single 
                    Recipient { _recipient_email = "karsten@null2.net"
                              , _recipient_name  = "lotta luft"
                              , _recipient_type  = Nothing }

          msg = def { _msg_to      = rcpt 
                    , _msg_subject = "test"
                    } :: Message

          cfg = MessageConfig { _conf_async   = False
                              , _conf_ip_pool = ""
                              , _conf_send_at = "" }

          tmpl = "test"

      resp <- Messages.sendTmpl key msg cfg tmpl []
      resp `shouldSatisfy` isRight

test_search :: Spec
test_search = 
  describe "/messages/search.json" $
    it "should return some search results" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }

      let q  = "karsten@null2.net"
          f  = "2013-01-01"
          t  = "2013-01-03"
          ts = []
          ss = []
          ks = []
      resp <- Messages.search key q f t ts ss ks 10
      resp `shouldSatisfy` isRight

test_searchTimeSeries :: Spec
test_searchTimeSeries = 
  describe "/messages/search-time-series.json" $
    it "should return a list of statistics" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }

      let q  = "karsten@null2.net"
          f  = "2013-01-01"
          t  = "2013-01-03"
          ts = []
          ss = []
          ks = []
      resp <- Messages.search key q f t ts ss ks 10
      resp `shouldSatisfy` isRight


test_info :: Spec
test_info = 
  describe "/messages/info.json" $
    it "should return a message when queried" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }

      resp <- Messages.info key "sdf" 
      resp `shouldSatisfy` isRight


test_content :: Spec
test_content = 
  describe "/messages/content.json" $
    it "should return some content" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }

      resp <- Messages.content key $ "sdf"
      resp `shouldSatisfy` isRight

test_parse :: Spec
test_parse = 
  describe "/messages/parse.json" $
    it "should return a message for our raw string" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
          msg = "hahaha"

      resp <- Messages.parse key msg
      resp `shouldSatisfy` isRight


test_sendRaw :: Spec
test_sendRaw = 
  describe "/messages/send-raw.json" $
    it "should send off a raw message" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }
          msg = "hahaha"
          email = "karsten@kurt.net"
          n    = "karsten kurt"
          to = TO_single $ Recipient { _recipient_email = "karsten@null2.net"
                                     , _recipient_name  = "karste krut"
                                     , _recipient_type  = Nothing }
          cfg = MessageConfig {
                  _conf_async   = False
                , _conf_ip_pool = ""
                , _conf_send_at = ""
                }
      resp <- Messages.sendRaw key msg email n to cfg
      resp `shouldSatisfy` isRight


test_listScheduled :: Spec
test_listScheduled = 
  describe "/messages/list-scheduled.json" $
    it "should list a scheduled email" $ do
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
