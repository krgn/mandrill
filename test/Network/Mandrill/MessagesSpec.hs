{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.MessagesSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib

import           Network.Mandrill.Types
import qualified Network.Mandrill.Messages as Messages
import qualified Network.Mandrill.Templates as Templates
import           System.Environment
import qualified Data.Text as Text 
import           Data.Time.Clock


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
      now <- getCurrentTime
      let key = ApiKey { _ApiKey =  Text.pack raw }
      let rcpt = TO_single Recipient 
                   { _recipient_email = "karsten@null2.net"
                   , _recipient_name = "lotta luft"
                   , _recipient_type = Nothing
                   }

          msg = def { _msg_to      = Just rcpt 
                    , _msg_subject = Just "test"
                    } :: Message

          cfg = MessageConfig {
                  _conf_async   = False
                , _conf_ip_pool = ""
                , _conf_send_at = Just TimeStamp { _ts_utctime = Just now }
                }


      resp <- Messages.send key msg cfg
      resp `shouldSatisfy` isRight


test_sendTemplate :: Spec
test_sendTemplate = 
  describe "/messages/send-template.json" $
    it "should send a template message with valid key" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      now <- getCurrentTime
      let key = ApiKey { _ApiKey =  Text.pack raw }
          tmpl = def { _tmpl_name = "test" }

      _ <- Templates.add key tmpl True 

      let rcpt = TO_single 
                    Recipient { _recipient_email = "karsten@null2.net"
                              , _recipient_name  = "lotta luft"
                              , _recipient_type  = Nothing }

          msg = def { _msg_to      = Just rcpt 
                    , _msg_subject = Just "test"
                    } :: Message

          cfg = MessageConfig { _conf_async   = False
                              , _conf_ip_pool = ""
                              , _conf_send_at = Just TimeStamp { _ts_utctime = Just now } } 

      resp <- Messages.sendTmpl key msg cfg (_tmpl_name tmpl)  []
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

      let q  = "karsten@null2.net"
          f  = "2013-01-01"
          t  = "2016-01-03"
          ts = []
          ss = []
          ks = []

      query <- Messages.search key q f t ts ss ks 10

      let vals = case query of
                   Right rs -> rs
                   Left _ -> []

      resp <- Messages.info key (_result__id $ head vals)
      resp `shouldSatisfy` isRight


test_content :: Spec
test_content = 
  describe "/messages/content.json" $
    it "should return entire content of a sent message" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      let key = ApiKey { _ApiKey =  Text.pack raw }

      let q  = "karsten@null2.net"
          f  = "2013-01-01"
          t  = "2016-01-03"
          ts = []
          ss = []
          ks = []
      query <- Messages.search key q f t ts ss ks 10
      let vals = case query of
                   Right rs -> rs
                   Left _ -> []

      resp <- Messages.content key (_result__id $ head vals)
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
      now <- getCurrentTime
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
                , _conf_send_at = Just TimeStamp { _ts_utctime = Just now }
                }
      resp <- Messages.sendRaw key msg email n to cfg
      resp `shouldSatisfy` isRight


test_listScheduled :: Spec
test_listScheduled = 
  describe "/messages/list-scheduled.json" $
    it "should list a scheduled email" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      now <- getCurrentTime
      let key = ApiKey { _ApiKey =  Text.pack raw }
          email = "karsten@kurt.net"

          rcpt = TO_single Recipient 
                   { _recipient_email = "karsten@null2.net"
                   , _recipient_name = "lotta luft"
                   , _recipient_type = Nothing
                   }

          msg = def { _msg_to      = Just rcpt 
                    , _msg_subject = Just "test"
                    } :: Message

          cfg = MessageConfig {
                  _conf_async   = False
                , _conf_ip_pool = ""
                , _conf_send_at = Just TimeStamp { _ts_utctime = Just now }
                }

      _ <- Messages.send key msg cfg

      resp <- Messages.listScheduled key email
      resp `shouldSatisfy` isRight
       

test_cancelScheduled :: Spec
test_cancelScheduled = 
  describe "/messages/cancel-scheduled.json" $
    it "should cancel a scheduled message" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      now <- getCurrentTime
      let key = ApiKey { _ApiKey =  Text.pack raw }
          rcpt = TO_single Recipient 
                   { _recipient_email = "karsten@null2.net"
                   , _recipient_name = "lotta luft"
                   , _recipient_type = Nothing
                   }

          msg = def { _msg_to      = Just rcpt 
                    , _msg_from_email = Just "karsten@kurt.net"
                    , _msg_subject = Just "test"
                    } :: Message

          cfg = MessageConfig {
                  _conf_async   = False
                , _conf_ip_pool = ""
                , _conf_send_at = Just TimeStamp { _ts_utctime = Just now }
                }

      d <- Messages.send key msg cfg

      let st = case d of
                 Right v -> v
                 Left _  -> []

      resp <- Messages.cancelScheduled key (_delivery__id $ head st)
      resp `shouldSatisfy` isRight

test_reschedule :: Spec
test_reschedule = 
  describe "/messages/reschedule.json" $
    it "should re-schedule a scheduled email" $ do
      raw <- getEnv "MANDRILL_API_KEY"
      now <- getCurrentTime
      let key = ApiKey { _ApiKey =  Text.pack raw }
          email = "karsten@kurt.net"
      resp <- Messages.reschedule key email TimeStamp { _ts_utctime = Just now }
      resp `shouldSatisfy` isRight
