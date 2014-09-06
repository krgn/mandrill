module Network.Mandrill.UsersSpec where

import Test.Hspec
-- import Control.Monad.IO.Class
-- import Test.QuickCheck
-- import Control.Exception (evaluate)

import Network.Mandrill.Users
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

spec :: Spec
spec = do
  test_info
  test_ping
  test_senders

test_info :: Spec
test_info = 
  describe "/users/info.json" $
    it "should return some user info upon valid request" $ do
      --response <- info key
      pending

test_ping :: Spec
test_ping = 
  describe "/users/ping.json" $
    it "should return pong upon valid request" $ do
      response <- ping key
      CBS.unpack (LBS.toStrict response) `shouldBe` "\"PONG!\""

test_senders :: Spec
test_senders = 
  describe "/users/senders.json" $
    it "should return a list of sender that have used this account" $
      pending
