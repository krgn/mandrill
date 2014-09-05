module Network.Mandrill.UsersSpec where

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)
 
-- import Network.Mandrill.Users

spec :: Spec
spec = do
  info
  ping
  senders

info :: Spec
info = 
  describe "Info" $
    it "should return some user info upon valid request" $
      pending

ping :: Spec
ping = 
  describe "Ping" $
    it "should return pong upon valid request" $
      pending


senders :: Spec
senders = 
  describe "Senders" $
    it "should return a list of sender that have used this account" $
      pending

