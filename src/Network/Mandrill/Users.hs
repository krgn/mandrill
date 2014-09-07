{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Users where

import Data.Aeson
import Network.Mandrill.ApiError
import Network.Mandrill.Types
import Network.Mandrill.Utils
import Data.API.Tools.Mandrill

import qualified Data.ByteString.Lazy as LBS

testKey :: ApiKey
testKey = "b0c5wPDu1J9q_7MqPYAqBg"

info :: ApiKey -> IO (Either ApiError User)
info key = do
  let obj = encode (object ["key" .= key])
  resp <- performRequest "/users/info.json" obj
  return $ parseResponse resp

ping :: ApiKey -> IO LBS.ByteString
ping key = do
  let obj = encode (object ["key" .= key])
  performRequest "/users/ping.json" obj

senders :: ApiKey -> IO [Sender]
senders = undefined
