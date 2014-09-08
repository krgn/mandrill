{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Users
       ( info
       , ping
       , senders) where

import Data.Aeson
import Network.Mandrill.ApiError
import Network.Mandrill.Types
import Network.Mandrill.Utils
import Data.API.Tools.Mandrill

import qualified Data.ByteString.Lazy as LBS

info :: ApiKey -> IO (Either ApiError User)
info key = do
  resp <- performRequest "/users/info.json" (mkObj key)
  return $ parseResponse resp

ping :: ApiKey -> IO LBS.ByteString
ping key = do
  let obj = mkObj key
    in performRequest "/users/ping.json" obj

senders :: ApiKey -> IO (Either ApiError [Stat])
senders key = do
  resp <- performRequest "/users/senders.json" (mkObj key)
  return $ parseResponse resp

mkObj :: ApiKey -> LBS.ByteString
mkObj key = encode (object ["key" .= key])
