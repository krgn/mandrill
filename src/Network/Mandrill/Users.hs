{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Users
       ( info
       , ping
       , senders) where

import Data.Aeson
import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

import qualified Data.ByteString.Lazy as LBS


-- | Return the information about the API-connected `User`
info :: ApiKey -> IO (Either ApiError User)
info key = 
     performRequest "/users/info.json" $
       object ["key" .= key]


-- | Validate an API key and respond to a ping
ping :: ApiKey -> IO LBS.ByteString
ping key = 
     performRequest' "/users/ping.json" $
       encode $ object ["key" .= key]

-- | Return the senders that have tried to use this account, 
-- both verified and unverified
senders :: ApiKey -> IO (Either ApiError [Stat])
senders key = 
        performRequest "/users/senders.json" $
          object ["key" .= key]
