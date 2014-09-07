{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Utils where

import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Conduit

-- | performRequest is the central function for making API calls
-- and returning the result json
performRequest :: String -> LBS.ByteString -> IO LBS.ByteString
performRequest endpoint obj = do
  initReq <- parseUrl (baseUrl ++ endpoint) 

  let request = initReq {
        method = "POST",
        checkStatus = \ _ _ _ -> Nothing,
        requestBody = RequestBodyLBS obj
      }

  withManager $ \ manager -> do
    resp <- httpLbs request manager
    return $ responseBody resp

  where baseUrl = "https://mandrillapp.com/api/1.0"

