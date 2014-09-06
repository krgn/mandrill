{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Utils where

import Data.API.JSON
import qualified Data.ByteString.Lazy as LBS
import Network.Mandrill.Types
import Network.HTTP.Conduit
import Network.Mandrill.ApiError

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
 
-- class (FromJSONWithErrs a) => MandrillResponse a where
--   parseResponse :: (FromJSONWithErrs a) => LBS.ByteString -> Either ApiError a
-- 
-- instance MandrillResponse User where
--   parseResponse resp =
--     case decodeWithErrs resp :: Either [(JSONError, Position)] User of
--      Right u -> Right u
--      Left  _ ->
--        case decodeWithErrs resp :: Either [(JSONError, Position)] ApiError of
--         Right e -> Left e
--         Left  _ -> Left ApiError {
--             _apierror_status  = "crazyerror",
--             _apierror_code    = 666, 
--             _apierror_name    = "Crazy_Error",
--             _apierror_message = "Uh oh, not good."
--           }
