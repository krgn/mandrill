{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Whitelists where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

add :: ApiKey -> 
      Email -> 
      Comment -> 
      IO (Either ApiError Whitelist)
add k e c = do
    resp <- performRequest "/whitelists/add.json" mkObj
    return $ parseResponse resp
    where mkObj = encode $ object [ "key"     .= k 
                                  , "email"   .= e 
                                  , "comment" .= c]

list :: ApiKey -> Email -> IO (Either ApiError Whitelist)
list k e = do
     resp <- performRequest "/whitelists/list.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object ["key" .= k, "email" .= e]

delete :: ApiKey -> Email -> IO (Either ApiError Whitelist) 
delete k e = do
       resp <- performRequest "/whitelists/delete.json" mkObj
       return $ parseResponse resp
       where mkObj = encode $ object ["key" .= k, "email" .= e]
