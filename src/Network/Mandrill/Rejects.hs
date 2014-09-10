{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Rejects where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

add :: ApiKey     -> 
      Email      -> 
      Comment    -> 
      Subaccount ->  
      IO (Either ApiError Reject)
add k e c a = do
    resp <- performRequest "/rejects/add.json" mkObj
    return $ parseResponse resp
    where mkObj = encode $ object [ "key"        .= k
                                  , "email"      .= e 
                                  , "comment"    .= c 
                                  , "subaccount" .= a ]

list :: ApiKey     -> 
       Email      -> 
       IncExpired -> 
       Subaccount -> 
       IO (Either ApiError Reject)
list k e i s = do
     resp <- performRequest "/rejects/list.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key"             .= k 
                                   , "email"           .= e
                                   , "include_expired" .= i 
                                   , "subaccount"      .= s ]

delete :: ApiKey     -> 
         Email      -> 
         Subaccount -> 
         IO (Either ApiError Reject)
delete k e s = do
       resp <- performRequest "/rejects/delete.json" mkObj
       return $ parseResponse resp
       where mkObj = encode $ object [ "key"        .= k 
                                     , "email"      .= e
                                     , "subaccount" .= s]
