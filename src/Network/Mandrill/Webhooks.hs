{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Webhooks where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

list :: ApiKey -> IO (Either ApiError [Webhook])
list k = do
     resp <- performRequest "/webhooks/list.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k ]

add :: ApiKey -> Url -> Description -> [MessageEvent] -> IO (Either ApiError Webhook)
add k u d es = do
    resp <- performRequest "/webhooks/add.json" mkObj
    return $ parseResponse resp
    where mkObj = encode $ object [ "key"         .= k
                                  , "url"         .= u
                                  , "description" .= d 
                                  , "events"      .= es ]

info :: ApiKey -> HookId -> IO (Either ApiError Webhook)
info k i = do
     resp <- performRequest "/webhooks/info.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k, "id" .= i ]

update :: ApiKey -> HookId -> Url -> Description -> [MessageEvent] -> IO (Either ApiError Webhook)
update k i u d es = do
       resp <- performRequest "/webhooks/update.json" mkObj
       return $ parseResponse resp
       where mkObj = encode $ object [ "key"         .= k
                                     , "id"          .= i
                                     , "url"         .= u
                                     , "description" .= d 
                                     , "events"      .= es ]

delete :: ApiKey -> HookId -> IO (Either ApiError Webhook)
delete k i = do
       resp <- performRequest "/webhooks/delete.json" mkObj
       return $ parseResponse resp
       where mkObj = encode $ object [ "key" .= k
                                     , "id"  .= i ]

