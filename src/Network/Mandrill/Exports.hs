{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Exports where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

list :: ApiKey -> IO (Either ApiError [Export])
list k = do 
     resp <- performRequest "/exports/list.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k ]

info :: ApiKey -> ExportId -> IO (Either ApiError Export)
info k i = do
     resp <- performRequest "/exports/info.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k, "id" .= i ]

rejects :: ApiKey -> Email -> IO (Either ApiError Export)
rejects k e = do
        resp <- performRequest "/exports/rejects.json" mkObj
        return $ parseResponse resp
        where mkObj = encode $ object [ "key" .= k, "notify_email" .= e ]

whitelist :: ApiKey -> Email -> IO (Either ApiError Export)
whitelist k e = do
          resp <- performRequest "/exports/withlist.json" mkObj
          return $ parseResponse resp
          where mkObj = encode $ object [ "key" .= k, "notify_email" .= e ]

activity :: ApiKey -> Email -> From -> Thru -> [Tag] -> [Email] -> [SendStatus] -> [ApiKey] -> IO (Either ApiError Export)
activity k e f t ts ss sts ks = do
         resp <- performRequest "/exports/activity.json" mkObj
         return $ parseResponse resp
         where mkObj = encode $ object [ "key"          .= k
                                       , "notify_email" .= e
                                       , "date_from"    .= f
                                       , "date_to"      .= t
                                       , "tags"         .= ts
                                       , "senders"      .= ss
                                       , "states"       .= sts
                                       , "api_keys"     .= ks ]

