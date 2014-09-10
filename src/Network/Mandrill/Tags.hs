{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Tags where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

list :: ApiKey -> IO (Either ApiError [Stat])
list key = do
     resp <- performRequest "/tags/list.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= key ]

info :: ApiKey -> 
       Tag ->
       IO (Either ApiError Stat)
info key tag  = do
     resp <- performRequest "/tags/info.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $
                   object [ "key" .= key
                          , "tag" .= tag ]

delete :: ApiKey -> Tag -> IO (Either ApiError Stat)
delete key tag = do
       resp <- performRequest "/tags/delete.json" mkObj
       return $ parseResponse resp
       where mkObj = encode $ object [ "key" .= key
                                     , "tag" .= tag ]

timeSeries :: ApiKey -> Tag -> IO (Either ApiError Stat)
timeSeries key tag = do
           resp <- performRequest "/tags/time-series.json" mkObj
           return $ parseResponse resp
           where mkObj = encode $ object [ "key" .= key
                                         , "tag" .= tag ]

allTimeSeries :: ApiKey -> IO (Either ApiError Stat)
allTimeSeries key = do
              resp <- performRequest "/tags/time-series.json" mkObj
              return $ parseResponse resp
              where mkObj = encode $ object [ "key" .= key ]
