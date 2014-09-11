{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Urls where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

list :: ApiKey -> IO (Either ApiError [UrlRecord])
list k = do 
     resp <- performRequest "/urls/list.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k ]

search :: ApiKey -> Query -> IO (Either ApiError [UrlRecord])
search k q = do 
       resp <- performRequest "/urls/search.json" mkObj
       return $ parseResponse resp
       where mkObj = encode $ object [ "key" .= k, "q" .= q ]

timeSeries :: ApiKey -> Url -> IO (Either ApiError [Stat])
timeSeries k u = do
           resp <- performRequest "/urls/time-series.json" mkObj
           return $ parseResponse resp
           where mkObj = encode $ object [ "key" .= k, "url" .= u ]


trackingDomains :: ApiKey -> IO (Either ApiError [TrackingDomain])
trackingDomains k = do
                resp <- performRequest "/urls/tracking-domains.json" mkObj
                return $ parseResponse resp
                where mkObj = encode $ object [ "key" .= k ]

addTrackingDomain :: ApiKey -> Url -> IO (Either ApiError TrackingDomain)
addTrackingDomain k d = do
                  resp <- performRequest "/ursl/add-tracking-domain.json" mkObj
                  return $ parseResponse resp
                  where mkObj = encode $ object [ "key" .= k, "domain" .= d ]

checkTrackingDomain :: ApiKey -> Url -> IO (Either ApiError TrackingDomain)
checkTrackingDomain k d = do
                  resp <- performRequest "/ursl/add-tracking-domain.json" mkObj
                  return $ parseResponse resp
                  where mkObj = encode $ object [ "key" .= k, "domain" .= d ]
