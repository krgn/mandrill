{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Urls where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

-- | Get the 100 most clicked URLs
list :: ApiKey -> IO (Either ApiError [UrlRecord])
list k =
     performRequest "/urls/list.json" $
       object [ "key" .= k ]


-- | Return the 100 most clicked URLs that match the search query given
search :: ApiKey -> Query -> IO (Either ApiError [UrlRecord])
search k q =
       performRequest "/urls/search.json" $ 
         object [ "key" .= k, "q" .= q ]


-- | Return the recent history (hourly stats for the last 30 days) for a url
timeSeries :: ApiKey -> Url -> IO (Either ApiError [Stat])
timeSeries k u =
           performRequest "/urls/time-series.json" $
             object [ "key" .= k, "url" .= u ]


-- | Get the list of tracking domains set up for this account
trackingDomains :: ApiKey -> IO (Either ApiError [TrackingDomain])
trackingDomains k =
                performRequest "/urls/tracking-domains.json" $ 
                  object [ "key" .= k ]


-- | Add a tracking domain to your account
addTrackingDomain :: ApiKey -> Url -> IO (Either ApiError TrackingDomain)
addTrackingDomain k d =
                  performRequest "/ursl/add-tracking-domain.json" $
                    object [ "key" .= k, "domain" .= d ]


-- | Checks the CNAME settings for a tracking domain. The domain must have been 
-- added already with the add-tracking-domain call
checkTrackingDomain :: ApiKey -> Url -> IO (Either ApiError TrackingDomain)
checkTrackingDomain k d =
                  performRequest "/ursl/add-tracking-domain.json" $
                    object [ "key" .= k, "domain" .= d ]
