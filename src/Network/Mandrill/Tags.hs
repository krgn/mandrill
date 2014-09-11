{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Tags where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

-- | Return all of the user-defined tag information
list :: ApiKey -> IO (Either ApiError [Stat])
list key =
     performRequest "/tags/list.json" $
       object [ "key" .= key ]

-- | Return more detailed information about a single tag, including aggregates of recent stats
info :: ApiKey -> 
       Tag ->
       IO (Either ApiError Stat)
info key tag =
     performRequest "/tags/info.json" $
       object [ "key" .= key
              , "tag" .= tag ]

-- | Deletes a tag permanently. Deleting a tag removes the tag from any 
-- messages that have been sent, and also deletes the tag's stats. 
-- There is no way to undo this operation, so use it carefully.
delete :: ApiKey -> Tag -> IO (Either ApiError Stat)
delete key tag =
       performRequest "/tags/delete.json" $
         object [ "key" .= key
                , "tag" .= tag ]

-- | Return the recent history (hourly stats for the last 30 days) for a tag
timeSeries :: ApiKey -> Tag -> IO (Either ApiError Stat)
timeSeries key tag =
           performRequest "/tags/time-series.json" $
             object [ "key" .= key
                    , "tag" .= tag ]

-- | Return the recent history (hourly stats for the last 30 days) for all tags
allTimeSeries :: ApiKey -> IO (Either ApiError Stat)
allTimeSeries key = 
              performRequest "/tags/time-series.json" $
                object [ "key" .= key ]
