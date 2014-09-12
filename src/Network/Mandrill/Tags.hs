{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Tags where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

-- | Return all of the user-defined tag information
list :: (MonadIO m) => MandrillT m (Either ApiError [Stat])
list = performRequest "/tags/list.json" []

-- | Return more detailed information about a single tag, including aggregates of recent stats
info :: (MonadIO m) => 
       Tag         ->
       MandrillT m (Either ApiError Stat)
info tag = performRequest "/tags/info.json" ["tag" .= tag]

-- | Deletes a tag permanently. Deleting a tag removes the tag from any 
-- messages that have been sent, and also deletes the tag's stats. 
-- There is no way to undo this operation, so use it carefully.
delete :: (MonadIO m) => 
          Tag -> 
          MandrillT m (Either ApiError Stat)
delete tag = performRequest "/tags/delete.json" ["tag" .= tag]

-- | Return the recent history (hourly stats for the last 30 days) for a tag
timeSeries :: (MonadIO m) => 
             Tag -> 
             MandrillT m (Either ApiError Stat)
timeSeries tag = performRequest "/tags/time-series.json" ["tag" .= tag]

-- | Return the recent history (hourly stats for the last 30 days) for all tags
allTimeSeries :: (MonadIO m) => MandrillT m (Either ApiError Stat)
allTimeSeries = performRequest "/tags/time-series.json" []
