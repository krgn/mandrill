{-# LANGUAGE QuasiQuotes #-}

module Network.Mandrill.TH.Users where

import Data.API.Parse
import Data.API.Types

users :: API
users = [api|

usr :: User
  // A record for a user
  = record 
    username     :: string
    created_at   :: string
    public_id    :: string
    reputation   :: integer
    hourly_quota :: integer
    backlog      :: integer
    stats        :: Stats

stat :: Stat
  // a stats record 
  = record 
    time          :: ? string
    tag           :: ? Tag
    reputation    :: ? Count
    address       :: ? Email
    created_at    :: ? utc
    sent          :: Count
    hard_bounces  :: Count
    soft_bounces  :: Count
    rejects       :: Count
    complaints    :: Count
    unsubs        :: Count
    opens         :: Count
    unique_opens  :: Count
    clicks        :: Count
    unique_clicks :: Count
    stats         :: ? Stats

usrstats :: Stats
  // A struct of stats by time
  = record
    today        :: Stat
    last_7_days  :: Stat
    last_30_days :: Stat
    last_60_days :: Stat
    last_90_days :: Stat
    all_time     :: Stat

|]
