{-# LANGUAGE QuasiQuotes #-}

module Network.Mandrill.TH.Users where

import Data.API.Parse
import Data.API.Types

users :: API
users = [api|

user :: User
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
  // A record for stats about Mandrill user
  = record 
    sent          :: integer
    hard_bounces  :: integer
    soft_bounces  :: integer
    rejects       :: integer
    complaints    :: integer
    unsubs        :: integer
    opens         :: integer
    unique_opens  :: integer
    clicks        :: integer
    unique_clicks :: integer

stats :: Stats
  // A struct of stats by time
  = record
    today        :: Stat
    last_7_days  :: Stat
    last_30_days :: Stat
    last_60_days :: Stat
    last_90_days :: Stat
    all_time     :: Stat

sender :: Sender
  // A record for the senders that have tried to use this account
  // both verified and unverified
  = record 
    address       :: Email
    created_at    :: utc
    sent          :: integer
    hard_bounces  :: integer
    soft_bounces  :: integer
    rejects       :: integer
    complaints    :: integer
    unsubs        :: integer
    opens         :: integer
    clicks        :: integer
    unique_opens  :: integer
    unique_clicks :: integer
|]
