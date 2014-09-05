{-# LANGUAGE QuasiQuotes #-}

module Network.Mandrill.TH.Users where

import Data.API.Parse
import Data.API.Types

users :: API
users = [api|

user :: User
     // A record for a user
     = record 
       name        :: string
       createdAt   :: utc
       publicId    :: string
       reputation  :: integer
       hourlyQuota :: integer
       backlog     :: integer
       stats       :: Stats

stat :: Stat
     // A record for stats about Mandrill user
     = record 
       sent         :: integer
       hardBounces  :: integer
       softBounces  :: integer
       rejects      :: integer
       complaints   :: integer
       unsubs       :: integer
       opens        :: integer
       uniqueOpens  :: integer
       clicks       :: integer
       uniqueClicks :: integer

stats :: Stats
      // A struct of stats by time
      = record
        today      :: Stat
        last7Days  :: Stat
        last30Days :: Stat
        last60Days :: Stat
        last90Days :: Stat
        allTime    :: Stat

senders :: Senders
        // A record for the senders that have tried to use this account
        // both verified and unverified
        = record 
          address      :: Email
          createdAt    :: utc
          sent         :: integer
          hardBounces  :: integer
          softBounces  :: integer
          rejects      :: integer
          complaints   :: integer
          unsubs       :: integer
          opens        :: integer
          clicks       :: integer
          uniqueOpens  :: integer
          uniqueClicks :: integer
|]
