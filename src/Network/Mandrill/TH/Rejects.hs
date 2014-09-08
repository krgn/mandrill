{-# LANGUAGE QuasiQuotes #-}

module Network.Mandrill.TH.Rejects where

import Data.API.Parse
import Data.API.Types

rejectsApi :: API
rejectsApi = [api|

rjctrq :: RejectRequest
  // a reject request
  = record 
    key             :: ApiKey
    email           :: Email
    comment         :: string
    subaccount      :: Subaccount      
    include_expired :: ? boolean

rjct :: Reject 
  // a rejecrt response
  = record 
    email         :: Email
    deleted       :: ? boolean
    reason        :: ? RejectReason
    detail        :: ? string
    created_at    :: ? string
    last_event_at :: ? string
    expires_at    :: ? string
    expired       :: ? string 
    sender        :: ? Stat 
    subaccount    :: ? Subaccount
    _added        :: ? boolean

|]
