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

sndr :: Sender
  // a stats record 
  = record 
    address       :: Email
    created_at    :: string
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

dr :: DomainRecord
  // spf or dkim record info of a Domain
  = record
    valid       :: boolean
    valid_after :: string
    error       :: string

dmn :: Domain
  // a domain
  = record
    domain         :: Url
    created_at     :: string
    last_tested_at :: string
    spf            :: DomainRecord
    dkim           :: DomainRecord
    verified_at    :: string
    valid_signing  :: boolean 

urlRec :: UrlRecord 
  // an url record with click stats
  = record 
    url           :: ? Url
    time          :: ? string
    sent          :: Count
    clicks        :: Count
    unique_clicks :: Count

trkdmn :: TrackingDomain 
  // a tracking domain
  = record 
    domain :: Url 
    created_at :: string
    last_tested_at :: string
    cname :: DomainRecord
    valid_tracking :: boolean

lbl :: Label
  // a template label
  = string

tmpl :: Template
  // an email template
  = record 
    slug               :: string 
    name               :: string
    labels             :: [Label]
    code               :: string
    subject            :: string
    from_email         :: Email
    from_name          :: string
    text               :: string 
    publish_name       :: string
    publish_code       :: string
    publish_subject    :: string
    publish_from_email :: Email
    publish_from_name  :: string
    publish_text       :: string
    published_at       :: string
    created_at         :: string
    updated_at         :: string

athk :: AuthKey
  // an auth key synonym
  = string
  
msgevt :: MessageEvent
  // an event in the message lifecycle
  = enum 
    | send
    | hard_bounce
    | soft_bounce
    | open
    | click
    | spam
    | unsub
    | reject

wbhk :: Webhook
  // a webhook
  = record 
    id           :: ? integer
    url          :: Url
    description  :: string 
    auth_key     :: AuthKey
    events       :: [MessageEvent]
    created_at   :: string
    last_sent_at :: string
    batches_sent :: Count
    events_sent  :: Count
    last_error   :: string

acctst :: AccountStatus
  // enum for status
  = enum
    | active
    | paused

sbacct :: Subaccount
  // a subaccount
  = record
    id            :: ? integer
    name          :: string
    custom_quota  :: Count
    hourly_quota  :: ? Count
    status        :: AccountStatus
    reputation    :: Count
    created_at    :: string
    first_sent_at :: string
    send_hourly   :: ? Count
    sent_weekly   :: Count 
    sent_monthly  :: Count
    sent_total    :: Count
    last_30_days  :: ? [Stat] 

|]
