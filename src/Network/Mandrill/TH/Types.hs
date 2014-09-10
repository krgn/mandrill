{-# LANGUAGE QuasiQuotes #-}

module Network.Mandrill.TH.Types where

import Data.API.Parse
import Data.API.Types

mandrillApi :: API
mandrillApi = [api|

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
    created_at    :: ? string
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

tmpl :: Template
  // an email template
  = record 
    slug               :: ? string 
    name               ::   string
    labels             :: ? [Label]
    code               :: ? string
    subject            :: ? string
    from_email         :: ? Email
    from_name          :: ? string
    text               :: ? string 
    publish_name       :: ? string
    publish_code       :: ? string
    publish_subject    :: ? string
    publish_from_email :: ? Email
    publish_from_name  :: ? string
    publish_text       :: ? string
    published_at       :: ? string
    content            :: ? string
    created_at         :: ? string
    updated_at         :: ? string

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

indom :: InboundDomain
  // an inbound domain record
  = record 
    domain     :: Url
    created_at :: string
    valid_mx   :: boolean

inrt :: InboundRoute
  // an inbound route
  = record 
    id      :: string
    email   :: ? Email 
    pattern :: string
    url     :: Url 

expt :: Export
  // an export
  = record 
    id          :: string
    created_at  :: string
    finished_at :: ? string
    type        :: ExportType
    state       :: ExportState
    result_url  :: ? Url

rjctrq :: RejectRequest
  // a reject request
  = record 
    key             :: ApiKey
    email           :: Email
    comment         :: string
    subaccount      :: Subaccount      
    include_expired :: ? boolean

rjct :: Reject 
  // a reject
  = record 
    email         :: Email
    reason        :: RejectReason
    detail        :: string
    created_at    :: string
    last_event_at :: string
    expires_at    :: string
    expired       :: string 
    sender        :: Stat 
    subaccount    :: Subaccount

wht :: Whitelist
  // a whitelist entry
  = record 
    email      :: Email
    detail     :: string
    created_at :: string

dtl :: Detail
  // A stat containing information how often a messsage has been opened or stuff 
  // was clicked.
  = record 
    ts       :: utc
    url      :: ? Url
    ip       :: IpAddress  
    location :: string
    ua       :: string

msgmta :: MessageMetadata 
  // Message metadata
  = record 
    website :: string

rcptmeta :: RecipientMetadata
  // per recipient metadata 
  = record
    rcpt   :: Email
    values :: MessageMetadata

mvar :: MergeVar
  // A merge var record
  = record
    name    :: string
    content :: string

rcptmvar :: RecipientMergeVar
  // bla
  = record
    recipient :: Email
    vars      :: [MergeVar] 

to :: To
  // a way to encode the possibility of a single or multiple recipient addresses 
  = union
    | single   :: Recipient
    | multiple :: [Recipient]

cnf :: MessageConfig
  // config
  = record 
    async   :: Async
    ip_pool :: IpPool
    send_at :: SendAt 

msg :: Message
  // A message structure
  = record 
    ts                        :: ? utc
    _id                       :: ? MessageId
    html                      ::   string
    text                      ::   string
    subject                   ::   string
    from_email                ::   Email
    from_name                 ::   string
    to                        ::   To
    important                 :: ? boolean
    track_opens               :: ? boolean
    track_clicks              :: ? boolean
    auto_text                 :: ? boolean
    auto_html                 :: ? boolean
    inline_css                :: ? boolean
    url_strip_qs              :: ? boolean
    preserve_recipients       :: ? boolean
    view_content_link         :: ? boolean
    bcc_address               :: ? Email
    tracking_domain           :: ? string
    signing_domain            :: ? string
    return_path_domain        :: ? string
    merge                     :: ? boolean
    global_merge_vars         :: ? [MergeVar]
    merge_vars                :: ? [RecipientMergeVar]
    tags                      ::   [Tag]
    subaccount                :: ? string
    google_analytics_domains  :: ? [string]
    google_analytics_campaign :: ? string
    metadata                  :: ? MessageMetadata
    recipient_metadata        :: ? [RecipientMetadata]
    attachments               ::   [Attachment]
    images                    :: ? [Image]

srsp :: DeliveryStatus
  // The response object when sending messages
  = record
    email         ::   Email
    status        ::   SendStatus
    reject_reason :: ? RejectReason
    _id           ::   MessageId

srchrsp :: SearchResult
  // result type for search requests
  = record 
    ts            ::   utc
    _id           ::   MessageId
    sender        ::   Email
    template      ::   TemplateName
    subject       ::   string
    email         ::   Email
    tags          ::   [Tag]
    opens         ::   Count
    opens_detail  ::   [Detail]
    clicks        ::   Count
    clicks_detail ::   [Detail]
    state         ::   SendStatus
    metadata      ::   MessageMetadata
    smtp_events   :: ? [SmtpEvent]

smtpev :: SmtpEvent
  // an event in the history of the message
  = record 
    ts   :: utc
    type :: string
    diag :: string

rawrq :: RawRequest
  // send a raw request 
  = record 
    key                ::   ApiKey
    raw_message        ::   string
    from_email         :: ? Email
    from_name          :: ? string
    to                 :: ? [Email]
    async              ::   boolean
    ip_pool            ::   string
    send_at            ::   string
    return_path_domain ::   string

sched :: Scheduled
  // a scheduled message
  = record
    _id        :: MessageId
    created_at :: string
    send_at    :: string
    from_email :: Email
    to         :: Email
    subject    :: string

|]
