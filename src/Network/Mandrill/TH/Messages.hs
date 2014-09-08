{-# LANGUAGE QuasiQuotes #-}

module Network.Mandrill.TH.Messages where

import Data.API.Parse
import Data.API.Types

messages :: API
messages = [api|

dtl :: Detail
  // A stat containing information how often a messsage has been opened or stuff 
  // was clicked.
  = record 
    ts       :: utc
    url      :: ? Url
    ip       :: IpAddress  
    location :: string
    ua       :: string

meta :: Metadata 
  // Message metadata
  = record 
    website :: string

rcptmeta :: RecipientMetadata
  // per recipient metadata 
  = record
    rcpt   :: Email
    values :: Metadata

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

tplnm :: TemplateName
  // type synoym for the name of a template
  = string

tplcnt :: TemplateContent
  // type synoym for the name of a template
  = record
    name :: TemplateName
    content :: string
 
to :: To
  // a way to encode the possibility of a single or multiple recipient addresses 
  = union
    | single   :: Recipient
    | multiple :: [Recipient]

msg :: Message
  // A message structure
  = record 
    ts                        :: ? utc
    _id                       :: ? MessageId
    html                      :: string
    text                      :: string
    subject                   :: string
    from_email                :: Email
    from_name                 :: string
    to                        :: To
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
    tags                      :: [Tag]
    subaccount                :: ? string
    google_analytics_domains  :: ? [string]
    google_analytics_campaign :: ? string
    metadata                  :: ? Metadata
    recipient_metadata        :: ? [RecipientMetadata]
    attachments               :: [Attachment]
    images                    :: ? [Image]

ss :: SendStatus
  // The status enum 
  = enum
    | sent
    | queued
    | bounced
    | rejected
    | invalid

rr :: RejectReason
  // Reason enum when message was rejected
  = enum
    | hard_bounce
    | soft_bounce
    | spam
    | unsub
    | custom
    | invalid_sender
    | invalid 
    | test_mode_limit
    | rule 

srq :: SendRequest
  // the request object sent to send of a new message via the API
  = record
    key              :: ApiKey
    template_name    :: ? TemplateName
    template_content :: ? [TemplateContent]
    message          :: Message
    async            :: boolean
    ip_pool          :: string
    send_at          :: string    

srsp :: SendResponse
  // The response object when sending messages
  = record
    email         :: Email
    status        :: SendStatus
    reject_reason :: RejectReason
    _id           :: MessageId

srchrq :: SearchRequest
  // request to search for message sent via the API
  = record
    key       :: ApiKey
    query     :: string
    date_from :: string
    date_to   :: string
    tags      :: [Tag]
    senders   :: [Email]
    api_keys  :: [ApiKey]
    limit     :: Count

srchrsp :: SearchResult
  // result type for search requests
  = record 
    ts            :: utc
    _id           :: MessageId
    sender        :: Email
    template      :: TemplateName
    subject       :: string
    email         :: Email
    tags          :: [Tag]
    opens         :: Count
    opens_detail  :: [Detail]
    clicks        :: Count
    clicks_detail :: [Detail]
    state         :: SendStatus
    metadata      :: Metadata
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
    key                :: ApiKey
    raw_message        :: string
    from_email         :: ? Email
    from_name          :: ? string
    to                 :: ? [Email]
    async              :: boolean
    ip_pool            :: string
    send_at            :: string
    return_path_domain :: string

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

