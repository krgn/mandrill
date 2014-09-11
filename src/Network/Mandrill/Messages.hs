{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Messages where

import Network.Mandrill.Utils
import Network.Mandrill.Response
import Network.Mandrill.Types

-- | Send a new transactional message through Mandrill
send :: ApiKey        -> 
       Message       -> 
       MessageConfig -> 
       IO (Either ApiError [DeliveryStatus])
send k m c = 
     performRequest "/messages/send.json" $
        object [ "key"     .= k
               , "message" .= m 
               , "ip_pool" .= _cnf_ip_pool c
               , "async"   .= _cnf_async   c 
               , "send_at" .= _cnf_send_at c ]


-- | Send a new transactional message through Mandrill using a template
sendTmpl :: ApiKey         -> 
           Message        -> 
           MessageConfig  -> 
           Name           -> 
           [Content]      -> 
           IO (Either ApiError [DeliveryStatus])
sendTmpl k m c t tc =
         performRequest "/messages/send-template.json" $
           object [ "key"              .= k
                  , "template_name"    .= t
                  , "template_content" .= tc
                  , "message"          .= m
                  , "ip_pool"          .= _cnf_ip_pool  c
                  , "async"            .= _cnf_async    c 
                  , "send_at"          .= _cnf_send_at  c ]


-- | Search recently sent messages and optionally narrow by date 
-- range, tags, senders, and API keys. If no date range is specified, 
-- results within the last 7 days are returned. This method may be called 
-- up to 20 times per minute. If you need the data more often, 
-- you can use /messages/info.json to get the information for a single message, 
-- or webhooks to push activity to your own application for querying.
search :: ApiKey   ->
         Query    ->
         From     ->
         Thru     ->
         [Tag]    ->
         [Email]  ->
         [ApiKey] -> 
         Count    ->
         IO (Either ApiError [SearchResult])
search k q f t ts es ks c =
       performRequest "/messages/search.json" $
         object [ "key"       .= k
                , "query"     .= q
                , "date_from" .= f
                , "date_to"   .= t
                , "tags"      .= ts
                , "senders"   .= es
                , "api_keys"  .= ks
                , "limit"     .= c ]

-- | Search the content of recently sent messages and return the aggregated 
-- hourly stats for matching messages
searchTimeSeries :: ApiKey   -> 
                   Query    ->
                   From     ->
                   Thru     ->
                   [Tag]    ->
                   [Email]  ->
                   IO (Either ApiError [Stat])
searchTimeSeries k q f t ts es =
                 performRequest "/messages/search-time-series.json" $
                   object [ "key"       .= k
                          , "query"     .= q
                          , "date_from" .= f
                          , "date_to"   .= t
                          , "tags"      .= ts
                          , "senders"   .= es ]

-- | Get the information for a single recently sent message
info :: ApiKey ->
       MessageId -> 
       IO (Either ApiError SearchResult)
info k mid =
     performRequest "/messages/info.json" $
       object [ "key" .= k
              , "id"  .= mid ]


-- | Get the full content of a recently sent message
content :: ApiKey -> 
          MessageId -> 
          IO (Either ApiError Message)
content k mid =
        performRequest "/messages/content.json" $
          object [ "key" .= k
                 , "id"  .= mid ]

-- | Parse the full MIME document for an email message, returning the content
-- of the message broken into its constituent pieces
parse :: ApiKey -> RawMessage -> IO (Either ApiError Message)
parse k raw = 
      performRequest "/messages/parse.json" $
        object [ "key"          .= k
               , "raw_message"  .= raw ]


-- | Take a raw MIME document for a message, and send it exactly as if it 
-- were sent through Mandrill's SMTP servers
sendRaw :: ApiKey ->
          RawMessage -> 
          Email -> 
          Name -> 
          To -> 
          MessageConfig -> 
          IO (Either ApiError [DeliveryStatus])
sendRaw k raw e n to c =
        performRequest "/messages/send-raw.json" $
          object [ "key"         .= k
                 , "raw_message" .= raw
                 , "from_email"  .= e
                 , "from_name"   .= n
                 , "to"          .= to
                 , "async"       .= _cnf_async c
                 , "ip_pool"     .= _cnf_ip_pool c
                 , "send_at"     .= _cnf_send_at c ]

-- | Queries your scheduled emails by sender or recipient, or both.
listScheduled :: ApiKey -> 
                Email ->
                IO (Either ApiError [Scheduled])
listScheduled k e = 
              performRequest "/messages/list-scheduled.json" $
                object [ "key" .= k
                       , "to"  .= e ]

-- | Cancels a scheduled email.
cancelScheduled :: ApiKey -> 
                  ScheduledId -> 
                  IO (Either ApiError Scheduled)
cancelScheduled k i =
                performRequest "/messages/cancel-scheduled.json" $
                  object [ "key" .= k
                         , "id"  .= i ]

-- | Reschedules a scheduled email.
reschedule :: ApiKey -> 
             ScheduledId -> 
             SendAt -> 
             IO (Either ApiError Scheduled)
reschedule k i s =
           performRequest "/messages/reschedule.json" $
             object [ "key"     .= k
                    , "id"      .= i 
                    , "send_at" .= s ]
