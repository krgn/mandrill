{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Messages where

import Network.Mandrill.Utils
import Network.Mandrill.Response
import Network.Mandrill.Types

send :: ApiKey        -> 
       Message       -> 
       MessageConfig -> 
       IO (Either ApiError [DeliveryStatus])
send k m c = do
  resp <- performRequest "/messages/send.json" mkObj
  return $ parseResponse resp
  where mkObj = encode $
                object [ "key"     .= k
                       , "message" .= m 
                       , "ip_pool" .= _cnf_ip_pool c
                       , "async"   .= _cnf_async   c 
                       , "send_at" .= _cnf_send_at c ]


sendTmpl :: ApiKey            -> 
           Message           -> 
           MessageConfig     -> 
           Template          -> 
           [TemplateContent] -> 
           IO (Either ApiError [DeliveryStatus])
sendTmpl k m c t tc = do
         resp <- performRequest "/messages/send-template.json" mkObj
         return $ parseResponse resp
  where mkObj = encode $
                object [ "key"              .= k
                       , "template_name"    .= _tmpl_name    t 
                       , "template_content" .= tc
                       , "message"          .= m
                       , "ip_pool"          .= _cnf_ip_pool  c
                       , "async"            .= _cnf_async    c 
                       , "send_at"          .= _cnf_send_at  c ]

search :: ApiKey   ->
         Query    ->
         From     ->
         Thru     ->
         [Tag]    ->
         [Email]  ->
         [ApiKey] -> 
         Count    ->
         IO (Either ApiError [SearchResult])
search k q f t ts es ks c = do
       resp <- performRequest "/messages/search.json" mkObj
       return $ parseResponse resp
  where mkObj = encode $
                object [ "key"       .= k
                       , "query"     .= q
                       , "date_from" .= f
                       , "date_to"   .= t
                       , "tags"      .= ts
                       , "senders"   .= es
                       , "api_keys"  .= ks
                       , "limit"     .= c ]


searchTimeSeries :: ApiKey   -> 
                   Query    ->
                   From     ->
                   Thru     ->
                   [Tag]    ->
                   [Email]  ->
                   IO (Either ApiError [Stat])
searchTimeSeries k q f t ts es = do
                 resp <- performRequest "/messages/search-time-series.json" mkObj
                 return $ parseResponse resp
   where mkObj = encode $
                 object [ "key"       .= k
                        , "query"     .= q
                        , "date_from" .= f
                        , "date_to"   .= t
                        , "tags"      .= ts
                        , "senders"   .= es ]


info :: ApiKey ->
       MessageId -> 
       IO (Either ApiError SearchResult)
info k mid = do
     resp <- performRequest "/messages/info.json" mkObj
     return $ parseResponse resp
   where mkObj = encode $
                 object [ "key" .= k
                        , "id"  .= mid ]


content :: ApiKey -> 
          MessageId -> 
          IO (Either ApiError Message)
content k mid = do
        resp <- performRequest "/messages/content.json" mkObj
        return $ parseResponse resp
   where mkObj = encode $
                 object [ "key" .= k
                        , "id"  .= mid ]


parse :: ApiKey -> RawMessage -> IO (Either ApiError Message)
parse k raw = do
      resp <- performRequest "/messages/parse.json" mkObj
      return $ parseResponse resp
   where mkObj = encode $
                 object [ "key"          .= k
                        , "raw_message"  .= raw ]


sendRaw :: ApiKey ->
          RawMessage -> 
          Email -> 
          Name -> 
          To -> 
          MessageConfig -> 
          IO (Either ApiError [DeliveryStatus])
sendRaw k raw e n to c = do
        resp <- performRequest "/messages/send-raw.json" mkObj
        return $ parseResponse resp
  where mkObj = encode $
                object [ "key"         .= k
                       , "raw_message" .= raw
                       , "from_email"  .= e
                       , "from_name"   .= n
                       , "to"          .= to
                       , "async"       .= _cnf_async c
                       , "ip_pool"     .= _cnf_ip_pool c
                       , "send_at"     .= _cnf_send_at c ]

listScheduled :: ApiKey -> 
                Email ->
                IO (Either ApiError [Scheduled])
listScheduled k e = do
              resp <- performRequest "/messages/list-scheduled.json" mkObj
              return $ parseResponse resp
  where mkObj = encode $
                object [ "key" .= k
                       , "to"  .= e ]


cancelScheduled :: ApiKey -> 
                  ScheduledId -> 
                  IO (Either ApiError Scheduled)
cancelScheduled k i = do
                resp <- performRequest "/messages/cancel-scheduled.json" mkObj
                return $ parseResponse resp
  where mkObj = encode $
                object [ "key" .= k
                       , "id"  .= i ]


reschedule :: ApiKey -> 
             ScheduledId -> 
             SendAt -> 
             IO (Either ApiError Scheduled)
reschedule k i s = do
           resp <- performRequest "/messages/reschedule.json" mkObj
           return $ parseResponse resp
  where mkObj = encode $
                object [ "key"     .= k
                       , "id"      .= i 
                       , "send_at" .= s ]
