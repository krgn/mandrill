{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Webhooks where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

-- | Get the list of all webhooks defined on the account
list :: ApiKey -> IO (Either ApiError [Webhook])
list k = 
     performRequest "/webhooks/list.json" $
       object [ "key" .= k ]

-- | Add a new webhook
add :: ApiKey -> Url -> Description -> [MessageEvent] -> IO (Either ApiError Webhook)
add k u d es = 
    performRequest "/webhooks/add.json" $
      object [ "key"         .= k
             , "url"         .= u
             , "description" .= d 
             , "events"      .= es ]

-- | Given the ID of an existing webhook, return the data about it
info :: ApiKey -> HookId -> IO (Either ApiError Webhook)
info k i = 
     performRequest "/webhooks/info.json" $
       object [ "key" .= k, "id" .= i ]

-- | Update an existing webhook
update :: ApiKey -> HookId -> Url -> Description -> [MessageEvent] -> IO (Either ApiError Webhook)
update k i u d es =
       performRequest "/webhooks/update.json" $
         object [ "key"         .= k
                , "id"          .= i
                , "url"         .= u
                , "description" .= d 
                , "events"      .= es ]

-- | Delete an existing webhook
delete :: ApiKey -> HookId -> IO (Either ApiError Webhook)
delete k i =
       performRequest "/webhooks/delete.json" $
         object [ "key" .= k, "id"  .= i ]


