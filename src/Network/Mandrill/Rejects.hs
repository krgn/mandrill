{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Rejects where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

-- | Adds an email to your email rejection blacklist. Addresses that you add 
-- manually will never expire and there is no reputation penalty for removing 
-- them from your blacklist. Attempting to blacklist an address that 
-- has been whitelisted will have no effect.
add :: ApiKey       -> 
      Email        -> 
      Comment      -> 
      SubaccountId ->  
      IO (Either ApiError Reject)
add k e c a =
    performRequest "/rejects/add.json" $
      object [ "key"        .= k
             , "email"      .= e 
             , "comment"    .= c 
             , "subaccount" .= a ]

-- | Retrieves your email rejection blacklist. You can provide an email 
-- address to limit the results. Returns up to 1000 results. By default, 
-- entries that have expired are excluded from the results; set 
-- `IncExpired` to true to include them.
list :: ApiKey       -> 
       Email        -> 
       IncExpired   -> 
       SubaccountId -> 
       IO (Either ApiError Reject)
list k e i s =
     performRequest "/rejects/list.json" $
       object [ "key"             .= k 
              , "email"           .= e
              , "include_expired" .= i 
              , "subaccount"      .= s ]

-- | Deletes an email rejection. There is no limit to how many rejections 
-- you can remove from your blacklist, but keep in mind that each deletion 
-- has an affect on your reputation.
delete :: ApiKey       -> 
         Email        -> 
         SubaccountId -> 
         IO (Either ApiError Reject)
delete k e s =
       performRequest "/rejects/delete.json" $
         object [ "key"        .= k 
                , "email"      .= e
                , "subaccount" .= s]
