{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Whitelists where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils


-- | Adds an email to your email rejection whitelist. If the address is 
-- currently on your blacklist, that blacklist entry will be removed 
-- automatically.
add :: ApiKey -> 
      Email -> 
      Comment -> 
      IO (Either ApiError Whitelist)
add k e c =
    performRequest "/whitelists/add.json" $
      object [ "key"     .= k 
             , "email"   .= e 
             , "comment" .= c]


-- | Retrieves your email rejection whitelist. You can provide an email address 
-- or search prefix to limit the results. Returns up to 1000 results.
list :: ApiKey -> Email -> IO (Either ApiError Whitelist)
list k e =
     performRequest "/whitelists/list.json" $
       object ["key" .= k, "email" .= e]


-- | Removes an email address from the whitelist.
delete :: ApiKey -> Email -> IO (Either ApiError Whitelist) 
delete k e =
       performRequest "/whitelists/delete.json" $
         object ["key" .= k, "email" .= e]
