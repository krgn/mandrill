{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Subaccounts where

import Network.Mandrill.Response
import Network.Mandrill.Utils
import Network.Mandrill.Types

-- | Get the list of subaccounts defined for the account, 
-- optionally filtered by a prefix
list :: ApiKey -> Query -> IO (Either ApiError [Subaccount])
list k q = 
     performRequest "/subaccounts/list.json" $
       object [ "key" .= k, "q" .= q ]


-- | Add a new subaccount
add :: ApiKey -> SubaccountId -> Name -> Notes -> Count -> IO (Either ApiError Subaccount)
add k s na no c =
    performRequest "/subaccounts/add.json" $
      object [ "key"          .= k
             , "id"           .= s
             , "name"         .= na
             , "notes"        .= no
             , "custom_quota" .= c ]

-- | Given the ID of an existing subaccount, return the data about it
info :: ApiKey -> SubaccountId -> IO (Either ApiError Subaccount)
info k i =
     performRequest "/subaccounts/info.json" $
       object [ "key" .= k , "id" .= i ]


-- | Update an existing subaccount
update :: ApiKey -> SubaccountId -> Name -> Notes -> Count -> IO (Either ApiError Subaccount)
update k s na no c = 
       performRequest "/subaccounts/update.json" $
         object [ "key"          .= k
                , "id"           .= s
                , "name"         .= na
                , "notes"        .= no
                , "custom_quota" .= c ]


-- | Delete an existing subaccount. Any email related to the subaccount will be 
-- saved, but stats will be removed and any future sending calls to this 
-- subaccount will fail.
delete :: ApiKey -> SubaccountId -> IO (Either ApiError Subaccount)
delete k i =
       performRequest "/subaccounts/delete.json" $
         object [ "key" .= k , "id" .= i ]


-- | Pause a subaccount's sending. Any future emails delivered to this 
-- subaccount will be queued for a maximum of 3 days until the subaccount 
-- is resumed.
pause :: ApiKey -> SubaccountId -> IO (Either ApiError Subaccount)
pause k i = 
      performRequest "/subaccounts/pause.json" $
        object [ "key" .= k , "id" .= i ]


-- | Resume a paused subaccount's sending
resume :: ApiKey -> SubaccountId -> IO (Either ApiError Subaccount)
resume k i =
       performRequest "/subaccounts/resume.json" $
         object [ "key" .= k , "id" .= i ]
