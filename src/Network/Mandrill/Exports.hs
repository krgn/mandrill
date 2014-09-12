{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Exports where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

-- | Returns a list of your exports.
list :: ApiKey -> IO (Either ApiError [Export])
list k =
     performRequest "/exports/list.json" $
       object [ "key" .= k ]

-- |  Returns information about an export job. If the export job's state is 
-- 'complete', the returned data will include a URL you can use to fetch the 
-- results. Every export job produces a zip archive, but the format of the 
-- archive is distinct for each job type. The api calls that initiate exports 
-- include more details about the output format for that job type.
info :: ApiKey -> ExportId -> IO (Either ApiError Export)
info k i =
     performRequest "/exports/info.json" $
       object [ "key" .= k, "id" .= i ]


-- | Begins an export of your rejection blacklist. The blacklist will be 
-- exported to a zip archive containing a single file named rejects.csv that 
-- includes the following fields: email, reason, detail, created_at, 
-- expires_at, last_event_at, expires_at.
rejects :: ApiKey -> Email -> IO (Either ApiError Export)
rejects k e =
        performRequest "/exports/rejects.json" $
          object [ "key" .= k, "notify_email" .= e ]


-- | Begins an export of your rejection whitelist. The whitelist will be 
-- exported to a zip archive containing a single file named whitelist.csv 
-- that includes the following fields: email, detail, created_at.
whitelist :: ApiKey -> Email -> IO (Either ApiError Export)
whitelist k e =
          performRequest "/exports/whitelist.json" $
            object [ "key" .= k, "notify_email" .= e ]


-- | Begins an export of your activity history. The activity will be exported 
-- to a zip archive containing a single file named activity.csv in the same 
-- format as you would be able to export from your account's activity view. It 
-- includes the following fields: Date, Email Address, Sender, Subject, Status, 
-- Tags, Opens, Clicks, Bounce Detail. If you have configured any custom metadata 
-- fields, they will be included in the exported data.
activity :: ApiKey -> Email -> From -> Thru -> [Tag] -> [Email] -> [SendStatus] -> [ApiKey] -> IO (Either ApiError Export)
activity k e f t ts ss sts ks = 
         performRequest "/exports/activity.json" $
           object [ "key"          .= k
                  , "notify_email" .= e
                  , "date_from"    .= f
                  , "date_to"      .= t
                  , "tags"         .= ts
                  , "senders"      .= ss
                  , "states"       .= sts
                  , "api_keys"     .= ks ]

