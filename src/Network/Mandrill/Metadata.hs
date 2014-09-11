{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Metadata where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

-- | Get the list of custom metadata fields indexed for the account.
list :: ApiKey -> IO (Either ApiError [Metadata])
list k = 
     performRequest "/metadata/list.json" $
       object [ "key" .= k ]


-- | Add a new custom metadata field to be indexed for the account.
delete :: ApiKey -> Name -> IO (Either ApiError Metadata)
delete k n = 
       performRequest "/metadata/delete.json" $
         object [ "key" .= k, "name" .= n ]


-- | Update an existing custom metadata field.
add :: ApiKey -> Metadata ->  IO (Either ApiError Metadata)
add k m  = 
    performRequest "/metadata/add.json" $
      object [ "key"           .= k
             , "name"          .= _mtdt_name m
             , "view_template" .= _mtdt_view_template m ]


-- | Delete an existing custom metadata field. Deletion isn't instataneous, 
-- and /metadata/list will continue to return the field until the  
-- asynchronous deletion process is complete.
update :: ApiKey -> Metadata ->  IO (Either ApiError Metadata)
update k m  = 
        performRequest "/metadata/update.json" $
          object [ "key"           .= k
                 , "name"          .= _mtdt_name m
                 , "view_template" .= _mtdt_view_template m ]
