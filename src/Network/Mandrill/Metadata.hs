{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Metadata where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils


list :: ApiKey -> IO (Either ApiError [Metadata])
list k = do
     resp <- performRequest "/metadata/list.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k ]


delete :: ApiKey -> Name -> IO (Either ApiError Metadata)
delete k n = do
       resp <- performRequest "/metadata/delete.json" mkObj
       return $ parseResponse resp
       where mkObj = encode $ object [ "key" .= k, "name" .= n ]


add :: ApiKey -> Metadata ->  IO (Either ApiError Metadata)
add k m  = do
    resp <- performRequest "/metadata/add.json" mkObj
    return $ parseResponse resp
    where mkObj = encode $ object [ "key"           .= k
                                  , "name"          .= _mtdt_name m
                                  , "view_template" .= _mtdt_view_template m ]


update :: ApiKey -> Metadata ->  IO (Either ApiError Metadata)
update k m  = do
        resp <- performRequest "/metadata/update.json" mkObj
        return $ parseResponse resp
        where mkObj = encode $ object [ "key"           .= k
                                      , "name"          .= _mtdt_name m
                                      , "view_template" .= _mtdt_view_template m ]
