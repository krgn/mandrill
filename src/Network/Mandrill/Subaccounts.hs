{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Subaccounts where

import Network.Mandrill.Response
import Network.Mandrill.Utils
import Network.Mandrill.Types


list :: ApiKey -> Query -> IO (Either ApiError [Subaccount])
list k q = do
     resp <- performRequest "/subaccounts/list.json" mkObj 
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k, "q" .= q ]


add :: ApiKey -> SubaccountId -> Name -> Notes -> Count -> IO (Either ApiError Subaccount)
add k s na no c = do
    resp <- performRequest "/subaccounts/add.json" mkObj 
    return $ parseResponse resp
    where mkObj = encode $ object [ "key"          .= k
                                  , "id"           .= s
                                  , "name"         .= na
                                  , "notes"        .= no
                                  , "custom_quota" .= c ]


info :: ApiKey -> SubaccountId -> IO (Either ApiError Subaccount)
info k i = do
     resp <- performRequest "/subaccounts/info.json" mkObj 
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k , "id" .= i ]


update :: ApiKey -> SubaccountId -> Name -> Notes -> Count -> IO (Either ApiError Subaccount)
update k s na no c = do
       resp <- performRequest "/subaccounts/update.json" mkObj 
       return $ parseResponse resp
       where mkObj = encode $ object [ "key"          .= k
                                     , "id"           .= s
                                     , "name"         .= na
                                     , "notes"        .= no
                                     , "custom_quota" .= c ]


delete :: ApiKey -> SubaccountId -> IO (Either ApiError Subaccount)
delete k i = do
       resp <- performRequest "/subaccounts/delete.json" mkObj 
       return $ parseResponse resp
       where mkObj = encode $ object [ "key" .= k , "id" .= i ]


pause :: ApiKey -> SubaccountId -> IO (Either ApiError Subaccount)
pause k i = do
      resp <- performRequest "/subaccounts/pause.json" mkObj 
      return $ parseResponse resp
      where mkObj = encode $ object [ "key" .= k , "id" .= i ]


resume :: ApiKey -> SubaccountId -> IO (Either ApiError Subaccount)
resume k i = do
       resp <- performRequest "/subaccounts/resume.json" mkObj 
       return $ parseResponse resp
       where mkObj = encode $ object [ "key" .= k , "id" .= i ]
