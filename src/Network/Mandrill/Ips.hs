{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Ips where

import Network.Mandrill.Response
import Network.Mandrill.Utils
import Network.Mandrill.Types

list :: ApiKey -> IO (Either ApiError [IpRecord])
list k = do
     resp <- performRequest "/ips/list.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k ] 

info :: ApiKey -> IpAddress -> IO (Either ApiError [IpRecord])
info k i = do
     resp <- performRequest "/ips/info.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k, "ip" .= i ] 

provision :: ApiKey -> Bool -> Name -> IO (Either ApiError IpProvision)
provision k w p = do
          resp <- performRequest "/ips/provision.json" mkObj
          return $ parseResponse resp
          where mkObj = encode $ object [ "key"    .= k
                                        , "warmup" .= w
                                        , "pool"   .= p ] 

startWarmup :: ApiKey -> IpAddress -> IO (Either ApiError IpRecord)
startWarmup k i = do
            resp <- performRequest "/ips/start-warmup.json" mkObj
            return $ parseResponse resp
            where mkObj = encode $ object [ "key" .= k, "ip" .= i ] 

cancelWarmup :: ApiKey -> IpAddress -> IO (Either ApiError IpRecord)
cancelWarmup k i = do
             resp <- performRequest "/ips/cancel-warmup.json" mkObj
             return $ parseResponse resp
             where mkObj = encode $ object [ "key" .= k, "ip" .= i ] 

setPool :: ApiKey -> IpAddress -> Name -> Bool -> IO (Either ApiError IpRecord)
setPool k i p c = do
        resp <- performRequest "/ips/cancel-warmup.json" mkObj
        return $ parseResponse resp
         where mkObj = encode $ object [ "key"         .= k
                                       , "ip"          .= i
                                       , "create_pool" .= c
                                       , "pool"        .= p ] 

delete :: ApiKey -> IpAddress -> IO (Either ApiError IpRecord)
delete k i = do
       resp <- performRequest "/ips/delete.json" mkObj
       return $ parseResponse resp
       where mkObj = encode $ object [ "key" .= k, "ip" .= i ] 

listPools :: ApiKey -> IO (Either ApiError [IpPool])
listPools k = do
          resp <- performRequest "/ips/list-pools.json" mkObj
          return $ parseResponse resp
          where mkObj = encode $ object [ "key" .= k ] 

poolInfo :: ApiKey -> Name -> IO (Either ApiError IpPool)
poolInfo k p = do
         resp <- performRequest "/ips/pool-info.json" mkObj
         return $ parseResponse resp
         where mkObj = encode $ object [ "key" .= k, "pool" .= p ] 

createPool :: ApiKey -> Name -> IO (Either ApiError IpPool)
createPool k p = do
         resp <- performRequest "/ips/create-pool.json" mkObj
         return $ parseResponse resp
         where mkObj = encode $ object [ "key" .= k, "pool" .= p ] 

deletePool :: ApiKey -> Name -> IO (Either ApiError IpPool)
deletePool k p = do
           resp <- performRequest "/ips/delete-pool.json" mkObj
           return $ parseResponse resp
           where mkObj = encode $ object [ "key" .= k, "pool" .= p ] 

checkCustomDns :: ApiKey -> IpAddress -> Url -> IO (Either ApiError DnsInfo)
checkCustomDns k i u = do
               resp <- performRequest "/ips/check-custom-dns.json" mkObj
               return $ parseResponse resp
               where mkObj = encode $ object [ "key"    .= k
                                             , "ip"     .= i
                                             , "domain" .= u ] 

setCustomDns :: ApiKey -> IpAddress -> Url -> IO (Either ApiError IpRecord)
setCustomDns k i u = do
                  resp <- performRequest "/ips/set-custom-dns.json" mkObj
                  return $ parseResponse resp
                  where mkObj = encode $ object [ "key"    .= k
                                                , "ip"     .= i
                                                , "domain" .= u ] 

