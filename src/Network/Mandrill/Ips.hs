{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Ips where

import Network.Mandrill.Response
import Network.Mandrill.Utils
import Network.Mandrill.Types

-- | Lists your dedicated IPs.
list :: ApiKey -> IO (Either ApiError [IpRecord])
list k =
     performRequest "/ips/list.json" $
       object [ "key" .= k ] 


-- | Retrieves information about a single dedicated ip.
info :: ApiKey -> IpAddress -> IO (Either ApiError [IpRecord])
info k i = 
     performRequest "/ips/info.json" $
       object [ "key" .= k, "ip" .= i ] 


-- | Requests an additional dedicated IP for your account. Accounts may have 
-- one outstanding request at any time, and provisioning requests are processed 
-- within 24 hours.
provision :: ApiKey -> Bool -> Name -> IO (Either ApiError IpProvision)
provision k w p =
          performRequest "/ips/provision.json" $
            object [ "key"    .= k
                   , "warmup" .= w
                   , "pool"   .= p ] 


-- | Begins the warmup process for a dedicated IP. During the warmup process, 
-- Mandrill will gradually increase the percentage of your mail that is sent 
-- over the warming-up IP, over a period of roughly 30 days. The rest of your 
-- mail will be sent over shared IPs or other dedicated IPs in the same pool.
startWarmup :: ApiKey -> IpAddress -> IO (Either ApiError IpRecord)
startWarmup k i = 
            performRequest "/ips/start-warmup.json" $
              object [ "key" .= k, "ip" .= i ] 


-- | Cancels the warmup process for a dedicated IP.
cancelWarmup :: ApiKey -> IpAddress -> IO (Either ApiError IpRecord)
cancelWarmup k i = 
             performRequest "/ips/cancel-warmup.json" $
               object [ "key" .= k, "ip" .= i ] 


-- | Moves a dedicated IP to a different pool.
setPool :: ApiKey -> IpAddress -> Name -> Bool -> IO (Either ApiError IpRecord)
setPool k i p c = 
        performRequest "/ips/cancel-warmup.json" $
          object [ "key"         .= k
                 , "ip"          .= i
                 , "create_pool" .= c
                 , "pool"        .= p ] 


-- | Deletes a dedicated IP. This is permanent and cannot be undone.
delete :: ApiKey -> IpAddress -> IO (Either ApiError IpRecord)
delete k i = 
       performRequest "/ips/delete.json" $
         object [ "key" .= k, "ip" .= i ] 


-- | Lists your dedicated IP pools.
listPools :: ApiKey -> IO (Either ApiError [IpPool])
listPools k = 
          performRequest "/ips/list-pools.json" $
            object [ "key" .= k ] 


-- | Describes a single dedicated IP pool.
poolInfo :: ApiKey -> Name -> IO (Either ApiError IpPool)
poolInfo k p = 
         performRequest "/ips/pool-info.json" $
           object [ "key" .= k, "pool" .= p ] 


-- | Creates a pool and returns it. If a pool already exists with this 
-- name, no action will be performed.
createPool :: ApiKey -> Name -> IO (Either ApiError IpPool)
createPool k p = 
         performRequest "/ips/create-pool.json" $
           object [ "key" .= k, "pool" .= p ] 


-- | Deletes a pool. A pool must be empty before you can delete it, and you 
-- cannot delete your default pool.
deletePool :: ApiKey -> Name -> IO (Either ApiError IpPool)
deletePool k p = 
           performRequest "/ips/delete-pool.json" $
             object [ "key" .= k, "pool" .= p ] 


-- | Tests whether a domain name is valid for use as the custom 
-- reverse DNS for a dedicated IP.
checkCustomDns :: ApiKey -> IpAddress -> Url -> IO (Either ApiError DnsInfo)
checkCustomDns k i u = 
               performRequest "/ips/check-custom-dns.json" $
                 object [ "key"    .= k
                        , "ip"     .= i
                        , "domain" .= u ] 


-- | Configures the custom DNS name for a dedicated IP.
setCustomDns :: ApiKey -> IpAddress -> Url -> IO (Either ApiError IpRecord)
setCustomDns k i u = 
             performRequest "/ips/set-custom-dns.json" $
               object [ "key"    .= k
                      , "ip"     .= i
                      , "domain" .= u ] 

