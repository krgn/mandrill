{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Senders where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

-- | Return the senders that have tried to use this account.
list :: ApiKey -> IO (Either ApiError Sender)
list k =
     performRequest "/senders/list.json" $
       object [ "key" .= k ]


-- | Returns the sender domains that have been added to this account.
domains :: ApiKey -> IO (Either ApiError [DomainRecord])
domains k =
        performRequest "/senders/domains.json" $
          object [ "key" .= k ]


-- | Adds a sender domain to your account. Sender domains are added 
-- automatically as you send, but you can use this call to add them ahead 
-- of time.
addDomain :: ApiKey -> Name -> IO (Either ApiError DomainRecord)
addDomain k n =
          performRequest "/senders/add-domain.json" $
            object [ "key" .= k, "domain" .= n ]


-- | Checks the SPF and DKIM settings for a domain. If you haven't already 
-- added this domain to your account, it will be added automatically.
checkDomain :: ApiKey -> Name -> IO (Either ApiError DomainRecord)
checkDomain k n = 
            performRequest "/senders/check-domain.json" $
              object [ "key" .= k, "domain" .= n ]


-- | Sends a verification email in order to verify ownership of a domain. 
-- Domain verification is an optional step to confirm ownership of a domain. 
-- Once a domain has been verified in a Mandrill account, other accounts may 
-- not have their messages signed by that domain unless they also verify the 
-- domain. This prevents other Mandrill accounts from sending mail signed by 
-- your domain.
verifyDomain :: ApiKey -> Name -> Mailbox -> IO (Either ApiError DomainState)
verifyDomain k n m =
             performRequest "/senders/verify-domain.json" $
               object [ "key"     .= k
                      , "domain"  .= n
                      , "mailbox" .= m ]


-- | Return more detailed information about a single sender, including 
-- aggregates of recent stats
info :: ApiKey -> Email -> IO (Either ApiError Stat)
info k e =
     performRequest "/senders/info.json" $
       object [ "key"     .= k
              , "address" .= e ]


-- | Return the recent history (hourly stats for the last 30 days) for a sender
timeSeries :: ApiKey -> Email -> IO (Either ApiError [Stat])
timeSeries k e =
           performRequest "/senders/time-series.json" $
             object [ "key"     .= k
                    , "address" .= e ]


