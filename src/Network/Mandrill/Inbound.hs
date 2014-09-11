{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Inbound where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

-- | List the domains that have been configured for inbound delivery
domains :: ApiKey -> IO (Either ApiError [InboundDomain])
domains k =
        performRequest "/inbound/domains.json" $
          object [ "key" .= k ]


-- | Add an inbound domain to your account
addDomain :: ApiKey -> Url -> IO (Either ApiError InboundDomain)
addDomain k d =
          performRequest "/inbound/add-domain.json" $
            object [ "key" .= k, "domain" .= d ]


-- | Check the MX settings for an inbound domain. The domain must have 
-- already been added with the add-domain call
checkDomain :: ApiKey -> Url -> IO (Either ApiError InboundDomain)
checkDomain k d =
            performRequest "/inbound/check-domain.json" $ 
              object [ "key" .= k, "domain" .= d ]


-- | Delete an inbound domain from the account. All mail will stop routing 
-- for this domain immediately.
deleteDomain :: ApiKey -> Url -> IO (Either ApiError InboundDomain)
deleteDomain k d = 
             performRequest "/inbound/delete-domain.json" $
               object [ "key" .= k, "domain" .= d ]


-- | List the mailbox routes defined for an inbound domain
routes :: ApiKey -> Url -> IO (Either ApiError [InboundRoute])
routes k d = 
       performRequest "/inbound/routes.json" $
         object [ "key" .= k, "domain" .= d ]


-- | Add a new mailbox route to an inbound domain
addRoute :: ApiKey -> Url -> Pattern -> Url -> IO (Either ApiError InboundRoute)
addRoute k u1 p u2 =
         performRequest "/inbound/add-route.json" $
           object [ "key"     .= k
                  , "domain"  .= u1
                  , "pattern" .= p
                  , "url"     .= u2 ]


-- | Update the pattern or webhook of an existing inbound mailbox route. 
-- If `Nothing` is provided for any fields, the values will remain unchanged.
updateRoute :: ApiKey -> RouteId -> Pattern -> Url -> IO (Either ApiError InboundRoute)
updateRoute k i p u =
            performRequest "/inbound/update-route.json" $
              object [ "key"     .= k
                     , "id"      .= i
                     , "pattern" .= p
                     , "url"     .= u ]


-- | Delete an existing inbound mailbox route
deleteRoute :: ApiKey -> RouteId -> IO (Either ApiError InboundRoute)
deleteRoute k i =
            performRequest "/inbound/delete-route.json" $
              object [ "key" .= k, "id" .= i ]


-- | Take a raw MIME document destined for a domain with inbound domains set 
-- up, and send it to the inbound hook exactly as if it had been sent over SMTP
sendRaw :: ApiKey -> RawMessage -> To -> Email -> Helo -> ClientAddress -> IO (Either ApiError InboundRoute)
sendRaw k r t e h a =
        performRequest "/inbound/send-raw.json" $
          object [ "key"            .= k
                 , "raw_message"    .= r
                 , "to"             .= t
                 , "mail_from"      .= e
                 , "helo"           .= h
                 , "client_address" .= a ]

