{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Inbound where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

domains :: ApiKey -> IO (Either ApiError [InboundDomain])
domains k = do
        resp <- performRequest "/inbound/domains.json" mkObj
        return $ parseResponse resp
        where mkObj = encode $ object [ "key" .= k ]

addDomain :: ApiKey -> Url -> IO (Either ApiError InboundDomain)
addDomain k d = do
          resp <- performRequest "/inbound/add-domain.json" mkObj
          return $ parseResponse resp
          where mkObj = encode $ object [ "key" .= k, "domain" .= d ]

checkDomain :: ApiKey -> Url -> IO (Either ApiError InboundDomain)
checkDomain k d = do
            resp <- performRequest "/inbound/check-domain.json" mkObj
            return $ parseResponse resp
            where mkObj = encode $ object [ "key" .= k, "domain" .= d ]

deleteDomain :: ApiKey -> Url -> IO (Either ApiError InboundDomain)
deleteDomain k d = do
             resp <- performRequest "/inbound/delete-domain.json" mkObj
             return $ parseResponse resp
             where mkObj = encode $ object [ "key" .= k, "domain" .= d ]


routes :: ApiKey -> Url -> IO (Either ApiError [InboundRoute])
routes k d = do
       resp <- performRequest "/inbound/routes.json" mkObj
       return $ parseResponse resp
       where mkObj = encode $ object [ "key" .= k, "domain" .= d ]


addRoute :: ApiKey -> Url -> Pattern -> Url -> IO (Either ApiError InboundRoute)
addRoute k u1 p u2 = do
         resp <- performRequest "/inbound/add-route.json" mkObj
         return $ parseResponse resp
         where mkObj = encode $ object [ "key"     .= k
                                       , "domain"  .= u1
                                       , "pattern" .= p
                                       , "url"     .= u2 ]

updateRoute :: ApiKey -> RouteId -> Pattern -> Url -> IO (Either ApiError InboundRoute)
updateRoute k i p u = do
            resp <- performRequest "/inbound/update-route.json" mkObj
            return $ parseResponse resp
            where mkObj = encode $ object [ "key"     .= k
                                          , "id"      .= i
                                          , "pattern" .= p
                                          , "url"     .= u ]


deleteRoute :: ApiKey -> RouteId -> IO (Either ApiError InboundRoute)
deleteRoute k i = do
            resp <- performRequest "/inbound/delete-route.json" mkObj
            return $ parseResponse resp
            where mkObj = encode $ object [ "key"     .= k
                                          , "id"      .= i ]

sendRaw :: ApiKey -> RawMessage -> To -> Email -> Helo -> ClientAddress -> IO (Either ApiError InboundRoute)
sendRaw k r t e h a = do
        resp <- performRequest "/inbound/send-raw.json" mkObj
        return $ parseResponse resp
        where mkObj = encode $ object [ "key"            .= k
                                      , "raw_message"    .= r
                                      , "to"             .= t
                                      , "mail_from"      .= e
                                      , "helo"           .= h
                                      , "client_address" .= a ]

