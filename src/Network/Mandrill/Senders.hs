{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Senders where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

list :: ApiKey -> IO (Either ApiError Sender)
list k = do
     resp <- performRequest "/senders/list.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k ]

domains :: ApiKey -> IO (Either ApiError [Domain])
domains k = do
        resp <- performRequest "/senders/domains.json" mkObj
        return $ parseResponse resp
        where mkObj = encode $ object [ "key" .= k ]

addDomain :: ApiKey -> Name -> IO (Either ApiError Domain)
addDomain k n = do
          resp <- performRequest "/senders/add-domain.json" mkObj
          return $ parseResponse resp
          where mkObj = encode $ object [ "key" .= k, "domain" .= n ]

checkDomain :: ApiKey -> Name -> IO (Either ApiError Domain)
checkDomain k n = do
            resp <- performRequest "/senders/check-domain.json" mkObj
            return $ parseResponse resp
            where mkObj = encode $ object [ "key" .= k, "domain" .= n ]

verifyDomain :: ApiKey -> Name -> Mailbox -> IO (Either ApiError DomainState)
verifyDomain k n m = do
             resp <- performRequest "/senders/verify-domain.json" mkObj
             return $ parseResponse resp
             where mkObj = encode $ object [ "key"     .= k
                                           , "domain"  .= n
                                           , "mailbox" .= m ]

info :: ApiKey -> Email -> IO (Either ApiError Stat)
info k e = do
     resp <- performRequest "/senders/info.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key"     .= k
                                   , "address" .= e ]


timeSeries :: ApiKey -> Email -> IO (Either ApiError [Stat])
timeSeries k e = do
           resp <- performRequest "/senders/time-series.json" mkObj
           return $ parseResponse resp
           where mkObj = encode $ object [ "key"     .= k
                                         , "address" .= e ]


