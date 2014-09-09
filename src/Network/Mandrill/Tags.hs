{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Tags where

import Network.Mandrill.Response
import Network.Mandrill.Types

allTimeSeries :: ApiKey -> IO (Either ApiError ())
allTimeSeries _ = undefined

timeSeries :: ApiKey -> IO (Either ApiError ())
timeSeries _ = undefined

list :: ApiKey -> IO (Either ApiError [Tag])
list _ = undefined

info :: ApiKey -> IO (Either ApiError Tag)
info _ = undefined

delete :: ApiKey -> IO (Either ApiError ())
delete _ = undefined
