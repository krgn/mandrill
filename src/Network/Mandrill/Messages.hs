{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Messages where

import Network.Mandrill.ApiError
import Network.Mandrill.Types

send :: ApiKey -> IO (Either ApiError ())
send _ = undefined

sendTemplate :: ApiKey -> IO (Either ApiError ())
sendTemplate _ = undefined

search :: ApiKey -> IO (Either ApiError ())
search _ = undefined

searchTimeSeries :: ApiKey -> IO (Either ApiError ())
searchTimeSeries _ = undefined

info :: ApiKey -> IO (Either ApiError ())
info _ = undefined

content :: ApiKey -> IO (Either ApiError ())
content _ = undefined

parse :: ApiKey -> IO (Either ApiError ())
parse _ = undefined

sendRaw :: ApiKey -> IO (Either ApiError ())
sendRaw _ = undefined

listScheduled :: ApiKey -> IO (Either ApiError ())
listScheduled _ = undefined

cancelScheduled :: ApiKey -> IO (Either ApiError ())
cancelScheduled _ = undefined

reschedule :: ApiKey -> IO (Either ApiError ())
reschedule _ = undefined
