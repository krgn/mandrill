{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Mandrill.TH.Utils where

import Data.API.Parse
import Data.API.Types

utils :: API
utils = [api|

sbccnt :: Subaccount
  // a subaccount 
  = string

apiKey :: ApiKey
  // key for api
  = basic string

tag :: Tag
  // a tag
  = string

email :: Email
  // Type synonym for email addresses
  = string

recipient :: Recipient
  // A recipient with data
  = record
    email :: Email
    name  :: string
    type  :: ? HeaderType

headerType :: HeaderType
  // an enum to discern to cc and bcc header field types
  = enum
    | to
    | cc
    | bcc

image :: Image
  // A recpient of this email address
  = record
    email      :: Email
    name       :: string
    headerType :: HeaderType

attachment :: Attachment 
  // An attachment to be added to the message 
  = record
    mimeType :: string
    name     :: string
    content  :: string

cnt :: Count
  // a type alias for a count
  = integer

url :: Url
  // a type alias for an Url
  = string
  
ip :: IpAddress
  // a type alias for ip addresses
  = string

msgid :: MessageId
  // a type alias for message Ids
  = string

|]
