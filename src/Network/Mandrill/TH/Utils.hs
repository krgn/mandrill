{-# LANGUAGE QuasiQuotes #-}

module Network.Mandrill.TH.Utils where

import Data.API.Parse
import Data.API.Types

utils :: API
utils = [api|

apiError :: ApiError
  // An API error record
  = record
    status  :: string
    code    :: integer
    name    :: string
    message :: string

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
    type  :: HeaderType

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

|]
