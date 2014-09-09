{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Mandrill.TH.Utils where

import Data.API.Parse
import Data.API.Types

utils :: API
utils = [api|

lbl :: Label
  // a template label
  = string

sbcntid :: SubaccountId
  // a subaccount 
  = string

athk :: AuthKey
  // an auth key synonym
  = string
  
tplnm :: TemplateName
  // type synoym for the name of a template
  = string

tplcnt :: TemplateContent
  // type synoym for the name of a template
  = record
    name :: TemplateName
    content :: string
 
exptst :: ExportType
  // type of an export
  = enum 
    | activity
    | reject
    | whitelist

exptst :: ExportState 
  // state of an export
  = enum 
    | waiting
    | working
    | complete
    | error
    | expired 

ss :: SendStatus
  // The status enum 
  = enum
    | sent
    | queued
    | bounced
    | rejected
    | invalid

rr :: RejectReason
  // Reason enum when message was rejected
  = enum
    | hard_bounce
    | soft_bounce
    | spam
    | unsub
    | custom
    | invalid_sender
    | invalid 
    | test_mode_limit
    | rule 

acctst :: AccountStatus
  // enum for status
  = enum
    | active
    | paused

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
