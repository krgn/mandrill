{-# LANGUAGE QuasiQuotes #-}

module Network.Mandrill.TH.Messages where

import Data.API.Parse
import Data.API.Types

messages :: API
messages = [api|

metadata :: Metadata 
         // Message metadata
         = record 
           website :: string

recipientMetadata :: RecipientMetadata
                  // per recipient metadata 
                  = record
                    rcpt :: Email
                    values :: Metadata

mergeVar :: MergeVar
         // A merge var record
         = record
           name    :: string
           content :: string

recipientMergeVar :: RecipientMergeVar
                  // bla
                  = record
                    recipient :: Email
                    vars      :: [MergeVar] 

message :: Message
        // A message structure
        = record 
          html                      :: string
          text                      :: string
          subject                   :: string
          from_email                :: Email
          from_name                 :: string
          to                        :: [Recipient]
          important                 :: boolean
          track_opens               :: boolean
          track_clicks              :: boolean
          auto_text                 :: boolean
          auto_html                 :: boolean
          inline_css                :: boolean
          url_strip_qs              :: boolean
          preserve_recipients       :: boolean
          view_content_link         :: boolean
          bcc_address               :: Email
          tracking_domain           :: string
          signing_domain            :: string
          return_path_domain        :: string
          merge                     :: boolean
          global_merge_vars         :: [MergeVar]
          merge_vars                :: [RecipientMergeVar]
          tags                      :: [Tag]
          subaccount                :: string
          google_analytics_domains  :: [string]
          google_analytics_campaign :: string
          metadata                  :: Metadata
          recipient_metadata        :: [RecipientMetadata]
          attachments               :: [Attachment]
          images                    :: [Image]

|]

