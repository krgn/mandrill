{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Types where
 
import Data.API.Tools
import Network.Mandrill.TH.Utils
import Network.Mandrill.TH.Users
import Network.Mandrill.TH.Messages

$(generate utils)
$(generateAPITools utils [enumTool, jsonTool, lensTool])

$(generate users)
$(generateAPITools users [enumTool, jsonTool, lensTool])

$(generate messages)
$(generateAPITools messages [enumTool, jsonTool, lensTool])

type ApiKey = String
