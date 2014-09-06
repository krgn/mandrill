{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Mandrill.Types where
 
import Data.API.Tools
import Data.API.Tools.Mandrill
import Network.Mandrill.ApiError
import Network.Mandrill.TH.Utils
import Network.Mandrill.TH.Users
import Network.Mandrill.TH.Messages

-- now, splice in all our data type definitions
$(generate utils)
$(generateAPITools utils [enumTool, jsonTool, lensTool, mandrillTool "ApiError"])

$(generate users)
$(generateAPITools users [enumTool, jsonTool, lensTool, mandrillTool "ApiError"])

$(generate messages)
$(generateAPITools messages [enumTool, jsonTool, lensTool, mandrillTool "ApiError"])

type ApiKey = String
