{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Mandrill.Types 
  ( module Network.Mandrill.Types
  , module Network.Mandrill.ApiError
  , module Data.API.JSON
  ) where

import Data.API.JSON
import Data.API.Tools
import Data.API.Tools.Mandrill
import Network.Mandrill.ApiError
import Network.Mandrill.TH.Utils
import Network.Mandrill.TH.Users
import Network.Mandrill.TH.Messages

-- now, splice in all our data type definitions
$(generate utils)
$(generateAPITools utils
  [enumTool, jsonTool, mandrillTool "ApiError", mandrillListTool "ApiError"])

$(generate users)
$(generateAPITools users
  [enumTool, jsonTool, mandrillTool "ApiError", mandrillListTool "ApiError"])

$(generate messages)
$(generateAPITools messages
  [enumTool, jsonTool, mandrillTool "ApiError", mandrillListTool "ApiError"])

type ApiKey = String
