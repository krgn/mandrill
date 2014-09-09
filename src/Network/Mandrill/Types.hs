{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Mandrill.Types 
  ( module Network.Mandrill.Types
  , module Network.Mandrill.Response
  , module Data.API.JSON
  ) where

import Data.API.JSON
import Data.API.Tools
import Data.API.Tools.Mandrill
import Network.Mandrill.Response
import Network.Mandrill.TH.Utils
import Network.Mandrill.TH.Types

-- now, splice in all our data type definitions
$(generate utils)
$(generateAPITools utils [enumTool, jsonTool, mandrillTool])

$(generate mandrillApi)
$(generateAPITools mandrillApi [enumTool, jsonTool, mandrillTool])
