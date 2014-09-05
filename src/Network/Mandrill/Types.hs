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
$(generateAPITools utils [enumTool, jsonTool, quickCheckTool])

$(generate users)
$(generateAPITools users [enumTool, jsonTool, quickCheckTool])

$(generate messages)
$(generateAPITools messages [enumTool, jsonTool, quickCheckTool])

type ApiKey = String
