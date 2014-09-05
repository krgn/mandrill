module Network.Mandrill.Users where

import Network.Mandrill.Types
import Network.HTTP

-- simpleHTTP (postRequestWithBody (url ++ "/users/ping2.json") "application/json" (CBS.unpack $ LBS.toStrict input))
listUsers :: ApiKey -> [User]
listUsers = undefined
