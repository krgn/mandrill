{-# LANGUAGE OverloadedStrings #-}
module Network.Mandrill.Response where

import Data.Aeson
import Data.API.JSON
import Data.Default
import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy

class (FromJSONWithErrs r) => MandrillResponse r where
  parseResponse :: (FromJSONWithErrs r) => ByteString -> Either ApiError r

-- | We don't generate this type like the others, as we'll need
-- it to parameterize our template splices with it and its instances
data ApiError = ApiError {
      status  :: String,
      code    :: Int,
      name    :: String,
      message :: String
    }
    deriving (Show)

instance Default ApiError where
  def = ApiError {
      status  = "crazyerror",
      code    = 666,
      name    = "Crazy_ErR0r",
      message = "something truly crazy happened"
    }

instance FromJSON ApiError where
  parseJSON (Object v) = ApiError <$>
                         v .: "status" <*>
                         v .: "code" <*>
                         v .: "name" <*>
                         v .: "message" 
  parseJSON _          = mzero

instance FromJSONWithErrs ApiError where
  parseJSONWithErrs val = case fromJSON val of
                           Error e   -> failWith $ SyntaxError e
                           Success v -> pure v
