module Network.Mandrill.InboundSpec where

import Test.Hspec

--import Network.Mandrill.Inbound

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

spec :: Spec
spec = do
  test_domains
  test_addDomain
  test_checkDomain
  test_deleteDomain
  test_routes
  test_addRoute
  test_updateRoute
  test_deleteRoute
  test_sendRaw

test_sendRaw :: Spec
test_sendRaw = 
  describe "/inbound/send-raw.json" $
    it "" $ do
      --response <- info key
      pending

test_deleteRoute :: Spec
test_deleteRoute = 
  describe "/inbound/delete-route.json" $
    it "" $ do
      --response <- info key
      pending

test_updateRoute :: Spec
test_updateRoute = 
  describe "/inbound/update-route.json" $
    it "" $ do
      --response <- info key
      pending

test_addRoute :: Spec
test_addRoute = 
  describe "/inbound/add-route.json" $
    it "" $ do
      --response <- info key
      pending

test_routes :: Spec
test_routes = 
  describe "/inbound/routes.json" $
    it "" $ do
      --response <- info key
      pending

test_deleteDomain :: Spec
test_deleteDomain = 
  describe "/inbound/delete-domain.json" $
    it "" $ do
      --response <- info key
      pending

test_checkDomain :: Spec
test_checkDomain = 
  describe "/inbound/check-domain.json" $
    it "" $ do
      --response <- info key
      pending

test_addDomain :: Spec
test_addDomain = 
  describe "/inbound/add-domain.json" $
    it "" $ do
      --response <- info key
      pending

test_domains :: Spec
test_domains = 
  describe "/inbound/domains.json" $
    it "" $ do
      --response <- info key
      pending
