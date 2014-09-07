module Network.Mandrill.IpsSpec where

import Test.Hspec

--import Network.Mandrill.Ips

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

spec :: Spec
spec = do
  test_list
  test_info
  test_provision
  test_startWarmup
  test_cancelWarmup
  test_setPool
  test_delete
  test_listPools
  test_poolInfo
  test_createPool
  test_deletePool
  test_checkCustomDns
  test_setCustomDns

test_setCustomDns :: Spec
test_setCustomDns = 
  describe "/ips/set-custom-dns.json" $
    it "" $ do
      --response <- info key
      pending

test_checkCustomDns :: Spec
test_checkCustomDns = 
  describe "/ips/check-custom-dns.json" $
    it "" $ do
      --response <- info key
      pending

test_deletePool :: Spec
test_deletePool = 
  describe "/ips/delete-pool.json" $
    it "" $ do
      --response <- info key
      pending

test_createPool :: Spec
test_createPool = 
  describe "/ips/create-pool.json" $
    it "" $ do
      --response <- info key
      pending

test_poolInfo :: Spec
test_poolInfo = 
  describe "/ips/pool-info.json" $
    it "" $ do
      --response <- info key
      pending

test_listPools :: Spec
test_listPools = 
  describe "/ips/list-pools.json" $
    it "" $ do
      --response <- info key
      pending

test_delete :: Spec
test_delete = 
  describe "/ips/delete.json" $
    it "" $ do
      --response <- info key
      pending

test_setPool :: Spec
test_setPool = 
  describe "/ips/set-pool.json" $
    it "" $ do
      --response <- info key
      pending

test_cancelWarmup :: Spec
test_cancelWarmup = 
  describe "/ips/cancel-warmup.json" $
    it "" $ do
      --response <- info key
      pending

test_startWarmup :: Spec
test_startWarmup = 
  describe "/ips/start-warmup.json" $
    it "" $ do
      --response <- info key
      pending

test_provision :: Spec
test_provision = 
  describe "/ips/provision.json" $
    it "" $ do
      --response <- info key
      pending

test_info :: Spec
test_info = 
  describe "/ips/info.json" $
    it "" $ do
      --response <- info key
      pending

test_list :: Spec
test_list = 
  describe "/ips/list.json" $
    it "" $ do
      --response <- info key
      pending
