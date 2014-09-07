module Network.Mandrill.TemplatesSpec where

import Test.Hspec

-- import Network.Mandrill.Templates

key :: String
key = "b0c5wPDu1J9q_7MqPYAqBg"

spec :: Spec
spec = do
  test_add
  test_info
  test_update
  test_publish
  test_delete
  test_list
  test_timeSeries
  test_render

test_add :: Spec
test_add = 
  describe "/templates/add.json" $
    it "" $ do
      --response <- info key
      pending

test_timeSeries :: Spec
test_timeSeries = 
  describe "/templates/time-series.json" $
    it "" $ do
      --response <- info key
      pending

test_render :: Spec
test_render = 
  describe "/templates/render.json" $
    it "" $ do
      --response <- info key
      pending

test_delete :: Spec
test_delete = 
  describe "/templates/delete.json" $
    it "" $ do
      --response <- info key
      pending

test_publish :: Spec
test_publish = 
  describe "/templates/publish.json" $
    it "" $ do
      --response <- info key
      pending

test_update :: Spec
test_update = 
  describe "/templates/update.json" $
    it "" $ do
      --response <- info key
      pending

test_info :: Spec
test_info = 
  describe "/templates/info.json" $
    it "" $ do
      --response <- info key
      pending

test_list :: Spec
test_list = 
  describe "/templates/list.json" $
    it "" $ do
      --response <- info key
      pending
