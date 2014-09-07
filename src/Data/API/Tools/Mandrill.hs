{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.API.Tools.Mandrill
       ( MandrillResponse(..)
       , mandrillTool
       , mandrillListTool ) where

import           Control.Monad (when)
import           Data.API.JSON
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types
import           Data.Monoid
import           Language.Haskell.TH
import           Data.Default
import qualified Data.ByteString.Lazy           as LBS

class (FromJSONWithErrs l, FromJSONWithErrs r) => MandrillResponse l r where
  parseResponse :: (FromJSONWithErrs l, FromJSONWithErrs r) => LBS.ByteString -> Either l r
  
-- | copy/paste from private module
simpleD :: Name -> ExpQ -> Q Dec
simpleD n e = funD n [clause [] (normalB e) []]

-- | copy/paste from private module
optionalInstanceD :: ToolSettings -> Name -> [TypeQ] -> [DecQ] -> Q [Dec]
optionalInstanceD stgs c tqs dqs = do
  ts <- sequence tqs
  ds <- sequence dqs
  exists <- isInstance c ts
  if exists then do
    when (warnOnOmittedInstance stgs) $ reportWarning $ msg ts
    return []
  else 
    return [InstanceD [] (foldl AppT (ConT c) ts) ds]
  where
  msg ts = "instance " ++ pprint c ++ " " ++ pprint ts ++ " already exists, so it was not generated"
	
-- | copy/paste from private module
optionalInstanceDL :: ToolSettings -> Name -> [TypeQ] -> [DecQ] -> Q [Dec]
optionalInstanceDL stgs c tqs dqs = do
  ts <- sequence tqs
  ds <- sequence dqs
  exists <- isInstance c ts
  if exists then do
    when (warnOnOmittedInstance stgs) $ reportWarning $ msg ts
    return []
  else 
    return [InstanceD [] (AppT (AppT (ConT c) (head ts)) (AppT ListT (last ts))) ds]
  where
  msg ts = "instance " ++ pprint c ++ " " ++ pprint ts ++ " already exists, so it was not generated"
	
mandrillTool :: String -> APITool
mandrillTool nm = apiNodeTool $ apiSpecTool
  (gen_sn_ex nm)
  (gen_sr_ex nm)
  (gen_su_ex nm)
  (gen_se_ex nm)
  mempty

mandrillListTool :: String -> APITool
mandrillListTool nm = apiNodeTool $ apiSpecTool
  (gen_sn_l_ex nm)
  (gen_sr_l_ex nm)
  (gen_su_l_ex nm)
  (gen_se_l_ex nm)
  mempty

gen_sr_ex :: String -> Tool (APINode, SpecRecord)
gen_sr_ex errType = mkTool $ \ ts (an, _) -> mkInst ts an errType (expCon an errType)

gen_sr_l_ex :: String -> Tool (APINode, SpecRecord)
gen_sr_l_ex errType = mkTool $ \ ts (an, _) -> mkLInst ts an errType (expLCon an errType)

gen_sn_ex :: String ->Tool (APINode, SpecNewtype)
gen_sn_ex errType = mkTool $ \ ts (an, _) -> mkInst ts an errType (expCon an errType)            

gen_sn_l_ex :: String ->Tool (APINode, SpecNewtype)
gen_sn_l_ex errType = mkTool $ \ ts (an, _) -> mkLInst ts an errType (expLCon an errType)            

gen_su_ex :: String ->Tool (APINode, SpecUnion)
gen_su_ex errType = mkTool $ \ ts (an, _) -> mkInst ts an errType (expCon an errType)

gen_su_l_ex :: String ->Tool (APINode, SpecUnion)
gen_su_l_ex errType = mkTool $ \ ts (an, _) -> mkLInst ts an errType (expLCon an errType)

gen_se_ex :: String ->Tool (APINode, SpecEnum)
gen_se_ex errType = mkTool $ \ ts (an, _) -> mkInst ts an errType (expCon an errType)

gen_se_l_ex :: String ->Tool (APINode, SpecEnum)
gen_se_l_ex errType = mkTool $ \ ts (an, _) -> mkLInst ts an errType (expLCon an errType)

mkInst :: ToolSettings -> APINode -> String -> ExpQ -> Q [Dec]
mkInst ts an errType e =
  optionalInstanceD ts
    ''MandrillResponse
    [conT $ mkName errType, nodeRepT an]
    [simpleD 'parseResponse e]

mkLInst :: ToolSettings -> APINode -> String -> ExpQ -> Q [Dec]
mkLInst ts an errType e =
  let lName = _TypeName $ anName an
  in optionalInstanceDL ts
     ''MandrillResponse
     [conT $ mkName errType, conT $ mkName lName]
     [simpleD 'parseResponse e]

expCon :: APINode -> String -> ExpQ
expCon an errType = [e| \resp ->
  case decodeWithErrs resp :: Either [(JSONError, Position)] $(nodeRepT an) of
   Right u -> Right u
   Left  _ ->
     case decodeWithErrs resp :: Either [(JSONError, Position)] $(conT $ mkName errType) of
      Right e -> Left e
      Left  _ -> Left (def :: $(conT $ mkName errType))
  |]

expLCon :: APINode -> String -> ExpQ
expLCon an errType = [e| \resp ->
  case decodeWithErrs resp :: Either [(JSONError, Position)] [$(nodeRepT an)] of
   Right u -> Right u
   Left  _ ->
     case decodeWithErrs resp :: Either [(JSONError, Position)] $(conT $ mkName errType) of
      Right e -> Left e
      Left  _ -> Left (def :: $(conT $ mkName errType))
  |]
