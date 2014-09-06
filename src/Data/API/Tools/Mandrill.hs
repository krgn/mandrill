{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.API.Tools.Mandrill
       ( MandrillResponse(..)
       , mandrillTool ) where

import           Control.Monad (when)
import           Data.API.JSON
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types
import           Data.API.Utils
import           Data.Monoid
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Data.Default
import qualified Data.ByteString.Lazy           as LBS

class (FromJSONWithErrs l, FromJSONWithErrs r) => MandrillResponse l r where
  parseResponse :: (FromJSONWithErrs l, FromJSONWithErrs r) => LBS.ByteString -> Either l r

-- | Construct a simple TH definition
simpleD :: Name -> ExpQ -> Q Dec
simpleD n e = funD n [clause [] (normalB e) []]

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
	
mandrillTool :: String -> APITool
mandrillTool nm = apiNodeTool $ apiSpecTool
                  (gen_sn_ex nm)
                  (gen_sr_ex nm)
                  (gen_su_ex nm)
                  (gen_se_ex nm)
                  mempty

gen_sr_ex :: String -> Tool (APINode, SpecRecord)
gen_sr_ex errType = mkTool $ \ ts (an, sn) -> mkInst ts an errType (expCon an errType)

gen_sn_ex :: String ->Tool (APINode, SpecNewtype)
gen_sn_ex errType = mkTool $ \ ts (an, sn) -> mkInst ts an errType (expCon an errType)            

gen_su_ex :: String ->Tool (APINode, SpecUnion)
gen_su_ex errType = mkTool $ \ ts (an, sn) -> mkInst ts an errType (expCon an errType)

gen_se_ex :: String ->Tool (APINode, SpecEnum)
gen_se_ex errType = mkTool $ \ ts (an, sn) -> mkInst ts an errType (expCon an errType)

mkInst :: ToolSettings -> APINode -> String -> ExpQ -> Q [Dec]
mkInst ts an errType e =
  optionalInstanceD ts
    ''MandrillResponse
    [conT $ mkName errType, nodeRepT an]
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

--  parseResponse resp =
--    case decodeWithErrs resp :: Either [(JSONError, Position)] User of
--     Right u -> Right u
--     Left  _ ->
--       case decodeWithErrs resp :: Either [(JSONError, Position)] ApiError of
--        Right e -> Left e
--        Left  _ -> Left ApiError {
--            _apierror_status  = "crazyerror",
--            _apierror_code    = 666, 
--            _apierror_name    = "Crazy_Error",
--            _apierror_message = "Uh oh, not good."
--          }
