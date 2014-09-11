{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Templates where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils


add :: ApiKey -> Template -> Publish -> IO (Either ApiError Template)
add k t p = do
    resp <- performRequest "/templates/add.json" mkObj
    return $ parseResponse resp
    where mkObj = encode $ object [ "key"        .= k
                                  , "publish"    .= p
                                  , "name"       .= _tmpl_name       t 
                                  , "from_email" .= _tmpl_from_email t 
                                  , "from_name"  .= _tmpl_from_name  t 
                                  , "subject"    .= _tmpl_subject    t 
                                  , "code"       .= _tmpl_code       t
                                  , "text"       .= _tmpl_text       t
                                  , "labels"     .= _tmpl_labels     t ]

info :: ApiKey -> Name -> IO (Either ApiError Template)
info k n = do
     resp <- performRequest "/templates/info.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k, "name" .= n ]

update :: ApiKey -> Template -> Publish -> IO (Either ApiError Template)
update k t p = do
       resp <- performRequest "/templates/update.json" mkObj
       return $ parseResponse resp
       where mkObj = encode $ object [ "key"        .= k
                                     , "publish"    .= p
                                     , "name"       .= _tmpl_name       t 
                                     , "from_email" .= _tmpl_from_email t 
                                     , "from_name"  .= _tmpl_from_name  t 
                                     , "subject"    .= _tmpl_subject    t 
                                     , "code"       .= _tmpl_code       t
                                     , "text"       .= _tmpl_text       t
                                     , "labels"     .= _tmpl_labels     t ]

publish :: ApiKey -> Name -> IO (Either ApiError Template)
publish k n = do
        resp <- performRequest "/templates/publish.json" mkObj
        return $ parseResponse resp
        where mkObj = encode $ object [ "key" .= k, "name" .= n ]

delete :: ApiKey -> Name -> IO (Either ApiError Template)
delete k n = do
       resp <- performRequest "/templates/delete.json" mkObj
       return $ parseResponse resp
       where mkObj = encode $ object [ "key" .= k, "name" .= n ]

list :: ApiKey -> Label -> IO (Either ApiError [Template])
list k l = do
     resp <- performRequest "/templates/list.json" mkObj
     return $ parseResponse resp
     where mkObj = encode $ object [ "key" .= k, "label" .= l ]

timeSeries :: ApiKey -> Name -> IO (Either ApiError [Stat])
timeSeries k n = do
           resp <- performRequest "/templates/time-series.json" mkObj
           return $ parseResponse resp
           where mkObj = encode $ object [ "key" .= k, "name" .= n ]

render :: ApiKey -> Name -> [Content] -> [MergeVar] -> IO (Either ApiError HtmlFragment)
render k n cs ms = do
       resp <- performRequest "/templates/render.json" mkObj
       return $ parseResponse resp
       where mkObj = encode $ object [ "key"              .= k
                                     , "template_name"    .= n
                                     , "template_content" .= cs
                                     , "merge_vars"       .= ms ]
