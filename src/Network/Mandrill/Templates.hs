{-# LANGUAGE OverloadedStrings #-}

module Network.Mandrill.Templates where

import Network.Mandrill.Response
import Network.Mandrill.Types
import Network.Mandrill.Utils

-- | Add a new template
add :: ApiKey -> Template -> Publish -> IO (Either ApiError Template)
add k t p =
    performRequest "/templates/add.json" $ 
      object [ "key"        .= k
             , "publish"    .= p
             , "name"       .= _tmpl_name       t 
             , "from_email" .= _tmpl_from_email t 
             , "from_name"  .= _tmpl_from_name  t 
             , "subject"    .= _tmpl_subject    t 
             , "code"       .= _tmpl_code       t
             , "text"       .= _tmpl_text       t
             , "labels"     .= _tmpl_labels     t ]


-- | Get the information for an existing template
info :: ApiKey -> Name -> IO (Either ApiError Template)
info k n =
     performRequest "/templates/info.json" $
       object [ "key" .= k, "name" .= n ]


-- | Update the code for an existing template. If null is provided for any 
-- fields, the values will remain unchanged.
update :: ApiKey -> Template -> Publish -> IO (Either ApiError Template)
update k t p =
       performRequest "/templates/update.json" $ 
         object [ "key"        .= k
                , "publish"    .= p
                , "name"       .= _tmpl_name       t 
                , "from_email" .= _tmpl_from_email t 
                , "from_name"  .= _tmpl_from_name  t 
                , "subject"    .= _tmpl_subject    t 
                , "code"       .= _tmpl_code       t
                , "text"       .= _tmpl_text       t
                , "labels"     .= _tmpl_labels     t ]


-- | Publish the content for the template. Any new messages sent using this  
-- template will start using the content that was previously in draft.
publish :: ApiKey -> Name -> IO (Either ApiError Template)
publish k n = 
        performRequest "/templates/publish.json" $
          object [ "key" .= k, "name" .= n ]


-- | Delete a template
delete :: ApiKey -> Name -> IO (Either ApiError Template)
delete k n = 
       performRequest "/templates/delete.json" $
         object [ "key" .= k, "name" .= n ]


-- | Return a list of all the templates available to this user
list :: ApiKey -> Label -> IO (Either ApiError [Template])
list k l = 
     performRequest "/templates/list.json" $
       object [ "key" .= k, "label" .= l ]


-- | Return the recent history (hourly stats for the last 30 days) for a template
timeSeries :: ApiKey -> Name -> IO (Either ApiError [Stat])
timeSeries k n = 
           performRequest "/templates/time-series.json" $
             object [ "key" .= k, "name" .= n ]


-- | Inject content and optionally merge fields into a template, returning 
-- the HTML that results
render :: ApiKey -> Name -> [Content] -> [MergeVar] -> IO (Either ApiError HtmlFragment)
render k n cs ms = 
       performRequest "/templates/render.json" $
         object [ "key"              .= k
                , "template_name"    .= n
                , "template_content" .= cs
                , "merge_vars"       .= ms ]
