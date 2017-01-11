{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Web.VKHS.API.Simple where

import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Function
import Web.VKHS.Types
import Web.VKHS.API.Base
import Web.VKHS.API.Types

max_count = 1000

-- | Use API v5.44 for now
api_ver nm args = api nm (("v","5.44"):args)

groupSearch :: (MonadAPI m x s) => Text -> API m x (Sized [GroupRecord])
groupSearch q =
  fmap (sortBy (compare `on` gr_members_count)) <$>
  resp_data <$> do
  api_ver "groups.search" $
    [("q",q),
     ("fields", "can_post,members_count"),
     ("count", tpack (show max_count))]

getCountries :: (MonadAPI m x s) => API m x (Sized [Country])
getCountries =
  fmap (sortBy (compare `on` co_title)) <$> do
  resp_data <$> do
  api_ver "database.getCountries" $
    [("need_all", "1"),
     ("count", tpack (show max_count))
    ]

getCities :: (MonadAPI m x s) => Country -> Maybe Text -> API m x (Sized [City])
getCities Country{..} mq =
  resp_data <$> do
  api_ver "database.getCities" $
    [("country_id", tpack (show co_int)),
     ("count", tpack (show max_count))
    ] ++
    maybe [] (\q -> [("q",q)]) mq
