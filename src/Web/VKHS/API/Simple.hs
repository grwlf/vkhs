{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Web.VKHS.API.Simple where

import Data.List
import Data.Text (Text)
import Data.Monoid((<>))
import qualified Data.Text as Text
import Data.Function
import Web.VKHS.Types
import Web.VKHS.API.Base
import Web.VKHS.API.Types

max_count = 1000
ver = "5.44"

apiSimple def nm args = apiD def nm (("v",ver):args)

groupSearch :: (MonadAPI m x s) => Text -> API m x (Sized [GroupRecord])
groupSearch q =
  fmap (sortBy (compare `on` gr_members_count)) <$>
  resp_data <$> do
  apiSimple emptyResponse "groups.search" $
    [("q",q),
     ("fields", "can_post,members_count"),
     ("count", tpack (show max_count))]

getCountries :: (MonadAPI m x s) => API m x (Sized [Country])
getCountries =
  fmap (sortBy (compare `on` co_title)) <$> do
  resp_data <$> do
  apiSimple emptyResponse "database.getCountries" $
    [("need_all", "1"),
     ("count", tpack (show max_count))
    ]

getCities :: (MonadAPI m x s) => Country -> Maybe Text -> API m x (Sized [City])
getCities Country{..} mq =
  resp_data <$> do
  apiSimple emptyResponse "database.getCities" $
    [("country_id", tpack (show co_int)),
     ("count", tpack (show max_count))
    ] ++
    maybe [] (\q -> [("q",q)]) mq

getGroupWall :: (MonadAPI m x s) => GroupRecord -> API m x (Sized [WallRecord])
getGroupWall GroupRecord{..} =
  resp_data <$> do
  apiSimple emptyResponse "wall.get" $
    [("owner_id", "-" <> tshow gr_id),
     ("count", "100")
    ]

