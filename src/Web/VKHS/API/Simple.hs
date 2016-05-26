module Web.VKHS.API.Simple where

import Data.List
import Data.Function
import Web.VKHS.API.Base
import Web.VKHS.API.Types

runGroupSearch :: (MonadAPI m x s) => String -> API m x (Sized [GroupRecord])
runGroupSearch q =
  fmap (sortBy (compare `on` gr_members_count)) <$>
  resp_data <$> do
  api "groups.search"
    [("q",q),
     ("v","5.44"),
     ("fields", "can_post,members_count"),
     ("count", "1000")]


