{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Web.VKHS.API.Simple where

import Control.Monad.Trans (liftIO)
import Data.List
import Data.Text (Text)
import Data.Monoid((<>))
import qualified Data.Text as Text
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Function
import Web.VKHS.Types
import Web.VKHS.Monad
import Web.VKHS.Error
import Web.VKHS.Types(tshow)
import Web.VKHS.Client(requestUploadPhoto, requestExecute, responseBody, responseBodyS)
import Web.VKHS.API.Base
import Web.VKHS.API.Types

max_count = 1000
ver = "5.44"

apiSimple def nm args = apiD def nm (("v",ver):args)
apiVer nm args = api nm (("v",ver):args)

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
  apiR "database.getCountries" $
    [("v",ver),
     ("need_all", "1"),
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

-- TODO: Take User as argument for more type-safety
getAlbums :: (MonadAPI m x s) => Maybe Integer -> API m x (Sized [Album])
getAlbums muid =
  resp_data <$> do
  apiSimple emptyResponse "photos.getAlbums" $
    (case muid of
     Just uid -> [("owner_id", tshow uid)]
     Nothing -> [])
    <>
    [("need_system", "1")
    ]

getPhotoUploadServer :: (MonadAPI m x s) => Album -> API m x PhotoUploadServer
getPhotoUploadServer Album{..} =
  resp_data <$> do
  api "photos.getUploadServer" $
    [("album_id", tshow al_id)
    ]


getCurrentUser :: (MonadAPI m x s) => API m x UserRecord
getCurrentUser = do
  Response{..} <- apiVer "users.get" []
  users <- pure resp_data
  case (length users == 1) of
    False -> terminate (JSONParseFailure' resp_json "getCurrentUser: expecting single UserRecord")
    True -> return (head users)


-- FIXME: move low-level upload code to API.Base
setUserPhoto :: (MonadAPI m x s) => UserRecord -> FilePath -> API m x ()
setUserPhoto UserRecord{..} photo_path =  do
  photo <- liftIO $ BS.readFile photo_path
  OwnerUploadServer{..} <-
    resp_data <$> api "photos.getOwnerPhotoUploadServer"
      [("owner_id", tshow ur_id)]
  req <- ensure $ requestUploadPhoto ous_upload_url photo
  (res, _) <- requestExecute req
  j@JSON{..} <- parseJSON (responseBody res)
  liftIO $ putStrLn $ (responseBodyS res)
  UploadRecord{..} <-
    case Aeson.parseEither Aeson.parseJSON js_aeson of
      Right a -> return a
      Left e -> terminate (JSONParseFailure' j e)
  Response{..} <- api "photos.saveOwnerPhoto"
      [("server", tshow upl_server)
      ,("hash", upl_hash)
      ,("photo", upl_photo)]
  PhotoSaveResult{..} <- pure resp_data
  return ()



