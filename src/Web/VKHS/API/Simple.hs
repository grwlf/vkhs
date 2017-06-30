-- | This module contains definitions of VK various API bindings. I tried to
-- keep it as simple as possible. More, the user is expected to copy any
-- function from this module into their 'runhaskell' script and customize
-- as required.
--
-- Runhaskell script may looks like the following:
-- @
--     #!/usr/bin/env runhaskell
--     {-# LANGUAGE RecordWildCards #-}
--     {-# LANGUAGE OverloadedStrings #-}
--
--     import Web.VKHS
--     import Web.VKHS.Imports
--
--     main :: IO ()
--     main = runVK_ defaultOptions $ do
--       Sized cnt gs <- groupSearch "Котики"
--       forM_ gs $ \gr@GroupRecord{..} -> do
--         liftIO $ tputStrLn gr_name
--         liftIO $ tputStrLn "--------------"
--         Sized wc ws <- getGroupWall gr
--         forM_ ws $ \WallRecord{..} -> do
--           liftIO $ tputStrLn wr_text
--           liftIO $ tputStrLn "--------------"
-- @
--
-- See more scripts under @./app/runhaskell@ folder
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.VKHS.API.Simple where

import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Web.VKHS.Imports
import Web.VKHS.Types
import Web.VKHS.Monad
import Web.VKHS.Error
import Web.VKHS.Client(requestUploadPhoto, requestExecute, responseBody, responseBodyS)
import Web.VKHS.API.Base
import Web.VKHS.API.Types

max_count = 1000

-- | We are using API v5.44 by default
ver = "5.44"

apiSimpleF nm args f = apiRf nm (("v",ver):args) f
apiSimple nm args = apiR nm (("v",ver):args)
apiSimpleH nm args handler = apiH nm (("v",ver):args) handler
apiSimpleHM nm args handler = apiHM nm (("v",ver):args) handler
apiVer nm args = api nm (("v",ver):args)

-- | This function demonstrates pure-functional error handling
groupSearch :: (MonadAPI m x s) => Text -> API m x (Sized [GroupRecord])
groupSearch q =
  fmap (sortBy (compare `on` gr_members_count)) <$> do
  apiSimpleH "groups.search"
    [("q",q),
     ("fields", "can_post,members_count"),
     ("count", tpack (show max_count))]
    (\ErrorRecord{..} ->
      case er_code of
        AccessDenied -> (Just $ Sized 0 [])
        _ -> Nothing
    )

getCountries :: (MonadAPI m x s) => API m x (Sized [Country])
getCountries =
  fmap (sortBy (compare `on` co_title)) <$> do
  apiSimple "database.getCountries"
    [("v",ver),
     ("need_all", "1"),
     ("count", tpack (show max_count))
    ]

getCities :: (MonadAPI m x s) => Country -> Maybe Text -> API m x (Sized [City])
getCities Country{..} mq =
  apiSimple "database.getCities" $
    [("country_id", tpack (show co_int)),
     ("count", tpack (show max_count))
    ] ++
    maybe [] (\q -> [("q",q)]) mq

-- | See [https://vk.com/dev/wall.get]
--
-- This function demonstrates monadic error handling
getGroupWall :: forall m x s . (MonadAPI m x s) => GroupRecord -> API m x (Sized [WallRecord])
getGroupWall GroupRecord{..} =
  apiSimpleHM "wall.get"
    [("owner_id", "-" <> tshow gr_id),
     ("count", "100")
    ]
    (\ErrorRecord{..} ->
      case er_code of
        AccessDenied -> do
          return (Just $ Sized 0 [])
        _ -> do
          return Nothing
        :: API m x (Maybe (Sized [WallRecord])))

-- TODO: Take User as argument for more type-safety
getAlbums :: (MonadAPI m x s) => Maybe Integer -> API m x (Sized [Album])
getAlbums muid =
  apiSimple "photos.getAlbums" $
    (case muid of
     Just uid -> [("owner_id", tshow uid)]
     Nothing -> [])
    <>
    [("need_system", "1")
    ]

getPhotoUploadServer :: (MonadAPI m x s) => Album -> API m x PhotoUploadServer
getPhotoUploadServer Album{..} =
  apiSimple "photos.getUploadServer" [("album_id", tshow al_id)]


getCurrentUser :: (MonadAPI m x s) => API m x UserRecord
getCurrentUser = do
  apiSimpleF "users.get" [] $ \users ->
    case (length users == 1) of
      False -> Left "getCurrentUser: array with one user record"
      True -> Right (head users)


-- * FIXME fix setUserPhoto, it is not actually working
-- * FIXME move low-level upload code to API.Base
setUserPhoto :: (MonadAPI m x s) => UserRecord -> FilePath -> API m x ()
setUserPhoto UserRecord{..} photo_path =  do
  photo <- liftIO $ BS.readFile photo_path
  OwnerUploadServer{..} <-
    resp_data <$> api "photos.getOwnerPhotoUploadServer"
      [("owner_id", tshow ur_id)]
  req <- ensure $ requestUploadPhoto ous_upload_url photo
  (res, _) <- requestExecute req
  j@JSON{..} <- decodeJSON (responseBody res)
  liftIO $ BS.putStrLn $ (responseBody res)
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



