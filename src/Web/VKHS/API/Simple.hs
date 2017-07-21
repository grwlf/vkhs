-- | This module contains definitions of various VK API bindings. It is desigend
-- to be as simple as possible. The collection is not even close to be complete.
-- The user is expected to copy-and-paste any function from this module into
-- their 'runhaskell' script and customize it as required.
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
-- See also a collection of scripts in the @./app/runhaskell@ folder of
-- the distribution package.
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

-- | Versioned aliases for api caller functions
apiSimpleF nm args f = apiRf nm (("v",ver):args) f
apiSimple nm args = apiR nm (("v",ver):args)
apiSimpleH nm args handler = apiH nm (("v",ver):args) handler
apiSimpleHM nm args handler = apiHM nm (("v",ver):args) handler

-- | Wrapper for 'groups.search' handler. The function demonstrates
-- pure-functional error handling.
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

-- | Get list of countries, known to VK
getCountries :: (MonadAPI m x s) => API m x (Sized [Country])
getCountries =
  fmap (sortBy (compare `on` co_title)) <$> do
  apiSimple "database.getCountries"
    [("v",ver),
     ("need_all", "1"),
     ("count", tpack (show max_count))
    ]

-- | Get list of country cities, known to VK
getCities :: (MonadAPI m x s) => Country -> Maybe Text -> API m x (Sized [City])
getCities Country{..} mq =
  apiSimple "database.getCities" $
    [("country_id", tpack (show co_int)),
     ("count", tpack (show max_count))
    ] ++
    maybe [] (\q -> [("q",q)]) mq

-- | Wrapper for [https://vk.com/dev/wall.get] function
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


-- | https://vk.com/dev/wall.getById
getWallById :: (MonadAPI m x s) => (Int, Int) -> API m x (Maybe WallRecord)
getWallById (owner_id, post_id) = do
  listToMaybe <$> do
  apiSimpleH "wall.getById"
    [("posts", tshow owner_id <> "_" <> tshow post_id)
    ]
    (\ErrorRecord{..} ->
      case er_code of
        _ -> Nothing
    )

-- | https://vk.com/dev/wall.getReposts
getWallReposts :: (MonadAPI m x s) => WallRecord -> API m x RepostRecord
getWallReposts WallRecord{..} = do
  apiSimple "wall.getReposts" $
    [ ("owner_id",tshow wr_owner_id)
    , ("post_id",tshow wr_id)
    , ("count",tshow 1000)
    ]

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


-- | Get current user
getCurrentUser :: (MonadAPI m x s) => API m x UserRecord
getCurrentUser = do
  apiSimpleF "users.get" [] $ \users ->
    case (length users == 1) of
      False -> Left "getCurrentUser: should be and array containing a single ser record"
      True -> Right (head users)


--    * FIXME move low-level upload code to API.Base
setUserPhoto :: (MonadAPI m x s) => UserRecord -> FilePath -> API m x ()
setUserPhoto UserRecord{..} photo_path =  do
  OwnerUploadServer{..} <-
    resp_data <$> api "photos.getOwnerPhotoUploadServer"
      [("owner_id", tshow ur_id)]
  req <- ensure $ requestUploadPhoto ous_upload_url photo_path
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

