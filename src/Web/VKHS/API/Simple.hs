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

max_count :: Integer
max_count = 1000

-- | We are using API v5.44 by default
ver :: Text
ver = "5.44"

-- | Versioned aliases for api caller functions
-- apiSimpleF nm args f = apiRf nm (("v",ver):args) f
apiSimple1 :: (MonadAPI m x s, FromJSON a) => MethodName -> [(String, Text)] -> API m x a
apiSimple1 nm args = api1 nm (("v",ver):args)

apiSimple2 :: (MonadAPI m x s, FromJSON b, FromJSON a) => MethodName -> [(String, Text)] -> API m x (Either a b)
apiSimple2 nm args = api2 nm (("v",ver):args)
-- apiSimpleHM nm args handler = apiHM nm (("v",ver):args) handler

apiSimple :: (FromJSON a, MonadAPI m x s) => MethodName -> [(String, Text)] -> API m x a
apiSimple nm args = apiSimple1 nm args
-- apiSimpleE nm args handler = do

apiSimpleH :: (FromJSON t, MonadAPI m x s) => MethodName -> [(String, Text)] -> (t -> b) -> (APIErrorRecord -> Either Text b) -> m (R m x) b
apiSimpleH nm args handlerA handlerB = do
  res <- apiSimple2 nm args
  case res of
    Left e ->
      case handlerB e of
        Left text -> terminate $ APIFailed $ APIUnhandledError nm e text
        Right a -> return a
    Right a -> return (handlerA a)



-- | Wrapper for 'groups.search' handler. The function demonstrates
-- pure-functional error handling.
groupSearch :: (MonadAPI m x s) => Text -> API m x [GroupRecord]
groupSearch q =
  sortBy (compare `on` gr_members_count) <$> do
    m_items <$> do
      apiSimpleH "groups.search"
        [("q",q),
         ("fields", "can_post,members_count"),
         ("count", tpack (show max_count))]
        id
        (\APIErrorRecord{..} ->
          case er_code of
            AccessDenied -> Right (Sized 0 [])
            _ -> Left ""
        )

-- | Get list of countries, known to VK
getCountries :: (MonadAPI m x s) => API m x [Country]
getCountries =
  sortBy (compare `on` co_title) <$> do
  m_items <$> do
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
  apiSimpleH "wall.get"
    [("owner_id", "-" <> (tshow $ gid_id $ gr_gid)),
     ("count", "100")
    ]
    id
    (\APIErrorRecord{..} ->
      case er_code of
        AccessDenied -> (Right $ Sized 0 [])
        _ -> Left ""
    )

-- | Wrapper for [https://vk.com/dev/wall.get] function
getWall :: forall m x s . (MonadAPI m x s) => Int -> Int -> API m x (Sized [WallRecord])
getWall owner_id count =
  apiSimpleH "wall.get"
    [("owner_id", tshow owner_id),
     ("count", tshow count)
    ]
    id
    (\APIErrorRecord{..} ->
      case er_code of
        AccessDenied -> Right $ Sized 0 []
        _ -> Left ""
    )

-- | https://vk.com/dev/wall.getById
getWallById :: (MonadAPI m x s) => (Int, Int) -> API m x (Maybe WallRecord)
getWallById (owner_id, post_id) = do
  apiSimpleH "wall.getById"
    [("posts", tshow owner_id <> "_" <> tshow post_id)
    ]
    Just
    (\APIErrorRecord{..} ->
      case er_code of
        AccessDenied -> Right Nothing
        _ -> Left ""
    )

-- | Return modified and unmodified reposts of this wall recor. Algorithm is
-- based on the following methods:
--    https://vk.com/dev/wall.getReposts
--    https://vk.com/dev/likes.getList
-- See also https://habrahabr.ru/post/177641 (in Russian) for explanation
getWallReposts :: (MonadAPI m x s) => WallRecord -> API m x [WallRecord]
getWallReposts wr = do
  modified_reposts <- apiSimple "wall.getReposts" $
    [ ("owner_id",tshow (wr_owner_id wr))
    , ("post_id",tshow (wr_id wr))
    , ("count",tshow max_count)
    ]

  (Sized cnt owners :: Sized [Int]) <-
    apiSimple "likes.getList" $
      [ ("type","post")
      , ("owner_id",tshow (wr_owner_id wr))
      , ("item_id",tshow (wr_id wr))
      , ("filter","copies")
      , ("count",tshow max_count)
      ]

  unmodified_reposts <-
    concat <$> do
      forM owners $ \o -> do
        (Sized _ wrs) <- getWall o 20
        return [ x | x <- wrs, (wr_id wr) `elem`(map wr_id (wr_copy_history x))]
  return $ (rr_items modified_reposts) <> unmodified_reposts

-- TODO: Take User as argument for more type-safety
getAlbums :: (MonadAPI m x s) => Maybe Integer -> API m x (Sized [Album])
getAlbums muid =
  apiSimpleH "photos.getAlbums"
    ((case muid of
      Just uid -> [("owner_id", tshow uid)]
      Nothing -> [])
     <>
     [("need_system", "1")])
    id
    (\APIErrorRecord{..} ->
      case er_code of
        AccessDenied -> Right (Sized 0 [])
        _ -> Left ""
    )

getPhotoUploadServer :: (MonadAPI m x s) => Album -> API m x PhotoUploadServer
getPhotoUploadServer Album{..} =
  apiSimple "photos.getUploadServer" [("album_id", tshow al_id)]


getUsers :: (MonadAPI m x s) => [UserId] -> API m x [UserRecord]
getUsers uids = do
  apiSimple "users.get" [
      ("user_ids", Text.intercalate "," [tshow x | UserId x <- uids])
    , ("fields", "city,country")
    ]

-- | Get current user
getCurrentUser :: (MonadAPI m x s) => API m x UserRecord
getCurrentUser = do
  users <- apiSimple "users.get" []
  case (length users == 1) of
    False -> terminate $ APIFailed $ APIUnexpected "users.get" "should be and array containing a single user record"
    True -> return $ head users

--    * FIXME move low-level upload code to API.Base
setUserPhoto :: (MonadAPI m x s) => UserRecord -> FilePath -> API m x ()
setUserPhoto UserRecord{..} photo_path =  do
  OwnerUploadServer{..} <-
    (fst . resp_data) <$> apiSimple "photos.getOwnerPhotoUploadServer"
      [("owner_id", tshow $ uid_id $ ur_uid)]

  UploadRecord{..} <- upload ous_upload_url photo_path

  Response{..} <- apiSimple "photos.saveOwnerPhoto"
      [("server", tshow upl_server)
      ,("hash", upl_hash)
      ,("photo", upl_photo)]
  PhotoSaveResult{..} <- pure (fst resp_data)
  return ()

getGroupMembers :: (MonadAPI m x s) => GroupId -> API m x [UserId]
getGroupMembers GroupId{..} =
  m_items <$>
    (apiSimple "groups.getMembers"
      [ ("group_id", tshow gid_id)
      , ("count", tshow max_count)
      ])
