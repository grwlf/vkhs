-- | This module contains wrappers for common API data types. The collection is
-- incomplete and may be out of date, since API is evolving constantly. Users
-- should consider extending this set by their own per-task definitions.
--
-- See [VK development docs](https://vk.com/dev) for the official documentation
--
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Web.VKHS.API.Types where

import Data.Time.Clock
import Data.Time.Clock.POSIX

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Vector as Vector (head, tail)

import Web.VKHS.Imports
import Web.VKHS.Error
import Web.VKHS.Types

import Web.VKHS.API.Base

{-# DEPRECATED SizedList "a) Use Sized instead. b) newer API verison may not need this" #-}
data SizedList a = SizedList Int [a]
  deriving(Show, Data, Typeable)

instance (FromJSON a) => FromJSON (SizedList a) where
  parseJSON = Aeson.withArray "SizedList" $ \v -> do
    n <- Aeson.parseJSON (Vector.head v)
    t <- Aeson.parseJSON (Aeson.Array (Vector.tail v))
    return (SizedList n t)

{-# DEPRECATED MusicRecord "music API was disabled by VK, unfortunately" #-}
data MusicRecord = MusicRecord
  { mr_id :: Int
  , mr_owner_id :: Int
  , mr_artist :: String
  , mr_title :: String
  , mr_duration :: Int
  , mr_url_str :: String
  } deriving (Show, Data, Typeable)

instance FromJSON MusicRecord where
  parseJSON = Aeson.withObject "MusicRecord" $ \o ->
    MusicRecord
      <$> (o .: "aid")
      <*> (o .: "owner_id")
      <*> (o .: "artist")
      <*> (o .: "title")
      <*> (o .: "duration")
      <*> (o .: "url")


{-
 - API version 5.44
 - <https://vk.com/dev/json_schema>
 -}


data RepostRecord = RepostRecord {
    rr_items :: [WallRecord]
  , rr_groups :: [GroupRecord]
  , rr_profiles :: [UserRecord]
  }
  deriving (Show, Data, Typeable)

instance FromJSON RepostRecord where
  parseJSON = Aeson.withObject "RepostRecord" $ \o ->
    RepostRecord
      <$> (o .: "items")
      <*> (o .: "groups")
      <*> (o .: "profiles")

data UserId = UserId { uid_id :: Integer }
  deriving(Show, Data, Eq, Typeable)

instance FromJSON UserId where
  parseJSON j = UserId <$> (Aeson.parseJSON j)

data UserRecord = UserRecord
  { ur_uid :: UserId
  , ur_first_name :: Text
  , ur_last_name :: Text
  , ur_deactivated :: Maybe Text
  , ur_hidden :: Maybe Integer
  -- , ur_photo :: String
  -- , ur_university :: Maybe Int
  -- , ur_university_name :: Maybe String
  -- , ur_faculty :: Maybe Int
  -- , ur_faculty_name :: Maybe String
  -- , ur_graduation :: Maybe Int
  } deriving (Show, Data, Typeable)

instance FromJSON UserRecord where
  parseJSON = Aeson.withObject "UserRecord" $ \o ->
    UserRecord
      <$> (UserId <$> (o .: "id"))
      <*> (o .: "first_name")
      <*> (o .: "last_name")
      <*> (o .:? "deactivated")
      <*> (o .:? "hidden")

-- | Wall post representation (partial)
--
-- See also https://vk.com/dev/objects/post
data WallRecord = WallRecord
  { wr_id :: Int
  , wr_from_id :: Int
  , wr_owner_id :: Int
  , wr_text :: Text
  , wr_date :: Int
  , wr_posttype :: Text
  , wr_attachments_json :: Maybe JSON
  , wr_copy_history :: [WallRecord]
  } deriving (Eq, Show, Data, Typeable)

instance FromJSON WallRecord where
  parseJSON = Aeson.withObject "WallRecord" $ \o ->
    WallRecord
      <$> (o .: "id")
      <*> (o .: "from_id")
      <*> (o .: "owner_id" <|> o .: "to_id")
      <*> (o .: "text")
      <*> (o .: "date")
      <*> (o .: "post_type")
      <*> (o .:? "attachments")
      <*> (fromMaybe [] <$> (o .:? "copy_history"))

publishedAt :: WallRecord -> UTCTime
publishedAt wr = posixSecondsToUTCTime $ fromIntegral $ wr_date wr


data Sized a = Sized {
    m_count :: Int
  , m_items :: a
  } deriving (Show, Functor)

instance FromJSON a => FromJSON (Sized a) where
  parseJSON = Aeson.withObject "Result" (\o ->
    Sized <$> o .: "count" <*> o .: "items")

instance Monoid a => Monoid (Sized a) where
  mempty = Sized 0 mempty
  mappend (Sized x a) (Sized y b) = Sized (x+y) (a<>b)

data Deact = Banned | Deleted | OtherDeact Text
  deriving(Show,Eq,Ord,Data,Typeable)

instance FromJSON Deact where
  parseJSON = Aeson.withText "Deact" $ \x ->
    return $ case x of
              "deleted" -> Deleted
              "banned" -> Banned
              x -> OtherDeact x

data GroupType = Group | Event | Public
  deriving(Show,Eq,Ord,Data,Typeable)

instance FromJSON GroupType where
  parseJSON = Aeson.withText "GroupType" $ \x ->
    return $ case x of
              "group" -> Group
              "page" -> Public
              "event" -> Event

data GroupIsClosed = GroupOpen | GroupClosed | GroupPrivate
  deriving(Show,Eq,Ord,Enum,Data,Typeable)

data GroupId = GroupId { gid_id :: Integer }
  deriving(Show,Eq,Data,Typeable)

data GroupRecord = GroupRecord {
    gr_gid :: GroupId
  , gr_name :: Text
  , gr_screen_name :: Text
  , gr_is_closed :: GroupIsClosed
  , gr_deact :: Maybe Deact
  , gr_is_admin :: Int
  , gr_admin_level :: Maybe Int
  , gr_is_member :: Bool
  , gr_member_status :: Maybe Int
  , gr_invited_by :: Maybe Int
  , gr_type :: GroupType
  , gr_has_photo :: Bool
  , gr_photo_50 :: String
  , gr_photo_100 :: String
  , gr_photo_200 :: String
  -- arbitrary fields
  , gr_can_post :: Maybe Bool
  , gr_members_count :: Maybe Int
  } deriving (Show, Data, Typeable)

instance FromJSON GroupRecord where
  parseJSON = Aeson.withObject "GroupRecord" $ \o ->
    GroupRecord
      <$> (GroupId <$> (o .: "id"))
      <*> (o .: "name")
      <*> (o .: "screen_name")
      <*> fmap toEnum (o .: "is_closed")
      <*> (o .:? "deactivated")
      <*> (o .:? "is_admin" .!= 0)
      <*> (o .:? "admin_level")
      <*> fmap (==1) (o .:? "is_member" .!= (0::Int))
      <*> (o .:? "member_status")
      <*> (o .:? "invited_by")
      <*> (o .: "type")
      <*> (o .:? "has_photo" .!= False)
      <*> (o .: "photo_50")
      <*> (o .: "photo_100")
      <*> (o .: "photo_200")
      <*> (fmap (==(1::Int)) <$> (o .:? "can_post"))
      <*> (o .:? "members_count")

groupURL :: GroupRecord -> String
groupURL GroupRecord{..} = "https://vk.com/" ++ urlify gr_type ++ (show $ gid_id $ gr_gid) where
  urlify Group = "club"
  urlify Event = "event"
  urlify Public = "page"


data Country = Country {
    co_int :: Integer
  , co_title :: Text
} deriving(Show)

instance FromJSON Country where
  parseJSON = Aeson.withObject "Country" $ \o ->
    Country
      <$> (o .: "id")
      <*> (o .: "title")

data City = City {
    c_city_id :: Integer
  , c_title :: Text
  , c_maybe_area :: Maybe Text
  , c_maybe_region :: Maybe Text
} deriving(Show)

instance FromJSON City where
  parseJSON = Aeson.withObject "City" $ \o ->
    City
      <$> (o .: "id")
      <*> (o .: "title")
      <*> (o .:? "area")
      <*> (o .:? "region")

data Album = Album {
  al_id :: Integer
, al_title :: Text
} deriving(Show)

instance FromJSON Album where
  parseJSON = Aeson.withObject "Album" $ \o ->
    Album
      <$> (o .: "id")
      <*> (o .: "title")

data PhotoUploadServer = PhotoUploadServer {
    pus_upload_url :: Text
  , pus_user_id :: Text
  , pus_album_id :: Text
} deriving(Show)

instance FromJSON PhotoUploadServer where
  parseJSON = Aeson.withObject "PhotoUploadServer" $ \o ->
    PhotoUploadServer
      <$> (o .: "upload_url")
      <*> (o .: "album_id")
      <*> (o .: "user_id")

data OwnerUploadServer = OwnerUploadServer {
    ous_upload_url :: Text
  } deriving(Show, Data, Typeable)

instance FromJSON OwnerUploadServer where
  parseJSON = Aeson.withObject "OwnerUploadServer" $ \o ->
    OwnerUploadServer
      <$>  (o .: "upload_url")

data UploadRecord = UploadRecord {
    upl_server :: Integer
  , upl_photo :: Text
  , upl_hash :: Text
  } deriving(Show, Data, Typeable)

instance FromJSON UploadRecord where
  parseJSON = Aeson.withObject "UploadRecord" $ \o ->
    UploadRecord
      <$>  (o .: "server")
      <*>  (o .: "photo")
      <*>  (o .: "hash")



data PhotoSaveResult = PhotoSaveResult {
    photo_hash :: Text
  , photo_src :: Text
  } deriving(Show, Data, Typeable)

instance FromJSON PhotoSaveResult where
  parseJSON = Aeson.withObject "PhotoSaveResult" $ \o ->
    PhotoSaveResult
      <$>  (o .: "photo_hash")
      <*>  (o .: "photo_src")

