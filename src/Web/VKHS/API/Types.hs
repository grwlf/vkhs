{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Web.VKHS.API.Types where

import Data.Typeable
import Data.Data
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Data.Aeson ((.=), (.:), (.:?), (.!=), FromJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Data.Vector as Vector (head, tail)
import Data.Text

import Text.Printf

-- See http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений
-- (in Russian) for more details

data Response a = Response {
    resp_json :: Aeson.Value
  , resp_data :: a
  }
  deriving (Show, Data, Typeable)

parseJSON_obj_error :: String -> Aeson.Value -> Aeson.Parser a
parseJSON_obj_error name o = fail $
  printf "parseJSON: %s expects object, got %s" (show name) (show o)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON j = Aeson.withObject "Response" (\o ->
    Response <$> pure j <*> o .: "response") j

-- Deprecated
data SizedList a = SizedList Int [a]
  deriving(Show, Data, Typeable)

instance (FromJSON a) => FromJSON (SizedList a) where
  parseJSON = Aeson.withArray "SizedList" $ \v -> do
    n <- Aeson.parseJSON (Vector.head v)
    t <- Aeson.parseJSON (Aeson.Array (Vector.tail v))
    return (SizedList n t)

data MusicRecord = MusicRecord
  { mr_id :: Int
  , mr_owner_id :: Int
  , mr_artist :: String
  , mr_title :: String
  , mr_duration :: Int
  , mr_url_str :: String
  } deriving (Show, Data, Typeable)

instance FromJSON MusicRecord where
  parseJSON = Aeson.withObject "MusicRecord" (\o ->
    MusicRecord
      <$> (o .: "aid")
      <*> (o .: "owner_id")
      <*> (o .: "artist")
      <*> (o .: "title")
      <*> (o .: "duration")
      <*> (o .: "url"))


data UserRecord = UserRecord
  { ur_id :: Int
  , ur_first_name :: String
  , ur_last_name :: String
  , ur_photo :: String
  , ur_university :: Maybe Int
  , ur_university_name :: Maybe String
  , ur_faculty :: Maybe Int
  , ur_faculty_name :: Maybe String
  , ur_graduation :: Maybe Int
  } deriving (Show, Data, Typeable)


data WallRecord = WallRecord
  { wr_id :: Int
  , wr_to_id :: Int
  , wr_from_id :: Int
  , wr_wtext :: String
  , wr_wdate :: Int
  } deriving (Show)

publishedAt :: WallRecord -> UTCTime
publishedAt wr = posixSecondsToUTCTime $ fromIntegral $ wr_wdate wr

{-
 - API version 5.44
 -}

data Many a = Many {
    m_count :: Int
  , m_items :: a
  } deriving (Show)

instance FromJSON a => FromJSON (Many a) where
  parseJSON = Aeson.withObject "Result" (\o ->
    Many <$> o .: "count" <*> o .: "items")


data Deact = Banned | Deleted | OtherDeact Text
  deriving(Show,Eq,Ord)

instance FromJSON Deact where
  parseJSON = Aeson.withText "Deact" $ \x ->
    return $ case x of
              "deleted" -> Deleted
              "banned" -> Banned
              x -> OtherDeact x

data GroupType = Group | Event | Public
  deriving(Show,Eq,Ord)

instance FromJSON GroupType where
  parseJSON = Aeson.withText "GroupType" $ \x ->
    return $ case x of
              "group" -> Group
              "page" -> Public
              "event" -> Event


data GroupIsClosed = GroupOpen | GroupClosed | GroupPrivate
  deriving(Show,Eq,Ord,Enum)

data GroupRecord = GroupRecord {
    gr_id :: Int
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
  } deriving (Show)

instance FromJSON GroupRecord where
  parseJSON = Aeson.withObject "GroupRecord" $ \o ->
    GroupRecord
      <$> (o .: "id")
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
groupURL GroupRecord{..} = "https://vk.com/" ++ urlify gr_type ++ (show gr_id) where
  urlify Group = "club"
  urlify Event = "event"
  urlify Public = "page"


