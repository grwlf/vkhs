{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.VKHS.API.Types where

import Data.Typeable
import Data.Data
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Data.Aeson ((.=), (.:), FromJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Data.Vector as Vector (head, tail)

import Text.Printf

newtype Response a = Response a
  deriving (Show, Data, Typeable)

parseJSON_obj_error :: String -> Aeson.Value -> Aeson.Parser a
parseJSON_obj_error name o = fail $
  printf "parseJSON: %s expects object, got %s" (show name) (show o)

parseJSON_arr_error :: String -> Aeson.Value -> Aeson.Parser a
parseJSON_arr_error name o = fail $
  printf "parseJSON: %s expects array, got %s" (show name) (show o)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON (Aeson.Object v) = do
    a <- v .: "response"
    x <- Aeson.parseJSON a
    return (Response x)
  parseJSON o = parseJSON_obj_error "Response" o

data SizedList a = SizedList Int [a]
  deriving(Show, Data, Typeable)

instance (FromJSON a) => FromJSON (SizedList a) where
  parseJSON (Aeson.Array v) = do
    n <- Aeson.parseJSON (Vector.head v)
    t <- Aeson.parseJSON (Aeson.Array (Vector.tail v))
    return (SizedList n t)
  parseJSON o = parseJSON_arr_error "SizedList" o

data MusicRecord = MusicRecord
  { mr_id :: Int
  , mr_owner_id :: Int
  , mr_artist :: String
  , mr_title :: String
  , mr_duration :: Int
  , mr_url :: String
  } deriving (Show, Data, Typeable)

instance FromJSON MusicRecord where
  parseJSON (Aeson.Object o) =
    MusicRecord
      <$> (o .: "aid")
      <*> (o .: "owner_id")
      <*> (o .: "artist")
      <*> (o .: "title")
      <*> (o .: "duration")
      <*> (o .: "url")
  parseJSON o = parseJSON_obj_error "MusicRecord" o

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

data RespError = RespError
  { error_code :: Int
  , error_msg :: String
  } deriving (Show)

