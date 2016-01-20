{-# LANGUAGE DeriveDataTypeable #-}

module Web.VKHS.API.Types where

import Data.Typeable
import Data.Data
import Data.Time.Clock
import Data.Time.Clock.POSIX

newtype Response a = Response a
  deriving (Show, Data, Typeable)

data SizedList a = SL Int [a]
  deriving(Show, Data, Typeable)

data MusicRecord = MR
  { mr_id :: Int
  , mr_owner_id :: Int
  , mr_artist :: String
  , mr_title :: String
  , mr_duration :: Int
  , mr_url :: String
  } deriving (Show, Data, Typeable)


data UserRecord = UR
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

data WallRecord = WR
  { wr_id :: Int
  , wr_to_id :: Int
  , wr_from_id :: Int
  , wr_wtext :: String
  , wr_wdate :: Int
  } deriving (Show)

publishedAt :: WallRecord -> UTCTime
publishedAt wr = posixSecondsToUTCTime $ fromIntegral $ wdate wr

data RespError = ER
  { error_code :: Int
  , error_msg :: String
  } deriving (Show)

