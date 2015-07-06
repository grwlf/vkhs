{-# LANGUAGE DeriveDataTypeable #-}

module Web.VKHS.API.Types where

import Data.Typeable
import Data.Data
import Data.Time.Clock
import Data.Time.Clock.POSIX

newtype Response a = Response a
  deriving (Show)

data SizedList a = SL Int [a]

data MusicRecord = MR
  { aid :: Int
  , owner_id :: Int
  , artist :: String
  , title :: String
  , duration :: Int
  , url :: String
  } deriving (Show, Data, Typeable)


data UserRecord = UR
  { uid :: Int
  , first_name :: String
  , last_name :: String
  , photo :: String
  , university :: Maybe Int
  , university_name :: Maybe String
  , faculty :: Maybe Int
  , faculty_name :: Maybe String
  , graduation :: Maybe Int
  } deriving (Show, Data, Typeable)

data WallRecord = WR
  { wid :: Int
  , to_id :: Int
  , from_id :: Int
  , wtext :: String
  , wdate :: Int
  } deriving (Show)

publishedAt :: WallRecord -> UTCTime
publishedAt wr = posixSecondsToUTCTime $ fromIntegral $ wdate wr

data RespError = ER
  { error_code :: Int
  , error_msg :: String
  } deriving (Show)

