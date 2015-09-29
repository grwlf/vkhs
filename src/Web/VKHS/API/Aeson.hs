{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.VKHS.API.Aeson
  ( api
  , api'
  , Base.envcall
  , APIError(..)
  , module Web.VKHS.API.Types
  ) where

import Control.Applicative
import Control.Monad.Error

import Data.Aeson as A
import Data.Aeson.Types as A
import Data.Aeson.Parser as AG
import Data.ByteString.Lazy as BS
import Data.Data
import Data.Vector as V (head, tail)
import Text.Printf
import Web.VKHS.Types
import Web.VKHS.API.Types
import qualified Web.VKHS.API.Base as Base

parseJSON_obj_error :: String -> A.Value -> A.Parser a
parseJSON_obj_error name o = fail $
  printf "parseJSON: %s expects object, got %s" (show name) (show o)

parseJSON_arr_error :: String -> A.Value -> A.Parser a
parseJSON_arr_error name o = fail $
  printf "parseJSON: %s expects array, got %s" (show name) (show o)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON (A.Object v) = do
    a <- v .: "response"
    x <- A.parseJSON a
    return (Response x)
  parseJSON o = parseJSON_obj_error "Response" o

parseGeneric :: (FromJSON a,Data a) => A.Value -> A.Parser a
parseGeneric val =
  case A.fromJSON val of
    A.Success a -> return a
    A.Error s -> fail $ "parseGeneric fails:" ++ s

instance FromJSON MusicRecord where
  parseJSON = parseGeneric

instance FromJSON WallRecord where
  parseJSON (Object o) =
    WR <$> (o .: "id")
       <*> (o .: "to_id")
       <*> (o .: "from_id")
       <*> (o .: "text")
       <*> (o .: "date")
  parseJSON o = parseJSON_obj_error "WallRecord" o

instance (FromJSON a) => FromJSON (SizedList a) where
  parseJSON (A.Array v) = do
    n <- A.parseJSON (V.head v)
    t <- A.parseJSON (A.Array (V.tail v))
    return (SL n t)
  parseJSON o = parseJSON_arr_error "SizedList" o

instance FromJSON RespError where
  parseJSON (Object o) = do
    (Object e) <- o .: "error"
    ER <$> (e .: "error_code")
       <*> (e .: "error_msg")
  parseJSON o = parseJSON_obj_error "RespError" o

data APIError = APIE_resp RespError | APIE_other String | APIE_badAccessToken
  deriving(Show)

instance Error APIError where
  strMsg x =  APIE_other x

api' :: (A.FromJSON a) => Env CallEnv -> String -> [(String,String)] -> IO (Either APIError a)
api' env mn mp
  | Prelude.null ((access_token . sub) env) = return (Left APIE_badAccessToken)
  | otherwise = runErrorT $ do
    e <- BS.fromStrict <$> ErrorT (either (Left . APIE_other) (Right . id) <$> (Base.api env mn mp))
    case (A.decode e) of
      Just x -> return x
      Nothing -> do
        case (A.decode e) of
          Just x -> throwError (APIE_resp x)
          Nothing -> throwError $ APIE_other $ "AESON: error parsing JSON: " ++ show e

api :: Env CallEnv -> String -> [(String,String)] -> IO (Either APIError A.Value)
api = api'

