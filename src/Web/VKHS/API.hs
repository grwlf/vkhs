{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Web.VKHS.API where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Category ((>>>))
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

-- import qualified Data.ByteString.UTF8 as U
-- import qualified Data.ByteString as BS

import Text.Printf

import Web.VKHS.Types
import Web.VKHS.Client
import Web.VKHS.Monad
import Web.VKHS.Error

import Debug.Trace

data APIState = APIState {
  api_access_token :: String
  } deriving (Show)

class ToGenericOptions s => ToAPIState s where
  toAPIState :: s -> APIState
  modifyAPIState :: (APIState -> APIState) -> (s -> s)

class (MonadIO m, MonadClient m s, ToAPIState s, MonadVK m r) => MonadAPI m r s | m -> s

-- | Invoke the request. Return answer (normally, string representation of
-- JSON data). See documentation:
--
-- <http://vk.com/developers.php?oid=-1&p=%D0%9E%D0%BF%D0%B8%D1%81%D0%B0%D0%BD%D0%B8%D0%B5_%D0%BC%D0%B5%D1%82%D0%BE%D0%B4%D0%BE%D0%B2_API>
-- api :: Env CallEnv
--     -- ^ the VKHS environment
--     -> String
--     -- ^ API method name
--     -> [(String, String)]
--     -- ^ API method parameters (name-value pairs)
--     -> IO (Either String BS.ByteString)
-- api e mn mp =
--   let uri = showUri $ (\f -> f $ toUri $ printf "https://api.vk.com/method/%s" mn) $
--               set query $ bw params (("access_token",(access_token . sub) e):mp)
--   in vk_curl_payload e (tell [CURLOPT_URL uri])


type API m x a = m (R m x) a

api :: (MonadAPI (m (R m x)) (R m x) s)
    => String
    -- ^ API method name
    -> [(String, String)]
    -- ^ API method arguments
    -> API m x ()
api mname margs = do
  APIState{..} <- toAPIState <$> get
  GenericOptions{..} <- toGenericOptions <$> get
  let protocol = (case o_use_https of
                    True -> "https"
                    False -> "http")
  url <- ensure $ pure
        (urlCreate
          (URL_Protocol protocol)
          (URL_Host o_api_host)
          (Just (URL_Port (show o_port)))
          (URL_Path ("/" ++ mname))
          (buildQuery (("access_token", api_access_token):margs)))

  req <- ensure $ requestCreateGet url (cookiesCreate ())
  (res, jar') <- requestExecute req
  liftIO $ putStrLn $ responseBody res
  return ()


