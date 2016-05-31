{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Web.VKHS.API.Base where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Arrow ((***),(&&&))
import Control.Category ((>>>))
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont

import Data.Text(Text)
import qualified Data.Text as Text

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Char8 as BS

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Text.Printf

import Web.VKHS.Types
import Web.VKHS.Client
import Web.VKHS.Monad
import Web.VKHS.Error

import Debug.Trace

data APIState = APIState {
    api_access_token :: String
  } deriving (Show)

defaultState = APIState {
    api_access_token = ""
  }

class ToGenericOptions s => ToAPIState s where
  toAPIState :: s -> APIState
  modifyAPIState :: (APIState -> APIState) -> (s -> s)

-- | Class of monads able to run VK API calls. @m@ - the monad itself, @x@ -
-- type of early error, @s@ - type of state (see alse @ToAPIState@)
class (MonadIO (m (R m x)), MonadClient (m (R m x)) s, ToAPIState s, MonadVK (m (R m x)) (R m x)) =>
  MonadAPI m x s | m -> s

type API m x a = m (R m x) a

-- | Utility function to parse JSON object
parseJSON :: (MonadAPI m x s)
    => ByteString
    -> API m x JSON
parseJSON bs = do
  case Aeson.decode (fromStrict bs) of
    Just js -> return (JSON js)
    Nothing -> raise (JSONParseFailure bs)

-- | Invoke the request. Returns answer as JSON object .
--
-- See documentation:
-- <http://vk.com/developers.php?oid=-1&p=%D0%9E%D0%BF%D0%B8%D1%81%D0%B0%D0%BD%D0%B8%D0%B5_%D0%BC%D0%B5%D1%82%D0%BE%D0%B4%D0%BE%D0%B2_API>
--
-- FIXME: We currentyl use Text.unpack to encode text into strings. Use encodeUtf8
-- instead.
apiJ :: (MonadAPI m x s)
    => String
    -- ^ API method name
    -> [(String, Text)]
    -- ^ API method arguments
    -> API m x JSON
apiJ mname (map (id *** tunpack) -> margs) = do
  GenericOptions{..} <- gets toGenericOptions
  APIState{..} <- gets toAPIState
  let protocol = (case o_use_https of
                    True -> "https"
                    False -> "http")
  url <- ensure $ pure
        (urlCreate
          (URL_Protocol protocol)
          (URL_Host o_api_host)
          (Just (URL_Port (show o_port)))
          (URL_Path ("/method/" ++ mname))
          (buildQuery (("access_token", api_access_token):margs)))

  debug $ "> " ++ (show url)

  req <- ensure (requestCreateGet url (cookiesCreate ()))
  (res, jar') <- requestExecute req
  parseJSON (responseBody res)


-- | Invoke the request, returns answer as a Haskell datatype
-- See also @apiJ@
api :: (Aeson.FromJSON a, MonadAPI m x s)
    => String
    -- ^ API method name
    -> [(String, Text)]
    -- ^ API method arguments
    -> API m x a
api m args = do
  j@JSON{..} <- apiJ m args
  case Aeson.parseEither Aeson.parseJSON js_aeson of
    Right a -> return a
    Left e -> terminate (JSONParseFailure' j e)


-- | String version of @api@
api_S :: (Aeson.FromJSON a, MonadAPI m x s)
    => String -> [(String, String)] -> API m x a
api_S m args = api m (map (id *** tpack) args)
