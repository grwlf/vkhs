{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Exception (catch, SomeException)

import Data.Text(Text)
import qualified Data.Text as Text

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy (fromStrict,toChunks)
import qualified Data.ByteString.Char8 as BS

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson

import Text.Printf
import Text.Read (readMaybe)

import Web.VKHS.Types
import Web.VKHS.Client hiding (Response(..))
import Web.VKHS.Monad
import Web.VKHS.Error
import Web.VKHS.API.Types

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

getGenericOptions :: (MonadState s m, ToGenericOptions s) => m GenericOptions
getGenericOptions = gets toGenericOptions

-- | Read the access token according with respect to user-defined parameters
readInitialAccessToken :: (MonadIO m, MonadState s m, ToAPIState s) => m (Maybe AccessToken)
readInitialAccessToken =
  let
    str2at s = Just (AccessToken s "<unknown>" "<unknown>")

    safeReadFile fn = do
      liftIO $ Control.Exception.catch (Just <$> readFile fn) (\(e :: SomeException) -> return Nothing)

  in do
  GenericOptions{..} <- getGenericOptions
  case l_access_token of
   [] -> do
    case l_access_token_file of
      [] -> return Nothing
      fl -> do
        safeReadFile l_access_token_file >>= \case
          Just txt -> do
            case readMaybe txt of
              Just at -> return (Just at)
              Nothing -> return (str2at txt)
          Nothing -> do
            return Nothing
   _ -> do
    return (str2at l_access_token)


-- | Modifies VK access token in the internal state as well as in the external
-- storage, if enabled
modifyAccessToken :: (MonadIO m, MonadState s m, ToAPIState s) => AccessToken -> m ()
modifyAccessToken at@AccessToken{..} = do
  modify $ modifyAPIState (\as -> as{api_access_token = at_access_token})
  GenericOptions{..} <- getGenericOptions
  case l_access_token_file of
    [] -> return ()
    fl -> liftIO $ writeFile l_access_token_file (show at)
  return ()

-- | Class of monads able to run VK API calls. @m@ - the monad itself, @x@ -
-- type of early error, @s@ - type of state (see alse @ToAPIState@)
class (MonadIO (m (R m x)), MonadClient (m (R m x)) s, ToAPIState s, MonadVK (m (R m x)) (R m x)) =>
  MonadAPI m x s | m -> s

type API m x a = m (R m x) a

-- | Utility function to parse JSON object
--
-- FIXME * Don't raise exception, simply return `Left err`
decodeJSON :: (MonadAPI m x s)
    => ByteString
    -> API m x JSON
decodeJSON bs = do
  case Aeson.decode (fromStrict bs) of
    Just js -> return (JSON js)
    Nothing -> raise (JSONParseFailure bs)

-- | Invoke the request. Returns answer as JSON object .
--
-- See the official documentation:
-- <https://vk.com/dev/methods>
-- <https://vk.com/dev/json_schema>
--
-- FIXME * We currentyl use Text.unpack to encode text into strings. Use encodeUtf8
-- FIXME   instead.
-- FIXME * Split into request builder and request executer
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

  debug $ "> " <> (tshow url)

  req <- ensure (requestCreateGet url (cookiesCreate ()))
  (res, jar') <- requestExecute req
  decodeJSON (responseBody res)


-- | Invoke the request, return answer as a Haskell datatype. On error fall out
-- to the supervizer (e.g. @VKHS.defaultSuperviser@) without possibility to
-- continue
api :: (Aeson.FromJSON a, MonadAPI m x s)
    => String
    -- ^ API method name
    -> [(String, Text)]
    -- ^ API method arguments
    -> API m x a
api m args = do
  j <- apiJ m args
  case parseJSON j of
    Right a -> return a
    Left e -> terminate (JSONParseFailure' j e)

-- | Invoke the request, in case of failure, escalate the probelm to the
-- superwiser. The superwiser has a chance to change the arguments
apiR :: (Aeson.FromJSON a, MonadAPI m x s)
    => MethodName -- ^ API method name
    -> MethodArgs -- ^ API method arguments
    -> API m x a
apiR m0 args0 = go (ReExec m0 args0) where
  go action = do
    j <- do
      case action of
        ReExec m args -> do
          apiJ m args
        ReParse j -> do
          pure j
    case parseJSON j of
      (Right a) -> return a
      (Left e) -> do
        recovery <- raise (CallFailure (m0, args0, j, e))
        go recovery

-- | Invoke the request, return answer as a Haskell datatype or @ErrorRecord@
-- object
apiE :: (Aeson.FromJSON a, MonadAPI m x s)
    => String           -- ^ API method name
    -> [(String, Text)] -- ^ API method arguments
    -> API m x (Either (Response ErrorRecord) a)
apiE m args = apiJ m args >>= convert where
  convert j = do
    err <- pure $ parseJSON j
    ans <- pure $ parseJSON j
    case  (ans, err) of
      (Right a, _) -> return (Right a)
      (Left a, Right e) -> return (Left e)
      (Left a, Left e) -> do
        j' <- raise (JSONCovertionFailure
                     (j, "apiE: " <> Text.pack m <> ": expecting either known response or error"))
        convert j'

-- | Invoke the request, returns answer or the default value in case of error
apiD :: (Aeson.FromJSON a, MonadAPI m x s)
    => a
    -> String           -- ^ API method name
    -> [(String, Text)] -- ^ API method arguments
    -> API m x a
apiD def m args =
  apiE m args >>= \case
    Left err -> return def
    Right x -> return x

-- | String version of @api@
-- Deprecated
api_S :: (Aeson.FromJSON a, MonadAPI m x s)
    => String -> [(String, String)] -> API m x a
api_S m args = api m (map (id *** tpack) args)

-- Encode JSON back to string
jsonEncode :: JSON -> ByteString
jsonEncode JSON{..} = BS.concat $ toChunks $ Aeson.encode js_aeson

jsonEncodePretty :: JSON -> ByteString
jsonEncodePretty JSON{..} = BS.concat $ toChunks $ Aeson.encodePretty js_aeson

