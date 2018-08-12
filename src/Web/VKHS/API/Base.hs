{-| Definitions of basic API types such as Response and Error, definitions of
 - various API-caller functions -}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.VKHS.API.Base where

import Data.Time

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Char8 as BS

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson

import Web.VKHS.Imports
import Web.VKHS.Types
import Web.VKHS.Client hiding (Response(..))
import Web.VKHS.Monad
import Web.VKHS.Error

import Debug.Trace

-- | VK Response representation
data Response a = Response {
    resp_json :: JSON
    -- ^ Original JSON representation of the respone, as received from the VK
  , resp_data :: (a,Bool)
    -- ^ Haskell ADT representation of the response
  -- , resp_error :: Bool
    -- ^ Indicator showing whether the parsed string contained 'response' or 'error'
  } deriving (Show, Functor, Data, Typeable)

emptyResponse :: (Monoid a) => Response a
emptyResponse = Response (JSON $ Aeson.object []) (mempty,True)

parseJSON_obj_error :: String -> Aeson.Value -> Aeson.Parser a
parseJSON_obj_error name o = fail $
  printf "parseJSON: %s expects object, got %s" (show name) (show o)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON j = Aeson.withObject "Response" (\o ->
    Response
      <$> pure (JSON j)
      <*> (((,) <$> (o .: "error") <*> pure False) <|> ((,) <$> (o .: "response") <*> pure True))
      ) j


-- | State of the API engine
data APIState = APIState {
    api_access_token :: String
  } deriving (Show)

defaultState :: APIState
defaultState = APIState {
    api_access_token = ""
  }

class ToGenericOptions s => ToAPIState s where
  toAPIState :: s -> APIState
  modifyAPIState :: (APIState -> APIState) -> (s -> s)

-- | Modify VK access token in the internal state and its external mirror
-- if enabled, if any.
--
-- See also 'readInitialAccessToken'
modifyAccessToken :: (MonadIO m, MonadState s m, ToAPIState s) => AccessToken -> m ()
modifyAccessToken at@AccessToken{..} = do
  debug $ "Modifying access token, new value: " <> tshow at
  modify $ modifyAPIState (\as -> as{api_access_token = at_access_token})
  GenericOptions{..} <- getGenericOptions
  case l_access_token_file of
    [] -> return ()
    fl -> do
      debug $ "Writing access token to file '" <> tpack l_access_token_file <> "'"
      liftIO $ writeFile l_access_token_file (show at)
  return ()

-- | Class of monads able to run VK API calls. @m@ - the monad itself, @x@ -
-- type of early error, @s@ - type of state (see alse @ToAPIState@)
class (MonadIO (m (R m x)), MonadClient (m (R m x)) s, ToAPIState s, MonadVK (m (R m x)) (R m x)) =>
  MonadAPI m x s | m -> s


-- | Short alias for the coroutine monad
type API m x a = m (R m x) a

-- | API via callbacks
api :: (MonadAPI m x s) => MethodName -> MethodArgs -> API m x JSON
api mname margs = raise (ExecuteAPI (mname,margs))


api1 :: forall m x a s . (Aeson.FromJSON a, MonadAPI m x s)
    => MethodName -- ^ API method name
    -> MethodArgs -- ^ API method arguments
    -> API m x a
api1 mname margs = do
  json <- api mname margs
  case parseJSON json of
    (Left e1) -> do
      terminate $ APIFailed $ APIInvalidJSON mname json e1
    (Right (Response _ (a,_))) -> do
      return a

api2 :: forall m x a b s . (Aeson.FromJSON a, Aeson.FromJSON b, MonadAPI m x s)
    => MethodName -- ^ API method name
    -> MethodArgs -- ^ API method arguments
    -> API m x (Either a b)
api2 mname margs = do
  json <- api mname margs
  case (parseJSON json, parseJSON json) of
    (Left e1, Left e2) -> do
      terminate $ APIFailed $ APIInvalidJSON mname json (e1 <> ";" <> e2)
    (Right (Response _ (a,_)), _) -> do
      return (Left a)
    (_, Right (Response _ (b,_))) -> do
      return (Right b)

-- | Upload File to server
upload :: (MonadAPI m x s) => HRef -> FilePath -> API m x UploadRecord
upload href filepath = raise (UploadFile (href,filepath))


-- | Login using default credentials
login :: (MonadAPI m x s) => API m x AccessToken
login = raise APILogin
