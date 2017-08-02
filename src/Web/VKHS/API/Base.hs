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

-- | Wrapper for common error codes returned by the VK API
data ErrorCode =
    AccessDenied
  | NotLoggedIn
  | TooManyRequestsPerSec
  | ErrorCode Scientific
  -- ^ Other codes go here
  deriving(Show,Read, Eq, Ord)

instance FromJSON ErrorCode where
  parseJSON = Aeson.withScientific "ErrorCode" $ \n ->
    case n of
      5 -> return NotLoggedIn
      6 -> return TooManyRequestsPerSec
      15 -> return AccessDenied
      x -> return (ErrorCode x)

-- | Top-level error description, returned by VK API
data ErrorRecord = ErrorRecord
  { er_code :: ErrorCode
  , er_msg :: Text
  } deriving(Show)

instance FromJSON ErrorRecord where
  parseJSON = Aeson.withObject "ErrorRecord" $ \o ->
    ErrorRecord
      <$> (o .: "error_code")
      <*> (o .: "error_msg")


-- | State of the API engine
data APIState = APIState {
    api_access_token :: String
  } deriving (Show)

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

-- | Utility function to parse ByteString into JSON object
--
--    * FIXME Don't raise exception, simply return `Left err`
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
--    * FIXME We currentyl use Text.unpack to encode text into strings. Use encodeUtf8
--      FIXME instead.
--    * FIXME Split into request builder and request executer
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
  res <- decodeJSON (responseBody res)

  debug $ "< " <> jsonEncodePretty res

  return res


-- | Invoke the request, return answer as a Haskell datatype. On error fall out
-- to the supervisor (e.g. @VKHS.defaultSuperviser@) without possibility to
-- continue
{-# DEPRECATED api "Consider using safer apiH-family functions" #-}
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
-- supervisor. The supervisor has a chance to change the arguments.
apiRf :: (Aeson.FromJSON b, MonadAPI m x s)
    => MethodName -- ^ API method name
    -> MethodArgs -- ^ API method arguments
    -> (b -> Either String a)
                  -- ^ A filter function, able to change the return value
                  -- inplace. Returing Left will instruct the coroutine to exit
                  -- to the supervisor
    -> API m x a
apiRf m0 args0 flt = go (ReExec m0 args0) where
  go action = do
    j <- do
      case action of
        ReExec m args -> do
          apiJ m args
        ReParse j -> do
          pure j
    case parseJSON j of
      (Right (Response _ (b,ok))) -> do
        case (ok,flt b) of
          (True, Right a) -> do
            return a
          (True, Left e) -> do
            recovery <- raise (CallFailure (m0, args0, j, e))
            go recovery
          (False,_) -> do
            recovery <- raise (CallFailure (m0, args0, j, "Error response"))
            go recovery
      (Left e) -> do
        recovery <- raise (CallFailure (m0, args0, j, e))
        go recovery

-- | Invoke the request, in case of failure, escalate the probelm to the
-- supervisor. The superwiser has a chance to change the arguments
apiR :: (Aeson.FromJSON a, MonadAPI m x s)
    => MethodName -- ^ API method name
    -> MethodArgs -- ^ API method arguments
    -> API m x a
apiR m0 args0 = apiRf m0 args0 Right

-- | Invoke the request, in case of failure, escalate the probelm to the
-- supervisor. The superwiser has a chance to change the arguments
apiHM :: forall m x a s . (Aeson.FromJSON a, MonadAPI m x s)
    => MethodName -- ^ API method name
    -> MethodArgs -- ^ API method arguments
    -> (ErrorRecord -> API m x (Maybe a))
                  -- ^ Error handler, allowing user to correct possible error
                  -- returned by VK. (Just value) continues the execution,
                  -- Nothing exits from the coroutine to the supervisor.
    -> API m x a
apiHM m0 args0 handler = go (ReExec m0 args0) where
  go action = do
    j <- do
      case action of
        ReExec m args -> do
          apiJ m args
        ReParse j -> do
          pure j
    case (parseJSON j, parseJSON j) of
      (Left e1, Left e2) -> do
        recovery <- raise (CallFailure (m0, args0, j, e1 <> ";" <> e2))
        go recovery
      (Left e, Right (Response _ (err, _))) -> do
        ma <- (handler err)
        case ma of
          Just a -> return a
          Nothing -> do
            recovery <- raise (CallFailure (m0, args0, j, e))
            go recovery
      (Right _, Right (Response _ (err,_))) -> do
        ma <- (handler err)
        case ma of
          Just a -> return a
          Nothing -> do
            recovery <- raise (CallFailure (m0, args0, j,
              "Response matches both error and result object"))
            go recovery
      (Right (Response _ (a,_)), _) -> do
        return a

apiH :: forall m x a s . (Aeson.FromJSON a, MonadAPI m x s)
    => MethodName -- ^ API method name
    -> MethodArgs -- ^ API method arguments
    -> (ErrorRecord -> Maybe a)
                  -- ^ Error handler, allowing user to correct possible error
                  -- returned by VK. (Just value) continues the execution,
                  -- Nothing results in exit from the corouting to the
                  -- supervisor.
    -> API m x a
apiH m args handler = apiHM m args (\e -> pure (handler e) :: API m x (Maybe a))

-- | Encode JSON to strict Char8 ByteStirng
jsonEncodeBS :: JSON -> ByteString
jsonEncodeBS JSON{..} = BS.concat $ toChunks $ Aeson.encode js_aeson

-- | Encode JSON to Text
jsonEncode :: JSON -> Text
jsonEncode JSON{..} = Text.decodeUtf8 $ BS.concat $ toChunks $ Aeson.encode js_aeson

-- | Encode JSON to strict Char8 ByteString using pretty-style formatter
jsonEncodePrettyBS :: JSON -> ByteString
jsonEncodePrettyBS JSON{..} = BS.concat $ toChunks $ Aeson.encodePretty js_aeson

-- | Encode JSON to Text using pretty-style formatter
jsonEncodePretty :: JSON -> Text
jsonEncodePretty JSON{..} = Text.decodeUtf8 $ BS.concat $ toChunks $ Aeson.encodePretty js_aeson

