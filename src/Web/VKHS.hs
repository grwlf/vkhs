{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.VKHS (
    module Web.VKHS
  , module Web.VKHS.Client
  , module Web.VKHS.Types
  , module Web.VKHS.Error
  , module Web.VKHS.Monad
  , module Web.VKHS.API
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.Shpider.Forms as Shpider
import qualified System.Process as Process

import Data.Time
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State (MonadState, execState, evalStateT, StateT(..), get, modify)
import Control.Monad.Cont
import Control.Monad.Reader
import Debug.Trace
import System.IO

import qualified Web.VKHS.Client as Client
import qualified Web.VKHS.Monad as VKHS
import qualified Web.VKHS.Login as Login
import qualified Web.VKHS.API as API

import Web.VKHS.Imports
import Web.VKHS.Error
import Web.VKHS.Types
import Web.VKHS.Client hiding (Response, defaultState)
import Web.VKHS.Monad hiding (catch)
import Web.VKHS.Login (MonadLogin, LoginState(..), ToLoginState(..), printForm, loginRoutine)
import Web.VKHS.API

-- | Main state of the VK monad stack. Consists of lesser states plus a copy of
-- generic options provided by the caller.
data VKState = VKState {
    cs :: ClientState
  , ls :: LoginState
  , as :: APIState
  , go :: GenericOptions
  }

instance ToLoginState VKState where
  toLoginState = ls
  modifyLoginState f = \s -> s { ls = f (ls s) }
instance ToClientState VKState where
  toClientState = cs
  modifyClientState f = \s -> s { cs = f (cs s) }
instance API.ToAPIState VKState where
  toAPIState = as
  modifyAPIState f = \s -> s { as = f (as s) }
instance ToGenericOptions VKState where
  toGenericOptions = go

initialState :: (MonadIO m) => GenericOptions -> m VKState
initialState go = VKState
  <$> liftIO (Client.defaultState go)
  <*> pure (Login.defaultState go)
  <*> pure (API.defaultState)
  <*> pure go

type Guts x m r a = ReaderT (r -> x r r) (ContT r m) a

-- | Main VK monad able to track errors, track full state 'VKState', set
-- early exit by the means of continuation monad. VK encodes a coroutine which
-- has entry points defined by 'Result' datatype.
--
-- See also 'runVK' and 'apiSupervisor`.
--
--    * FIXME Re-write using modern 'Monad.Free'
newtype VK r a = VK { unVK :: Guts VK (StateT VKState (ExceptT VKError IO)) r a }
  deriving(MonadIO, Functor, Applicative, Monad, MonadState VKState, MonadReader (r -> VK r r) , MonadCont)

instance MonadClient (VK r) VKState
instance MonadVK (VK r) r
instance MonadLogin VK r VKState
instance MonadAPI VK r VKState

instance MonadClient (StateT VKState (ExceptT VKError IO)) VKState

-- | Run the VK coroutine till next return. Consider using 'runVK' for full
-- spinup.
stepVK :: VK r r -> StateT VKState (ExceptT VKError IO) r
stepVK m = runContT (runReaderT (unVK (VKHS.catch m)) undefined) return

printAPIError :: APIError -> Text
printAPIError = \case
  APIInvalidJSON mname json t ->
    "Method \"" <> tpack mname <> "\": invalid json (" <> jsonEncode json <> ")"
  APIUnhandledError mname erec t ->
    "Method \"" <> tpack mname <> "\": server responded with error (" <> tshow erec  <> ")"
  APIUnexpected mname t ->
    "Method \"" <> tpack mname <> "\": unexpected condition (" <> t  <> ")"

printClientError :: ClientError -> Text
printClientError = \case
  ErrorParseURL{..} -> "Invalid HTTP url \"" <> euri <> "\""
  ErrorSetURL{..} -> "Invalid URL \"" <> tshow eurl <> "\""

printLoginError :: LoginError -> Text
printLoginError = \case
  LoginNoAction -> "No obvious login action left"
  LoginClientError e -> "Failed to login due to client error: " <> printClientError e
  LoginInvalidInputs Form{..} inputs ->
    "Invalid values for " <> (tshow $ Set.toList inputs) <> " inputs of form \"" <> tpack form_title <> "\""

data VKJSONError =
    VKJSONDecodeError Text ByteString
  | VKJSONParseError Text JSON
  deriving(Show)

printVKJSONError :: VKJSONError -> Text
printVKJSONError = \case
  VKJSONDecodeError err bsjson ->
    "Failed to decode JSON: " <> err <> ":\n" <> bspack bsjson
  VKJSONParseError err json ->
    "Failed to parse JSON: " <> err <> ":\n" <> jsonEncodePretty json

data VKError =
    VKFormInputError Form Text
  | VKLoginError LoginError
  | VKUploadError FilePath (Either ClientError VKJSONError)
  | VKExecAPIError MethodName (Either ClientError VKJSONError)
  | VKAPIError APIError
  deriving(Show)

printVKError :: VKError -> Text
printVKError = \case
  -- VKInvalidURLError err url ->
  --   "Invalid URL: " <> err <> ":\n" <> tshow url
  -- VKInvalidHRefError err HRef{..} ->
  --   "Invalid HRef: \"" <> href <> "\":\n" <> printClientError err
  VKFormInputError Form{..} input ->
    "No value to fill input \"" <> input <> "\" of form \"" <> tpack form_title <> "\""
  VKLoginError e -> printLoginError e
  VKUploadError fpath e ->
    "Failed to upload file \"" <> tpack fpath <> "\": " <> either printClientError printVKJSONError e
  VKExecAPIError mname e ->
    "Failed execute method \"" <> tpack mname <> "\": " <> either printClientError printVKJSONError e
  VKAPIError e ->
    "API program failed: " <> printAPIError e


lldebug :: (ToGenericOptions s, MonadState s m, MonadIO m) => Text -> m ()
lldebug str = do
  GenericOptions{..} <- gets toGenericOptions
  when o_verbose $ do
    liftIO $ Text.hPutStrLn stderr str

llprompt :: (ToGenericOptions s, MonadState s m, MonadIO m) => Text -> m ()
llprompt str = do
    liftIO $ Text.hPutStrLn stderr str
    liftIO $ Text.hPutStr stderr "> "

llalert :: (ToGenericOptions s, MonadState s m, MonadIO m) => Text -> m ()
llalert str = do
    liftIO $ Text.hPutStrLn stderr str

executeAPI :: MethodName -> [(String,String)] -> StateT VKState (ExceptT VKError IO) JSON
executeAPI mname margs = do
  llalert $ "Executing API: " <> tpack mname

  GenericOptions{..} <- gets toGenericOptions
  APIState{..} <- gets toAPIState

  let throwAPIError x = throwError (VKExecAPIError mname x)

  let protocol = (case o_use_https of
                    True -> "https"
                    False -> "http")
  url <- pure $ urlCreate
      (URL_Protocol protocol)
      (URL_Host o_api_host)
      (Just (URL_Port (show o_port)))
      (URL_Path ("/method/" ++ mname))
      (buildQuery (("access_token", api_access_token):margs))

  lldebug $ "> " <> (tshow url)

  mreq <- requestCreateGet url (cookiesCreate ())
  case mreq of
    Left err -> do
      throwAPIError (Left err)
    Right req -> do
      (res, jar') <- requestExecute req
      bsjson <- pure $ responseBody res
      case decodeJSON bsjson of
        Left err -> do
          throwAPIError (Right $ VKJSONDecodeError err bsjson)
        Right json -> do
          lldebug $ "< " <> jsonEncodePretty json

          case parseJSON json of
            Left err -> do
              return json {- not an error -}

            Right (Response _ (APIErrorRecord{..},_)) -> do
              case er_code of
                NotLoggedIn -> do
                  llalert $ "Attempting to re-login"
                  at <- loginSupervisor (loginRoutine >>= return . LoginOK)
                  modifyAccessToken at
                  executeAPI mname margs

                TooManyRequestsPerSec -> do
                  llalert $ "Too many requests per second, consider changing options"
                  executeAPI mname margs

                _ -> do
                  return json {- Allow application to handle the code -}


uploadFile :: (FromJSON b, MonadError VKError m, MonadClient m s) => HRef -> String -> m b
uploadFile href filepath = do
  mreq <- requestUploadPhoto href filepath
  case mreq of
    Left err ->
      throwError $ VKUploadError filepath (Left err)
    Right req -> do
      (res, _) <- requestExecute req
      bsjson <- pure $ responseBody res
      case decodeJSON bsjson of
        Left err -> do
          throwError $ VKUploadError filepath (Right $ VKJSONDecodeError err bsjson)
        Right json -> do
          case parseJSON json of
            Left err -> do
              throwError $ VKUploadError filepath (Right $ VKJSONParseError err json)
            Right urec -> do
              return urec


loginSupervisor :: (Show a) => VK (L VK a) (L VK a) -> StateT VKState (ExceptT VKError IO) a
loginSupervisor = go where
  go m = do
    GenericOptions{..} <- toGenericOptions <$> get
    res <- stepVK m
    case res of

      LoginOK a -> do
        return a

      LoginAskInput tags form@(Form tit f) i k ->
        let
          generic_filler = do
            llprompt $ "Please, enter the value for input \"" <> tpack i <> "\""
            v <- liftIO $ getLine
            go (k v)

        in do
        lldebug $ "While filling form " <> (printForm "" f)
        case (o_allow_interactive, i) of

          (True,"captcha_key") -> do
            case Shpider.gatherCaptcha tags of
              Just c -> do
                llprompt $ "Please fill the captcha " <> tshow c
                _ <- liftIO $ Process.spawnCommand $ "curl '" <> c <> "' | feh -"
                v <- liftIO $ getLine
                go (k v)
              Nothing ->
                generic_filler
          (True,_) -> generic_filler

          (False,_) -> do
            throwError (VKFormInputError form (tpack i))

      LoginMessage verb text k -> do
        when ((verb < Debug) || o_verbose) $ do
          llalert text
        go (k ())

      LoginFailed e -> do
        throwError $ VKLoginError e


-- | Run VK monad @m@ and handle continuation requests using default
-- algorithm. @apiSupervisor@ would relogin on invalid access token
-- condition, ask for missing form fields (typically - an email/password)
--
-- See also 'runVK'
--
--    * FIXME Store known answers in external DB (in file?) instead of LoginState
--      FIXME dictionary
--    * FIXME Handle capthas (offer running standalone apps)
apiSupervisor :: (Show a) => VK (R VK a) (R VK a) -> StateT VKState (ExceptT VKError IO) a
apiSupervisor = go where
  go m = do
    GenericOptions{..} <- getGenericOptions
    res <- stepVK m
    case res of

      Fine a -> do
        return a

      APIFailed e ->
        throwError (VKAPIError e)

      APIMessage verb text k -> do
        when ((verb < Debug) || o_verbose) $ do
          llalert text
        go (k ())

      ExecuteAPI (mname,margs) k -> do
        json <- executeAPI mname (map (id *** tunpack) margs)
        go (k json)

      UploadFile (href,filepath) k -> do
        urec <- uploadFile href filepath
        go (k urec)

      APILogin k -> do
        at <- loginSupervisor (loginRoutine >>= return . LoginOK)
        go (k at)

-- | Run loginRoutine procedure using 'apiSupervisor'. Return 'AccessToken' on
-- success
runLogin :: GenericOptions -> ExceptT VKError IO AccessToken
runLogin go = do
  s <- initialState go
  evalStateT (loginSupervisor (loginRoutine >>= return . LoginOK)) s

-- | Run the VK monad @m@ using generic options @go@ and 'apiSupervisor'.
-- Perform loginRoutine procedure if needed. This is an mid-layer runner, consider
-- using 'runVK' instead.
runAPI :: Show b => GenericOptions -> VK (R VK b) b -> ExceptT VKError IO b
runAPI go@GenericOptions{..} m = do
  s <- initialState go
  flip evalStateT s $ do

    readInitialAccessToken >>= \case
      Nothing -> do
        lldebug "No initial access token was read"
        return ()
      Just at -> do
        modifyAccessToken at

    apiSupervisor (m >>= return . Fine)

-- | Run the VK monad @m@ using generic options @go@ and 'apiSupervisor'
runVK :: Show a => GenericOptions -> VK (R VK a) a -> IO (Either VKError a)
runVK go = runExceptT . runAPI go

-- | A version of 'runVK' with unit return.
runVK_ :: Show a => GenericOptions -> VK (R VK a) a -> IO ()
runVK_ go = do
  runVK go >=> \case
    Left e -> fail (tunpack $ printVKError e)
    Right _ -> return ()

-- | Read the access token according with respect to user-defined parameters
--
-- See also 'modifyAccessToken'
-- FIXME: move to Utils
readInitialAccessToken :: (MonadIO m, MonadState s m, ToGenericOptions s) => m (Maybe AccessToken)
readInitialAccessToken =
  let
    str2at s = Just (AccessToken s "<unknown>" "<unknown>")

    safeReadFile fn = do
      liftIO $ Web.VKHS.Imports.catch (Just <$> readFile fn) (\(e :: SomeException) -> return Nothing)

  in do
  GenericOptions{..} <- getGenericOptions
  case l_access_token of
   [] -> do
    lldebug "Initial access token is empty"
    case l_access_token_file of
      [] -> do
        lldebug "No access token file specified"
        return Nothing
      fl -> do
        safeReadFile l_access_token_file >>= \case
          Just txt -> do
            case readMaybe txt of
              Just at -> return (Just at)
              Nothing -> return (str2at txt)
          Nothing -> do
            lldebug $ "Unable to read access token from file '" <> tpack l_access_token_file <> "'"
            return Nothing
   _ -> do
    return (str2at l_access_token)

-- | Modify VK access token in the internal state and its external mirror
-- if enabled, if any.
--
-- See also 'readInitialAccessToken'
modifyAccessToken :: (MonadIO m, MonadState s m, ToAPIState s) => AccessToken -> m ()
modifyAccessToken at@AccessToken{..} = do
  lldebug $ "Modifying access token, new value: " <> tshow at
  modify $ modifyAPIState (\as -> as{api_access_token = at_access_token})
  GenericOptions{..} <- getGenericOptions
  case l_access_token_file of
    [] -> return ()
    fl -> do
      lldebug $ "Writing access token to file '" <> tpack l_access_token_file <> "'"
      liftIO $ writeFile l_access_token_file (show at)
  return ()

