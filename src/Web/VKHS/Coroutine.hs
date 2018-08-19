{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | TODO: Rename to Coroutine.hs
module Web.VKHS.Coroutine where

import qualified Text.HTML.TagSoup.Parsec as Tagsoup
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.Shpider.Forms as Shpider
import qualified System.Process as Process

import qualified Web.VKHS.Client as Client
import qualified Web.VKHS.Login as Login
import qualified Web.VKHS.API as API

import Web.VKHS.Types
import Web.VKHS.Imports
import Web.VKHS.Client (ToClientState(..), ClientState(..), MonadClient,
                        urlCreate, URL_Protocol(..), URL_Host(..), URL_Port(..), URL_Path(..),
                        buildQuery, requestCreateGet, cookiesCreate, requestExecute, requestUploadPhoto,
                        responseBody, requestExecute, defaultClientState, dumpResponseBody, requestCreatePost)
import Web.VKHS.Login (MonadLogin, LoginState(..), ToLoginState(..), printForm, loginRoutine, LoginRoutine(..), L,
                       defaultLoginState, RobotAction(..), printAction, ensureClient)
import Web.VKHS.API (MonadAPI, APIState(..), ToAPIState(..), APIResponse(..), APIRoutine(..), R, defaultAPIState)

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

-- | Initial State constructor
initialState :: (MonadIO m) => GenericOptions -> m VKState
initialState go = VKState
  <$> liftIO (defaultClientState go)
  <*> pure (defaultLoginState go)
  <*> pure (defaultAPIState)
  <*> pure go

type Guts x m r a = ReaderT (r -> x r r) (ContT r m) a

-- | Main VK monad transformer able to raise errors, track 'VKState', set
-- coroutine-style exit with continuation by the means of continuation monad.
-- Technicaly, this monad encodes a coroutine with entry points defined by
-- `APIRoutine` datatype.
--
-- See also `runVK` and `apiSupervisor`
--
-- FIXME * Re-write using modern `Monad.Free`
newtype VKT m r a = VKT { unVKT :: Guts (VKT m) (StateT VKState (ExceptT VKError m)) r a }
  deriving(MonadIO, Functor, Applicative, Monad, MonadReader (r -> VKT m r r), MonadCont)



-- | Alias for IO-based monad stack
type VK r a  = VKT IO r a

instance (MonadIO m) => MonadVK (VKT m r) r VKState where
  getVKState = VKT $ get
  putVKState = VKT . put

-- instance (MonadIO m) => MonadClient (VKT m r) VKState
instance (MonadIO m) => MonadLogin (VKT m) r VKState
instance (MonadIO m) => MonadAPI (VKT m) r VKState
instance (MonadIO m) => MonadClient (StateT VKState (ExceptT VKError m)) VKState

instance (MonadState s m) => MonadState s (VKT m r) where
  get = VKT $ lift $ lift $ lift $ lift $ get
  put = VKT . lift . lift . lift . lift . put

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
  | VKLoginRequestError ClientError
  | VKUploadError FilePath (Either ClientError VKJSONError)
  | VKExecAPIError MethodName (Either ClientError VKJSONError)
  | VKAPIError APIError
  deriving(Show)

printVKError :: VKError -> Text
printVKError = \case
  VKFormInputError Form{..} input ->
    "No value to fill input \"" <> input <> "\" of form \"" <> tpack form_title <> "\""
  VKLoginError e -> printLoginError e
  VKUploadError fpath e ->
    "Failed to upload file \"" <> tpack fpath <> "\": " <> either printClientError printVKJSONError e
  VKExecAPIError mname e ->
    "Failed execute method \"" <> tpack mname <> "\": " <> either printClientError printVKJSONError e
  VKAPIError e ->
    "API program failed: " <> printAPIError e
  VKLoginRequestError ce ->
    "Unable to execute login procedure, " <> printClientError ce

llprompt :: (ToGenericOptions s, MonadState s m, MonadIO m) => Text -> m ()
llprompt str = do
    liftIO $ Text.hPutStrLn stderr str
    liftIO $ Text.hPutStr stderr "> "

llmessage :: (ToGenericOptions s, MonadState s m, MonadIO m) => Verbosity -> Text -> m ()
llmessage verb str = do
  GenericOptions{..} <- gets toGenericOptions
  when (verb <= o_verbosity) $ do
    liftIO $ Text.hPutStrLn stderr str

llalert :: (ToGenericOptions s, MonadState s m, MonadIO m) => Text -> m ()
llalert str = llmessage Normal str

lldebug :: (ToGenericOptions s, MonadState s m, MonadIO m) => Text -> m ()
lldebug str = llmessage Debug str

executeAPI :: (MonadIO m) => MethodName -> [(String,String)] -> StateT VKState (ExceptT VKError m) JSON
executeAPI mname margs = do
  lldebug $ "Executing API: " <> tpack mname

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

            Right (APIResponse _ (APIErrorRecord{..},_)) -> do
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

-- | Run the VK coroutine till next return. Consider using `runVK` for full
-- spinup.
stepVK :: (MonadIO m) => VKT m r r -> StateT VKState (ExceptT VKError m) r
stepVK m = runContT (runReaderT (unVKT (catchVK m)) undefined) return


loginSupervisor :: (MonadIO m, Show a) => VKT m (L (VKT m) a) (L (VKT m) a) -> StateT VKState (ExceptT VKError m) a
loginSupervisor = go where
  go m = do
    let throwLoginError x = throwError (VKLoginRequestError x)
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

      LoginRequestExecute ra k -> do
        case ra of
          a@(DoGET url jar) -> do
            lldebug (printAction "> " a)
            mreq <- requestCreateGet url jar
            case mreq of
              Left err -> throwLoginError err
              Right req -> do
                (cres, jar') <- requestExecute req
                dumpResponseBody "latest.html" cres
                go (k (cres, jar'))

          a@(DoPOST form jar) -> do
            lldebug (printAction "> " a)
            mreq <- requestCreatePost form jar
            case mreq of
              Left err -> throwLoginError err
              Right req -> do
                (cres, jar') <- requestExecute req
                dumpResponseBody "latest.html" cres
                go (k (cres, jar'))

      LoginMessage verb text k -> do
        llmessage verb text
        go (k ())

      LoginFailed e -> do
        throwError $ VKLoginError e


-- | Run VK monad @m@ and handle continuation requests using default
-- algorithm. @apiSupervisor@ would relogin on invalid access token
-- condition, ask for missing form fields (typically - an email/password)
--
-- See also `runVK`
--
-- FIXME * Store known answers in external DB (in file?) instead of LoginState
-- FIXME   dictionary
-- FIXME * Handle capthas (offer running standalone apps)
apiSupervisor :: (MonadIO m, Show a) => VKT m (R (VKT m) a) (R (VKT m) a) -> StateT VKState (ExceptT VKError m) a
apiSupervisor = go where
  go m = do
    GenericOptions{..} <- toGenericOptions <$> get
    res <- stepVK m
    case res of

      Fine a -> do
        return a

      APIFailed e ->
        throwError (VKAPIError e)

      APIMessage verb text k -> do
        llmessage verb text
        go (k ())

      ExecuteAPI (mname,margs) k -> do
        json <- executeAPI mname (map (id *** tunpack) margs)
        go (k json)

      UploadFile (href,filepath) k -> do
        urec <- uploadFile href filepath
        go (k urec)

      APILogin k -> do
        at <- loginSupervisor (loginRoutine >>= return . LoginOK)
        modifyAccessToken at
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
runAPI :: (MonadIO m, Show b) => GenericOptions -> VKT m (R (VKT m) b) b -> ExceptT VKError m b
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

-- | Run the VK monad @m@ using generic options @go@ and `apiSupervisor`
runVK :: (MonadIO m, Show a) => GenericOptions -> VKT m (R (VKT m) a) a -> m (Either VKError a)
runVK go = runExceptT . runAPI go

-- | A version of `runVK` with unit return.
runVK_ :: (MonadIO m, Show a) => GenericOptions -> VKT m (R (VKT m) a) a -> m ()
runVK_ go = do
  runVK go >=> \case
    Left e -> fail (tunpack $ printVKError e)
    Right _ -> return ()

-- | Read the access token according with respect to user-defined parameters
--
-- See also `modifyAccessToken`
--
-- FIXME  Move to Utils
readInitialAccessToken :: (MonadIO m, MonadState s m, ToGenericOptions s) => m (Maybe AccessToken)
readInitialAccessToken =
  let
    str2at s = Just (AccessToken s "<unknown>" "<unknown>")

    safeReadFile fn = do
      liftIO $ catch (Just <$> readFile fn) (\(e :: SomeException) -> return Nothing)

  in do
  GenericOptions{..} <- toGenericOptions <$> get
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
-- See also `readInitialAccessToken`
modifyAccessToken :: (MonadIO m, MonadState s m, ToAPIState s) => AccessToken -> m ()
modifyAccessToken at@AccessToken{..} = do
  lldebug $ "Modifying access token, new value: " <> tshow at
  modify $ modifyAPIState (\as -> as{api_access_token = at_access_token})
  GenericOptions{..} <- toGenericOptions <$> get
  case l_access_token_file of
    [] -> return ()
    fl -> do
      lldebug $ "Writing access token to file '" <> tpack l_access_token_file <> "'"
      liftIO $ writeFile l_access_token_file (show at)
  return ()

