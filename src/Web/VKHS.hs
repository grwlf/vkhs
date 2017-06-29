{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.VKHS (
    module Web.VKHS
  , module Web.VKHS.Client
  , module Web.VKHS.Types
  , module Web.VKHS.Error
  , module Web.VKHS.Monad
  , module Web.VKHS.Login
  , module Web.VKHS.API
  ) where

import Data.Time
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State (MonadState, execState, evalStateT, StateT(..), get, modify)
import Control.Monad.Cont
import Control.Monad.Reader
import Debug.Trace
import System.IO

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

import Web.VKHS.Imports
import Web.VKHS.Error
import Web.VKHS.Types
import Web.VKHS.Client hiding (Error, Response, defaultState)
import qualified Web.VKHS.Client as Client
import Web.VKHS.Monad hiding (catch)
import qualified Web.VKHS.Monad as VKHS
import Web.VKHS.Login (MonadLogin, LoginState(..), ToLoginState(..), printForm, login)
import qualified Web.VKHS.Login as Login
import Web.VKHS.API
import qualified Web.VKHS.API as API

-- | Main state of the VK monad stack. Consists of lesser states plus a copy of
-- generic options provided by the caller.
data State = State {
    cs :: ClientState
  , ls :: LoginState
  , as :: APIState
  , go :: GenericOptions
  }

instance ToLoginState State where
  toLoginState = ls
  modifyLoginState f = \s -> s { ls = f (ls s) }
instance ToClientState State where
  toClientState = cs
  modifyClientState f = \s -> s { cs = f (cs s) }
instance API.ToAPIState State where
  toAPIState = as
  modifyAPIState f = \s -> s { as = f (as s) }
instance ToGenericOptions State where
  toGenericOptions = go

initialState :: (MonadIO m) => GenericOptions -> m State
initialState go = State
  <$> liftIO (Client.defaultState go)
  <*> pure (Login.defaultState go)
  <*> pure (API.defaultState)
  <*> pure go

type Guts x m r a = ReaderT (r -> x r r) (ContT r m) a

-- | Main VK monad able to track errors, track full state 'State', set
-- early exit by the means of continuation monad. VK encodes a coroutine which
-- has entry points defined by 'Result' datatype.
--
-- See also 'runVK' and 'defaultSupervisor`.
--
-- * FIXME Re-write using modern 'Monad.Free'
newtype VK r a = VK { unVK :: Guts VK (StateT State (ExceptT Text IO)) r a }
  deriving(MonadIO, Functor, Applicative, Monad, MonadState State, MonadReader (r -> VK r r) , MonadCont)

instance MonadClient (VK r) State
instance MonadVK (VK r) r
instance MonadLogin (VK r) r State
instance MonadAPI VK r State

-- | Run the VK coroutine till next return. Consider using 'runVK' for full
-- spinup.
stepVK :: VK r r -> StateT State (ExceptT Text IO) r
stepVK m = runContT (runReaderT (unVK (VKHS.catch m)) undefined) return

-- | Run VK monad @m@ and handle continuation requests using default
-- algorithm. @defaultSupervisor@ would relogin on invalid access token
-- condition, ask for missing form fields (typically - an email/password)
--
-- See also 'runVK'
--
-- * FIXME Store known answers in external DB (in file?) instead of LoginState
--   FIXME dictionary
-- * FIXME Handle capthas (offer running standalone apps)
defaultSupervisor :: (Show a) => VK (R VK a) (R VK a) -> StateT State (ExceptT Text IO) a
defaultSupervisor = go where
  go m = do
    GenericOptions{..} <- toGenericOptions <$> get
    res <- stepVK m
    res_desc <- pure (describeResult res)
    case res of
      Fine a -> do
        return a

      UnexpectedInt e k -> do
        alert "UnexpectedInt (ignoring)"
        go (k 0)

      UnexpectedFormField (Form tit f) i k -> do
        alert $ "While filling form " <> (printForm "" f)
        case o_allow_interactive of
          True -> do
            v <- do
              alert $ "Please, enter the value for input " <> tpack i <> " : "
              liftIO $ getLine
            go (k v)
          False -> do
            alert $ "Unable to query value for " <> tpack i <> " since interactive mode is disabled"
            lift $ throwError res_desc

      LogError text k -> do
        alert text
        go (k ())

      CallFailure (m, args, j, err) k -> do
        alert $    "Error calling API:\n\n\t" <> tshow m <> " " <> tshow args <> "\n"
              <> "\nResponse object:\n\n\t" <> tpack (ppShow j) <> "\n"
              <> "\nParser error was:" <> tshow err <> "\n"

        case parseJSON j of
          Left err -> do
            alert $ "Failed to parse JSON error object, message: " <> tshow err
            lift $ throwError res_desc

          Right (Response _ ErrorRecord{..}) -> do
            case er_code of
              NotLoggedIn -> do
                alert $ "Attempting to re-login"
                at <- defaultSupervisor (login >>= return . Fine)
                modifyAccessToken at
                go (k $ ReExec m args)
              TooManyRequestsPerSec -> do
                alert $ "Too many requests per second, consider changing options"
                go (k $ ReExec m args)
              ErrorCode ec -> do
                alert $  "Unhandled error code " <> tshow ec <> "\n"
                      <> "Consider improving 'defaultSupervisor' or applying \n"
                      <> "custom error filters using `apiH` ,`apiHS` or their \n"
                      <> "high-level wrappers `apiSimpleH` / `apiSimpleHM`"
                lift $ throwError res_desc

      _ -> do
        alert $ "Unsupervised error: " <> res_desc
        lift $ throwError res_desc

-- | Run login procedure using 'defaultSupervisor'. Return 'AccessToken' on
-- success
runLogin :: GenericOptions -> ExceptT Text IO AccessToken
runLogin go = do
  s <- initialState go
  evalStateT (defaultSupervisor (login >>= return . Fine)) s

-- | Run the VK monad @m@ using generic options @go@ and 'defaultSupervisor'.
-- Perform login procedure if needed. This is an mid-layer runner, consider
-- using 'runVK' instead.
runAPI :: Show b => GenericOptions -> VK (R VK b) b -> ExceptT Text IO b
runAPI go@GenericOptions{..} m = do
  s <- initialState go
  flip evalStateT s $ do

    at <- readInitialAccessToken >>= \case
      Nothing ->
        defaultSupervisor (login >>= return . Fine)
      Just at ->
        pure at

    modifyAccessToken at
    defaultSupervisor (m >>= return . Fine)

-- | Run the VK monad @m@ using generic options @go@ and 'defaultSupervisor'
runVK :: Show a => GenericOptions -> VK (R VK a) a -> IO (Either Text a)
runVK go = runExceptT . runAPI go

-- | A version of 'runVK' with unit return.
runVK_ :: Show a => GenericOptions -> VK (R VK a) a -> IO ()
runVK_ go = do
  runVK go >=> \case
    Left e -> fail (tunpack e)
    Right _ -> return ()

