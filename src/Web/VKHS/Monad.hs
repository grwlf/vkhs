{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Web.VKHS.Monad where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Cont
import Data.Default.Class

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Pipes.Prelude as PP  (foldM)
import Pipes (for, runEffect, (>->))
import Pipes.HTTP hiding (Request)

import Web.VKHS.Types
import Web.VKHS.Client hiding(Error)
import qualified Web.VKHS.Client as Client


data ClientState = ClientState {
    ls_rights :: [AccessRight]
  -- ^ Access rights to be requested
  , ls_appid :: AppID
  -- ^ Application ID provided by vk.com
  , ls_formdata :: [(String,String)]
  -- ^ Dictionary containig inputID/value map for filling forms
  , ls_options :: Options
  } deriving (Show)

defaultState = ClientState {
    ls_rights = allAccess
  , ls_appid = (AppID "3128877")
  , ls_formdata = []
  , ls_options = defaultOptions
  }

newtype ClientT m a = ClientT { unClient :: StateT ClientState m a }
  deriving(Functor, Applicative, Monad, MonadState ClientState, MonadIO)

runClient l = runStateT (unClient l) defaultState

liftLogin :: (Monad m) => m a -> ClientT m a
liftLogin = ClientT . lift


newtype VKT_ r m a = VKT_ { unVK :: ContT r (ClientT m) a }
  deriving(Functor, Applicative, Monad, MonadCont, MonadState ClientState, MonadIO)

liftCont :: ((a -> ClientT m r) -> ClientT m r) -> VKT_ r m a
liftCont f = VKT_ (ContT f)

data Error = ETimeout | EClient Client.Error
  deriving(Show, Eq, Ord)

data Result m a =
    Fine a
  | UnexpectedInt Error (Int -> VKT a m a)
  | UnexpectedBool Error (Bool -> VKT a m a)
  | UnexpectedURL Client.Error (URL -> VKT a m a)
  | UnexpectedRequest Client.Error (Request -> VKT a m a)

data ResultDescription a =
    DescFine a
  | DescVKError Error
  | DescClientError Client.Error
  deriving(Show)

describeResult :: Result m a -> ResultDescription a
describeResult (Fine a) = DescFine a
describeResult (UnexpectedInt e k) = DescVKError e
describeResult (UnexpectedBool e k) = DescVKError e
describeResult (UnexpectedURL e k) = DescClientError e
describeResult (UnexpectedRequest e k) = DescClientError e

type VKT x m a = VKT_ (Result m x) m a

runVK :: (Monad m) => VKT a m a -> ClientT m (Result m a)
runVK m = runContT (unVK m) (return . Fine)

raiseError :: (Monad m) => ((z -> VKT x m x) -> Result m x) -> VKT x m z
raiseError ex = liftCont (\cont -> do
  return (ex (\x -> liftCont (\cont' -> do
    res <- cont x
    case res of
      Fine a -> cont' a
      x -> return x))))


class (Monad m) => VK_Error m c t a where
  handle :: (t -> (a -> VKT x m x) -> Result m x) -> c -> VKT x m a

instance (Monad m) => VK_Error m (Either t a) t a where
  handle ctx (Right u) = return u
  handle ctx (Left e) = raiseError (\k -> ctx e k)

instance (MonadIO m) => VK_Error m (IO (Either t a)) t a where
  handle ctx m = liftIO m >>= handle ctx

data RobotAction = DoGET URL Cookies | DoPOST
  deriving(Show,Eq)

initialAction :: (Monad m) => VKT z m RobotAction
initialAction = do
  ClientState{..} <- get
  Options{..} <- pure ls_options
  u <- handle UnexpectedURL
        (urlCreate
          (URL_Protocol "https")
          (URL_Host o_host)
          (Just (URL_Port o_port))
          (URL_Path "/authorize")
          (buildQuery [
              ("client_id", aid_string ls_appid)
            , ("scope", toUrlArg ls_rights)
            , ("redirect_url", "https://oauth.vk.com/blank.html")
            , ("display", "wap")
            , ("response_type", "token")
            ]))
  return (DoGET u (cookiesCreate ()))

actionRequest :: (MonadIO m) => RobotAction -> VKT z m Request
actionRequest (DoGET url cookiejar) = do
  ClientState{..} <- get
  r <- handle UnexpectedRequest $ requestCreate url cookiejar
  return r
actionRequest (DoPOST) = do
  undefined

