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

import Web.VKHS.Error
import Web.VKHS.Types
import Web.VKHS.Client hiding(Error)
import qualified Web.VKHS.Client as Client

data VKState = VKState {
    ls_rights :: [AccessRight]
  -- ^ Access rights to be requested
  , ls_appid :: AppID
  -- ^ Application ID provided by vk.com
  , ls_formdata :: [(String,String)]
  -- ^ Dictionary containig inputID/value map for filling forms
  , ls_options :: Options
  } deriving (Show)

defaultState = VKState {
    ls_rights = allAccess
  , ls_appid = (AppID "3128877")
  , ls_formdata = []
  , ls_options = defaultOptions
  }

newtype VKClientT m a = VKClientT { unVKClient :: StateT VKState (ClientT m) a }
  deriving(Functor, Applicative, Monad, MonadState VKState, MonadIO)

newtype VKT_ r m a = VKT_ { unVK :: ContT r (VKClientT m) a }
  deriving(Functor, Applicative, Monad, MonadCont, MonadState VKState, MonadIO)

liftCont :: ((a -> VKClientT m r) -> VKClientT m r) -> VKT_ r m a
liftCont f = VKT_ (ContT f)

data Error = ETimeout | EClient Client.Error
  deriving(Show, Eq, Ord)

type Result m a = Result' VKT_ m a

type VKT x m a = VKT_ (Result m x) m a

runVK :: (Monad m) => VKT a m a -> VKClientT m (Result m a)
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

