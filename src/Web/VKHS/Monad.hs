{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FunctionalDependencies #-}
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

newtype VKClientT s m a = VKClientT { unVKClient :: StateT s (ClientT m) a }
  deriving(Functor, Applicative, Monad, MonadState s, MonadIO)

newtype VKT_ s r m a = VKT_ { unVK :: ContT r (VKClientT s m) a }
  deriving(Functor, Applicative, Monad, MonadCont, MonadState s, MonadIO)

liftCont :: ((a -> VKClientT s m r) -> VKClientT s m r) -> VKT_ s r m a
liftCont f = VKT_ (ContT f)

data Error = ETimeout | EClient Client.Error
  deriving(Show, Eq, Ord)

type Result s m a = Result' (VKT_ s) m a

type VKT s x m a = VKT_ s (Result s m x) m a

runVK :: (Monad m) => VKT s a m a -> VKClientT s m (Result s m a)
runVK m = runContT (unVK m) (return . Fine)

raiseError :: (Monad m) => ((z -> VKT s x m x) -> Result s m x) -> VKT s x m z
raiseError ex = liftCont (\cont -> do
  return (ex (\x -> liftCont (\cont' -> do
    res <- cont x
    case res of
      Fine a -> cont' a
      x -> return x))))

client :: (Monad m) => ClientT m a -> VKT s x m a
client = undefined

class (Monad m) => VK_Error m c t a where
  handle :: (t -> (a -> VKT s x m x) -> Result s m x) -> c -> VKT s x m a

instance (Monad m) => VK_Error m (Either t a) t a where
  handle ctx (Right u) = return u
  handle ctx (Left e) = raiseError (\k -> ctx e k)

instance (MonadIO m) => VK_Error m (IO (Either t a)) t a where
  handle ctx m = liftIO m >>= handle ctx

instance (MonadIO m) => VK_Error m (ClientT m a) t a where
  handle ctx m = undefined

class VK_Ensure c a | c -> a where
  ensure :: (Monad m) => VKT s x m c -> VKT s x m a

instance VK_Ensure (Either Client.Error Request) Request where
  ensure m  = m >>= \x ->
    case x of
      (Right u) -> return u
      (Left e) -> raiseError (\k -> UnexpectedRequest e k)

instance VK_Ensure (Either Client.Error URL) URL where
  ensure m  = m >>= \x ->
    case x of
      (Right u) -> return u
      (Left e) -> raiseError (\k -> UnexpectedURL e k)

