{-# LANGUAGE UndecidableInstances #-}
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
import Control.Monad.Reader
import Control.Monad.Cont
import Data.Default.Class
import System.IO

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Web.VKHS.Error
import Web.VKHS.Types
import Web.VKHS.Client hiding(Error)
import qualified Web.VKHS.Client as Client


-- newtype VKT m r a = VKT { unVKT :: StateT (r -> VKT m r r) (ContT r m) a }
--   deriving(Functor, Applicative, Monad, MonadState (r -> VKT m r r), MonadCont, MonadIO)

class (MonadCont m, MonadReader (r -> m r) m) => MonadVK m r

-- instance (Monad m) => MonadVK (VKT m r) r

-- | Store early exit handler in the reader monad, run the computation @m@
catch :: (MonadVK m r) => m r -> m r
catch m = do
  callCC $ \k -> do
    local (const k) m

raise :: (MonadVK m r) => ((a -> m b) -> r) -> m a
raise z = callCC $ \k -> do
  err <- ask
  err (z k)
  undefined

terminate :: (MonadVK m r) => r -> m a
terminate r = do
  err <- ask
  err r
  undefined

class MonadVK (t r) r => EnsureVK t r c a | c -> a where
  ensure :: t r c -> t r a

instance (MonadVK (t (R t x)) (R t x)) => EnsureVK t (R t x) (Either Client.Error Request) Request where
  ensure m  = m >>= \x ->
    case x of
      (Right u) -> return u
      (Left e) -> raise (\k -> UnexpectedRequest e k)

instance (MonadVK (t (R t x)) (R t x)) => EnsureVK t (R t x) (Either Client.Error URL) URL where
  ensure m  = m >>= \x ->
    case x of
      (Right u) -> return u
      (Left e) -> raise (\k -> UnexpectedURL e k)

-- instance EnsureVK (Either Client.Error Request) Request where
--   ensure m  = m >>= \x ->
--     case x of
--       (Right u) -> return u
--       (Left e) -> raiseError (\k -> UnexpectedRequest e k)

-- instance EnsureVK (Either Client.Error URL) URL where
--   ensure m  = m >>= \x ->
--     case x of
--       (Right u) -> return u
--       (Left e) -> raiseError (\k -> UnexpectedURL e k)


debug :: (ToGenericOptions s, MonadState s m, MonadIO m) => String -> m ()
debug str = do
  GenericOptions{..} <- gets toGenericOptions
  when o_verbose $ do
    liftIO $ hPutStrLn stderr str

alert :: (ToGenericOptions s, MonadState s m, MonadIO m) => String -> m ()
alert str = do
    liftIO $ hPutStrLn stderr str

