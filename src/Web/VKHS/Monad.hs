{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
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

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Web.VKHS.Imports
import Web.VKHS.Error
import Web.VKHS.Types
import Web.VKHS.Client
import qualified Web.VKHS.Client as Client


class (MonadCont m, MonadReader (r -> m r) m) => MonadVK m r

-- | Store early exit handler in the reader monad, run the computation @m@
catch :: (MonadVK m r) => m r -> m r
catch m = do
  callCC $ \k -> do
    local (const k) m

raise :: (MonadVK m r) => ((a -> m b) -> r) -> m a
raise z = callCC $ \k -> do
  err <- ask
  _ <- err (z k)
  undefined

terminate :: (MonadVK m r) => r -> m a
terminate r = do
  err <- ask
  _ <- err r
  undefined

getGenericOptions :: (MonadState s m, ToGenericOptions s) => m GenericOptions
getGenericOptions = gets toGenericOptions


