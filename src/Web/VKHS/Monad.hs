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
  _ <- err (z k)
  undefined

terminate :: (MonadVK m r) => r -> m a
terminate r = do
  err <- ask
  _ <- err r
  undefined

-- | Request the superviser to log @text@
log_error :: MonadVK (t (R t a)) (Result t a) => Text -> t (R t a) ()
log_error text = raise (LogError text)

-- class MonadVK (t r) r => EnsureVK t r c a | c -> a where
--   ensure :: t r c -> t r a

-- instance (MonadVK (t (R t x)) (R t x)) => EnsureVK t (R t x) (Either Client.Error Request) Request where
--   ensure m  = m >>= \x ->
--     case x of
--       (Right u) -> return u
--       (Left e) -> raise (\k -> UnexpectedRequest e k)

-- instance (MonadVK (t (R t x)) (R t x)) => EnsureVK t (R t x) (Either Client.Error URL) URL where
--   ensure m  = m >>= \x ->
--     case x of
--       (Right u) -> return u
--       (Left e) -> raise (\k -> UnexpectedURL e k)

getGenericOptions :: (MonadState s m, ToGenericOptions s) => m GenericOptions
getGenericOptions = gets toGenericOptions

-- | Read the access token according with respect to user-defined parameters
--
-- See also 'modifyAccessToken'
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
    debug "Initial access token is empty"
    case l_access_token_file of
      [] -> do
        debug "No access token file specified"
        return Nothing
      fl -> do
        safeReadFile l_access_token_file >>= \case
          Just txt -> do
            case readMaybe txt of
              Just at -> return (Just at)
              Nothing -> return (str2at txt)
          Nothing -> do
            debug $ "Unable to read access token from file '" <> tpack l_access_token_file <> "'"
            return Nothing
   _ -> do
    return (str2at l_access_token)


