{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.VKHS where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Applicative
import Control.Monad
import Control.Monad.State (MonadState, execState, evalStateT, StateT(..))
import Control.Monad.Cont

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Web.VKHS.Error
import Web.VKHS.Types
import Web.VKHS.Login as Login
import Web.VKHS.Client as Client
import Web.VKHS.Monad

{- Test -}

data State = State {
    cs :: ClientState
  , ls :: LoginState
  }

instance ToLoginState State where
  toLoginState = ls
instance ToClientState State where
  toClientState = cs

initialState :: IO State
initialState = State <$> Client.defaultState <*> pure Login.defaultState

type ST a = StateT State IO a

newtype VK r a = VK { unVK :: VKT (StateT State IO) r a }
  deriving(MonadIO, Functor, Applicative, Monad, MonadState State, MonadCont)

instance MonadClient State (VK r)
instance MonadLogin State (VK r)
-- instance MonadVK VK where
--   raiseError = raiseError_vk VK VK

-- test_loop :: (Monad m) => VKT m (Result (VKT m) a) a -> m (ResultDescription a)
test_loop m = do
  res <- runVKT Fine (unVK m)
  case res of
    UnexpectedInt e k -> test_loop (k 0)
    _ -> return (describeResult res)

runVK :: VK (Result VK a) a -> IO (ResultDescription a)
runVK m = do
  s <- initialState
  evalStateT (test_loop m) s

