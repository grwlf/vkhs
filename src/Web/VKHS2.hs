{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.VKHS2 where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams

import Network.Http.Client

import Web.VKHS2.Types

data LoginState = LoginState {
    ls_rights :: [AccessRight]
  -- ^ Access rights to be requested
  , ls_clientId :: ClientID
  -- ^ Application ID provided by vk.com
  , ls_formdata :: [(String,String)]
  -- ^ Dictionary containig input/value taken from various forms
  } deriving (Show)

newtype LoginT m a = LoginT { unLogin :: StateT LoginState m a }
  deriving(Functor, Applicative, Monad, MonadState LoginState, MonadIO)

login :: (MonadIO m) => LoginT m a
login = do
  c <- openConnection "kernel.operationaldynamics.com" 58080

