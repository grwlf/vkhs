{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module Web.VKHS.Monad where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Catch
import Data.Default.Class

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

-- import Network.HTTP.Client hiding (get)
-- import Network.HTTP.Client.Internal (setUri)
-- import Network.HTTP.Client.TLS
-- import Network.URI

import Pipes.Prelude as PP  (foldM)
import Pipes (for, runEffect, (>->))
import Pipes.HTTP hiding (Request)

-- import Web.Cookie
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
  deriving(Functor, Applicative, Monad, MonadCont, MonadState ClientState)

liftCont :: ((a -> ClientT m r) -> ClientT m r) -> VKT_ r m a
liftCont f = VKT_ (ContT f)

data Error x = ETimeout | EClient Client.Error
  deriving(Show, Eq, Ord)

data Result m a =
    Fine a
  | forall x . Unexpected (Error x) (x -> VKT_ (Result m a) m a)

-- type VKT m a = VKT_ (Result m) m a

runVK :: (Monad m) => VKT_ a m a -> ClientT m a
runVK m = runContT (unVK m) return

raiseError :: (Monad m) => Error a -> VKT_ (Result m x) m a
raiseError ex = liftCont (\cont -> do
  return (Unexpected ex (\x -> liftCont (\cont' -> do
    res <- cont x
    case res of
      Fine a -> cont' a
      x -> return x))))

client :: (Monad m) => Either Client.Error a -> VKT_ (Result m x) m a
client (Right a) = return a
client (Left err) = raiseError (EClient err)


data RobotAction = DoGET URL Cookies | DoPOST
  deriving(Show,Eq)

initialAction :: (Monad m) => VKT_ (Result m RobotAction) m RobotAction
initialAction = do
  ClientState{..} <- get
  Options{..} <- pure ls_options
  u <- client (urlCreate (URL_Protocol "https") (URL_Host o_host) (Just (URL_Port o_port)) (URL_Path "/authorize") (buildQuery [ ("client_id", aid_string ls_appid) , ("scope", toUrlArg ls_rights) , ("redirect_url", "https://oauth.vk.com/blank.html") , ("display", "wap") , ("response_type", "token") ]))
  return $ DoGET u (cookiesCreate ())

-- actionRequest :: (MonadIO m) => RobotAction -> VKT m Request
-- actionRequest (DoGET url cookiejar) = do
--   ClientState{..} <- get
--   r <- client $ requestCreate url cookiejar
--   return r
-- actionRequest (DoPOST) = do
--   undefined

