{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.VKHS where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Applicative
import Control.Monad
import Control.Monad.State (MonadState, execState, evalStateT, StateT(..), get, modify)
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Either

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import System.IO

import Web.VKHS.Error
import Web.VKHS.Types
import Web.VKHS.Client as Client
import Web.VKHS.Monad
import Web.VKHS.Login (MonadLogin, LoginState(..), ToLoginState(..), printForm, login)
import qualified Web.VKHS.Login as Login
import Web.VKHS.API (MonadAPI, APIState(..), ToAPIState(..), api)
import qualified Web.VKHS.API as API

import Debug.Trace

{- Test -}

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

initialState :: GenericOptions -> EitherT String IO State
initialState go = State
  <$> lift (Client.defaultState go)
  <*> pure (Login.defaultState go)
  <*> pure (API.defaultState)
  <*> pure go


newtype VK r a = VK { unVK :: Guts VK (StateT State (EitherT String IO)) r a }
  deriving(MonadIO, Functor, Applicative, Monad, MonadState State, MonadReader (r -> VK r r) , MonadCont)

instance MonadClient (VK r) State
instance MonadVK (VK r) r
instance MonadLogin (VK r) r State
instance API.MonadAPI (VK r) r State

type Guts x m r a = ReaderT (r -> x r r) (ContT r m) a

runVK :: VK r r -> StateT State (EitherT String IO) r
runVK m = runContT (runReaderT (unVK (catch m)) undefined) return

defaultSuperviser :: (Show a) => VK (R VK a) (R VK a) -> StateT State (EitherT String IO) a
defaultSuperviser = go where
  go m = do
    GenericOptions{..} <- toGenericOptions <$> get
    res <- runVK m
    res_desc <- pure (describeResult res)
    case res of
      Fine a -> return a
      UnexpectedInt e k -> do
        alert "UnexpectedInt (ignoring)"
        go (k 0)
      UnexpectedFormField (Form tit f) i k -> do
        alert $ "While filling form " ++ (printForm "" f)
        case o_allow_interactive of
          True -> do
            v <- liftIO $ do
              liftIO $ hPutStrLn stderr $ "Please, enter the correct value for input " ++ i ++ " : "
              getLine
            go (k v)
          False -> do
            alert $ "Unable to query value for " ++ i ++ " since interactive mode is disabled"
            lift $ left res_desc
      _ -> do
        alert $ "Unsupervised error: " ++ res_desc
        lift $ left res_desc

runLogin go = do
  s <- initialState go
  evalStateT (defaultSuperviser (login >>= return . Fine)) s


runAPI go@GenericOptions{..} m = do
  s <- initialState go
  flip evalStateT s $ do
  case (null l_access_token) of
    True -> do
      AccessToken{..} <- defaultSuperviser (login >>= return . Fine)
      modify $ modifyAPIState (\as -> as{api_access_token = at_access_token})
    False -> do
      modify $ modifyAPIState (\as -> as{api_access_token = l_access_token})
  defaultSuperviser (m >>= return . Fine)


