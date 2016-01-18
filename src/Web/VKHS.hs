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

initialState :: LoginOptions -> IO State
initialState lo =
  let go = l_generic lo in
  State <$> Client.defaultState go <*> pure (Login.defaultState lo) <*> pure API.defaultState <*> pure go

newtype VK r a = VK { unVK :: Guts VK (StateT State IO) r a }
  deriving(MonadIO, Functor, Applicative, Monad, MonadState State, MonadReader (r -> VK r r) , MonadCont)

instance MonadClient (VK r) State
instance MonadVK (VK r) r
instance MonadLogin (VK r) r State
instance API.MonadAPI (VK r) r State

type Guts x m r a = ReaderT (r -> x r r) (ContT r m) a

runVK :: VK r r -> StateT State IO r
runVK m = runContT (runReaderT (unVK (catch m)) undefined) return

defaultSuperviser :: (Show a) => VK (R VK a) (R VK a) -> StateT State IO (Maybe a)
defaultSuperviser = go where
  go m = do
    GenericOptions{..} <- toGenericOptions <$> get
    res <- runVK m
    case res of
      Fine a -> return (Just a)
      UnexpectedInt e k -> do
        liftIO (putStrLn "Int!")
        go (k 0)
      UnexpectedFormField (Form tit f) i k -> do
        liftIO $ putStrLn $ "While filling form " ++ (printForm "" f)
        case o_allow_interactive of
          True -> do
            v <- liftIO $ do
              putStrLn $ "Please, enter the correct value for input " ++ i ++ " : "
              getLine
            go (k v)
          False -> do
            liftIO $ putStrLn $ "Unable to query value for " ++ i ++ " since interactive mode is disabled"
            return Nothing
      _ -> do
        liftIO $ putStrLn $ "Unsupervised error: " ++ (describeResult res)
        return Nothing

runLogin lo = initialState lo >>= evalStateT (defaultSuperviser (login >>= return . Fine))


runAPI APIOptions{..} = do
  s <- initialState a_login_options
  flip evalStateT s $ do
    case (null a_access_token) of
      True -> do
        at <- defaultSuperviser (login >>= return . Fine)
        case at of
          Just (AccessToken{..}) -> do
            modify $ modifyAPIState (\as -> as{api_access_token = at_access_token})
            call_api
          Nothing -> do
            return Nothing
      False -> do
        call_api
  where
    call_api = defaultSuperviser ((api a_method (Client.splitFragments "," "=" a_args)) >>= return . Fine)


