{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.VKHS (
    module Web.VKHS
  , module Web.VKHS.Client
  , module Web.VKHS.Types
  , module Web.VKHS.Error
  , module Web.VKHS.Monad
  , module Web.VKHS.Login
  , module Web.VKHS.API.Base
  , module Web.VKHS.API.Types
  , module Web.VKHS.API.Simple
  ) where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Data.Text(Text)
import qualified Data.Text as Text
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State (MonadState, execState, evalStateT, StateT(..), get, modify)
import Control.Monad.Cont
import Control.Monad.Reader

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import System.IO

import Web.VKHS.Error
import Web.VKHS.Types
import Web.VKHS.Client hiding (Error, Response)
import qualified Web.VKHS.Client as Client
import Web.VKHS.Monad hiding (catch)
import qualified Web.VKHS.Monad as VKHS
import Web.VKHS.Login (MonadLogin, LoginState(..), ToLoginState(..), printForm, login)
import qualified Web.VKHS.Login as Login
import Web.VKHS.API.Base (MonadAPI, APIState(..), ToAPIState(..), api)
import qualified Web.VKHS.API.Base as API
import Web.VKHS.API.Types
import Web.VKHS.API.Simple

import Debug.Trace

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

initialState :: GenericOptions -> ExceptT String IO State
initialState go = State
  <$> lift (Client.defaultState go)
  <*> pure (Login.defaultState go)
  <*> pure (API.defaultState)
  <*> pure go


-- Intermediate alias
type Guts x m r a = ReaderT (r -> x r r) (ContT r m) a

-- | Main VK monad able to track errors, track full state @State@, set
-- early exit by the means of continuation monad. See @runVK@
newtype VK r a = VK { unVK :: Guts VK (StateT State (ExceptT String IO)) r a }
  deriving(MonadIO, Functor, Applicative, Monad, MonadState State, MonadReader (r -> VK r r) , MonadCont)

instance MonadClient (VK r) State
instance MonadVK (VK r) r
instance MonadLogin (VK r) r State
-- instance MonadAPI (VK r) r State
instance MonadAPI VK r State

-- | Run the VK script, return final state and error status
stepVK :: VK r r -> StateT State (ExceptT String IO) r
stepVK m = runContT (runReaderT (unVK (VKHS.catch m)) undefined) return

defaultSuperviser :: (Show a) => VK (R VK a) (R VK a) -> StateT State (ExceptT String IO) a
defaultSuperviser = go where
  go m = do
    GenericOptions{..} <- toGenericOptions <$> get
    res <- stepVK m
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
            v <- do
              alert $ "Please, enter the correct value for input " ++ i ++ " : "
              liftIO $ getLine
            go (k v)
          False -> do
            alert $ "Unable to query value for " ++ i ++ " since interactive mode is disabled"
            lift $ throwError res_desc
      _ -> do
        alert $ "Unsupervised error: " ++ res_desc
        lift $ throwError res_desc

runLogin go = do
  s <- initialState go
  evalStateT (defaultSuperviser (login >>= return . Fine)) s


runAPI :: Show b => GenericOptions -> VK (R VK b) b -> ExceptT String IO b
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

runVK :: Show a => GenericOptions -> VK (R VK a) a -> IO (Either String a)
runVK go = runExceptT . runAPI go

runVK_ :: Show a => GenericOptions -> VK (R VK a) a -> IO ()
runVK_ go = do
  runVK go >=> \case
    Left e -> fail e
    Right _ -> return ()

