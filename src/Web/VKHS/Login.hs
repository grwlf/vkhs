{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.VKHS.Login where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Cont

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Web.VKHS.Types
import Web.VKHS.Client
import Web.VKHS.Monad
import Web.VKHS.Error

-- | Login robot action
data RobotAction = DoGET URL Cookies | DoPOST
  deriving(Show,Eq)

initialAction :: (Monad m) => VKT z m RobotAction
initialAction = do
  VKState{..} <- get
  Options{..} <- pure ls_options
  u <- handle UnexpectedURL
        (urlCreate
          (URL_Protocol "https")
          (URL_Host o_host)
          (Just (URL_Port o_port))
          (URL_Path "/authorize")
          (buildQuery [
              ("client_id", aid_string ls_appid)
            , ("scope", toUrlArg ls_rights)
            , ("redirect_url", "https://oauth.vk.com/blank.html")
            , ("display", "wap")
            , ("response_type", "token")
            ]))
  return (DoGET u (cookiesCreate ()))

actionRequest :: (MonadIO m) => RobotAction -> VKT z m Request
actionRequest (DoGET url cookiejar) = do
  VKState{..} <- get
  r <- handle UnexpectedRequest $ requestCreate url cookiejar
  return r
actionRequest (DoPOST) = do
  undefined

