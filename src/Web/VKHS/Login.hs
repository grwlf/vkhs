{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.VKHS.Login where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Category
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Cont

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import qualified Text.HTML.TagSoup.Parsec as Tagsoup
import qualified Network.Shpider.Forms as Shpider

import Web.VKHS.Types
import Web.VKHS.Client
import Web.VKHS.Monad
import Web.VKHS.Error

data LoginState = LoginState {
    ls_rights :: [AccessRight]
  -- ^ Access rights to be requested
  , ls_appid :: AppID
  -- ^ Application ID provided by vk.com
  , ls_formdata :: [(String,String)]
  -- ^ Dictionary containig inputID/value map for filling forms
  , ls_options :: Options
  } deriving (Show)

defaultState = LoginState {
    ls_rights = allAccess
  , ls_appid = (AppID "3128877")
  , ls_formdata = []
  , ls_options = defaultOptions
  }

class ToLoginState s where
  toLoginState :: s -> LoginState

getLoginState :: (MonadState s m, ToLoginState s) => m LoginState
getLoginState = toLoginState <$> get

-- | Login robot action
data RobotAction = DoGET URL Cookies | DoPOST
  deriving(Show,Eq)

initialAction :: (MonadIO m, ToLoginState s) => VKT s z m RobotAction
initialAction = do
  LoginState{..} <- getLoginState
  Options{..} <- pure ls_options
  u <- ensure $ pure
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

actionRequest :: (MonadIO m, ToLoginState s) => RobotAction -> VKT s z m Request
actionRequest (DoGET url cookiejar) = do
  LoginState{..} <- getLoginState
  req <- ensure $ pure $ requestCreate url cookiejar
  res <- client $ requestExecute req
  let forms = (Tagsoup.parseTags >>> Shpider.gatherForms) (responseBody res)
  return req
actionRequest (DoPOST) = do
  undefined

