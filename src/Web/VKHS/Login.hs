{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Web.VKHS.Login where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Category ((>>>))
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont

import Data.Map (Map)
import qualified Data.Map as Map

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import qualified Text.HTML.TagSoup.Parsec as Tagsoup
import qualified Network.Shpider.Forms as Shpider

import Web.VKHS.Types
import Web.VKHS.Client
import Web.VKHS.Monad
import Web.VKHS.Error

import Debug.Trace

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

class (MonadIO m, MonadClient m s, ToLoginState s, MonadVK m r) => MonadLogin m r s | m -> s

-- | Login robot action
data RobotAction = DoGET URL Cookies | DoPOST Form Cookies
  deriving(Show)

type Login m x a = m (R m x) a

initialAction :: (MonadLogin (m (R m x)) (R m x) s) => Login m x RobotAction
initialAction = do
  LoginState{..} <- toLoginState <$> get
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

showForm :: Shpider.Form -> String
showForm Shpider.Form{..} =
  let
    telln x = tell (x ++ "\n")
  in
  execWriter $ do
    telln $ "Form " ++ (show method)
    forM_ (Map.toList inputs) $ \(input,value) -> do
      telln $ "\t" ++ input ++ ":" ++ (if null value then "<empty>" else value)
    telln $ "Action " ++ action

fillForm :: (MonadLogin (m (R m x)) (R m x) s) => Form -> Login m x FilledForm
fillForm (Form f) = do
    LoginState{..} <- toLoginState <$> get
    fis <- forM (Map.toList (Shpider.inputs f)) $ \(input,value) -> do
      case lookup input ls_formdata of
        Just value' -> do
          -- trace $ "Overwriting default value for " ++ input ++ "( " ++ value ++ ") with " ++ value' $ do
          return (input, value')
        Nothing -> do
          case null value of
            False -> do
              -- trace "Using default value for " ++ input ++ " (" ++ value ++ ")" $ do
              return (input, value)
            True -> do
              value' <- raise (\k -> UnexpectedFormField (Form f) input k)
              return (input, value')
    return $ FilledForm (f{Shpider.inputs = Map.fromList fis})

actionRequest :: (MonadLogin (m (R m x)) (R m x) s) => RobotAction -> Login m x (Response, Cookies)
actionRequest (DoGET url jar) = do
  LoginState{..} <- toLoginState <$> get
  req <- ensure $ pure $ requestCreate url jar
  (res, jar') <- requestExecute req jar
  let forms = map Form $ (Tagsoup.parseTags >>> Shpider.gatherForms) (responseBody res)
  forM_ forms $ \f -> do
    ff <- fillForm f
    liftIO $ putStrLn $ showForm $ fform ff
  return (res,jar')
actionRequest (DoPOST form jar) = do
  undefined

