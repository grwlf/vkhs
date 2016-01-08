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

defaultState o = LoginState {
    ls_rights = allAccess
  , ls_appid = (AppID "3128877")
  , ls_formdata = []
  , ls_options = o
  }

class ToLoginState s where
  toLoginState :: s -> LoginState

class (MonadIO m, MonadClient m s, ToLoginState s, MonadVK m r) => MonadLogin m r s | m -> s

-- | Login robot action
data RobotAction = DoGET URL Cookies | DoPOST FilledForm Cookies
  deriving(Show)

printAction :: String -> RobotAction -> String
printAction prefix (DoGET url jar) = prefix ++ " GET " ++ (show url)
printAction prefix (DoPOST FilledForm{..} jar) = printForm prefix fform

type Login m x a = m (R m x) a

initialAction :: (MonadLogin (m (R m x)) (R m x) s) => Login m x RobotAction
initialAction = do
  LoginState{..} <- toLoginState <$> get
  Options{..} <- pure ls_options
  let
    protocol = (case o_use_https of
                  True -> "https"
                  False -> "http")
  u <- ensure $ pure
        (urlCreate
          (URL_Protocol protocol)
          (URL_Host o_host)
          (Just (URL_Port (show o_port)))
          (URL_Path "/authorize")
          (buildQuery [
              ("client_id", aid_string ls_appid)
            , ("scope", toUrlArg ls_rights)
            , ("redirect_url", protocol ++ "://oauth.vk.com/blank.html")
            , ("display", "wap")
            , ("response_type", "token")
            ]))
  return (DoGET u (cookiesCreate ()))

printForm :: String -> Shpider.Form -> String
printForm prefix Shpider.Form{..} =
  let
    telln x = tell (x ++ "\n")
  in
  execWriter $ do
    telln $ prefix ++ "Form #" ++ " (" ++ (show method) ++ ") Action " ++ action
    forM_ (Map.toList inputs) $ \(input,value) -> do
      telln $ prefix ++ "\t" ++ input ++ ":" ++ (if null value then "<empty>" else value)

fillForm :: (MonadLogin (m (R m x)) (R m x) s) => Form -> Login m x FilledForm
fillForm (Form tit f) = do
    LoginState{..} <- toLoginState <$> get
    Options{..} <- pure ls_options
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
              value' <- raise (\k -> UnexpectedFormField (Form tit f) input k)
              return (input, value')
    -- Replace HTTPS with HTTP if not using TLS
    let action' = (if o_use_https == False && isPrefixOf "https" (Shpider.action f) then
                     "http" ++ (fromJust $ stripPrefix "https" (Shpider.action f))
                   else
                     Shpider.action f)
    return $ FilledForm tit f{Shpider.inputs = Map.fromList fis, Shpider.action = action'}

actionRequest :: (MonadLogin (m (R m x)) (R m x) s) => RobotAction -> Login m x (Response, Cookies)
actionRequest a@(DoGET url jar) = do
  liftIO $ putStrLn $ printAction "> " a
  req <- ensure $ requestCreateGet url jar
  (res, jar') <- requestExecute req jar
  return (res, jar')
actionRequest a@(DoPOST form jar) = do
  liftIO $ putStrLn $ printAction "> " a
  req <- ensure $ requestCreatePost form jar
  (res, jar') <- requestExecute req jar
  return (res, jar')

analyzeResponse :: (MonadLogin (m (R m x)) (R m x) s) => (Response, Cookies) -> Login m x (Either RobotAction ())
analyzeResponse (res, jar) = do
  LoginState{..} <- toLoginState <$> get
  let tags = Tagsoup.parseTags (responseBody res)
      title = Shpider.gatherTitle tags
      forms = map (Form title) (Shpider.gatherForms tags)
  liftIO $ writeFile "latest.html" (responseBody res)
  liftIO $ putStrLn $ "< 0 Title: " ++ title
  case forms of
    [] -> do
      terminate LoginActionsExhausted
    (f:[]) -> do
       liftIO $ putStrLn $ printForm "< 0 " $ form f
       ff <- fillForm f
       return $ Left (DoPOST ff jar)
    fs -> do
      forM_ (fs`zip`[0..]) $ \(f,n) -> do
        ff <- fillForm f
        liftIO $ putStrLn $ printForm ("< " ++ (show n) ++ " ") $ fform ff
      terminate LoginActionsExhausted


