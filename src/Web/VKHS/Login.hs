{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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

import Data.Text(Text)
import qualified Data.Text as Text

import qualified Text.HTML.TagSoup.Parsec as Tagsoup
import qualified Network.Shpider.Forms as Shpider

import Web.VKHS.Types
import Web.VKHS.Client
import Web.VKHS.Monad
import Web.VKHS.Error

import Debug.Trace
import System.IO

data LoginState = LoginState {
    ls_rights :: [AccessRight]
  -- ^ Access rights to be requested
  , ls_appid :: AppID
  -- ^ Application ID provided by vk.com
  , ls_formdata :: [(String,String)]
  -- ^ Dictionary containig inputID/value map for filling forms
  , ls_input_sets :: [[String]]
  }

defaultState :: GenericOptions -> LoginState
defaultState go@GenericOptions{..} =
  LoginState {
    ls_rights = allAccess
  , ls_appid = l_appid
  , ls_formdata = (if not (null l_username) then [("email", l_username)] else [])
               ++ (if not (null l_password) then [("pass", l_password)] else [])
  , ls_input_sets = []
  }

class (ToGenericOptions s) => ToLoginState s where
  toLoginState :: s -> LoginState
  modifyLoginState :: (LoginState -> LoginState) -> (s -> s)

class (MonadIO m, MonadClient m s, ToLoginState s, MonadVK m r) => MonadLogin m r s | m -> s

-- | Login robot action
data RobotAction = DoGET URL Cookies | DoPOST FilledForm Cookies
  deriving(Show)

printAction :: String -> RobotAction -> Text
printAction prefix (DoGET url jar) = tpack $ prefix ++ " GET " ++ (show url)
printAction prefix (DoPOST FilledForm{..} jar) = printForm prefix fform

type Login m x a = m (R m x) a

initialAction :: (MonadLogin (m (R m x)) (R m x) s) => Login m x RobotAction
initialAction = do
  LoginState{..} <- gets toLoginState
  GenericOptions{..} <- gets toGenericOptions
  let
    protocol = (case o_use_https of
                  True -> "https"
                  False -> "http")
  u <- ensure $ pure
        (urlCreate
          (URL_Protocol protocol)
          (URL_Host o_login_host)
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

printForm :: String -> Shpider.Form -> Text
printForm prefix Shpider.Form{..} =
  let
    telln x = tell (x ++ "\n")
  in
  tpack $
  execWriter $ do
    telln $ prefix ++ "Form #" ++ " (" ++ (show method) ++ ") Action " ++ action
    forM_ (Map.toList inputs) $ \(input,value) -> do
      telln $ prefix ++ "\t" ++ input ++ ":" ++ (if null value then "<empty>" else value)

fillForm :: (MonadLogin (m (R m x)) (R m x) s) => Form -> Login m x FilledForm
fillForm f@(Form{..}) = do
    LoginState{..} <- toLoginState <$> get
    GenericOptions{..} <- gets toGenericOptions
    let empty_inputs = Shpider.emptyInputs form
    case empty_inputs `elem` ls_input_sets of
      False -> do
        modify $ modifyLoginState (\s -> s{ls_input_sets = empty_inputs:ls_input_sets})
      True -> do
        raise (\k -> RepeatedForm f k)
        return ()
    fis <- forM (Map.toList (Shpider.inputs form)) $ \(input,value) -> do
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
              value' <- raise (\k -> UnexpectedFormField f input k)
              return (input, value')
    -- Replace HTTPS with HTTP if not using TLS
    let action' = (if o_use_https == False && isPrefixOf "https" (Shpider.action form) then
                     "http" ++ (fromJust $ stripPrefix "https" (Shpider.action form))
                   else
                     Shpider.action form)
    return $ FilledForm form_title form{Shpider.inputs = Map.fromList fis, Shpider.action = action'}

actionRequest :: (MonadLogin (m (R m x)) (R m x) s) => RobotAction -> Login m x (Response, Cookies)
actionRequest a@(DoGET url jar) = do
  debug (printAction "> " a)
  req <- ensure $ requestCreateGet url jar
  (res, jar') <- requestExecute req
  return (res, jar')
actionRequest a@(DoPOST form jar) = do
  debug (printAction "> " a)
  req <- ensure $ requestCreatePost form jar
  (res, jar') <- requestExecute req
  return (res, jar')

analyzeResponse :: (MonadLogin (m (R m x)) (R m x) s) => (Response, Cookies) -> Login m x (Either RobotAction AccessToken)
analyzeResponse (res, jar) = do
  LoginState{..} <- toLoginState <$> get
  let tags = Tagsoup.parseTags (responseBodyS res)
      title = Shpider.gatherTitle tags
      forms = map (Form title) (Shpider.gatherForms tags)
  dumpResponseBody "latest.html" res
  debug ("< 0 Title: " <> tpack title)

  case (responseRedirect res) of
    Just url -> do
      debug $ "< 0 Fragments: " <> tshow (urlFragments url)
      maybe (return $ Left $ DoGET url jar) (\x -> return $ Right x) $ do
        let frg = (urlFragments url)
        at_access_token <- lookup "access_token" frg
        at_user_id <-  lookup "user_id" frg
        at_expires_in <-  lookup "expires_in" frg
        return AccessToken{..}
    Nothing -> do
      case forms of
        [] -> do
          terminate LoginActionsExhausted
        (f:[]) -> do
           debug $ printForm "< 0 " $ form f
           ff <- fillForm f
           return $ Left (DoPOST ff jar)
        fs -> do
          forM_ (fs`zip`[0..]) $ \(f,n) -> do
            ff <- fillForm f
            debug $ printForm ("< " ++ (show n) ++ " ") $ fform ff
          terminate LoginActionsExhausted

login :: (MonadLogin (m (R m x)) (R m x) s) => Login m x AccessToken
login = initialAction >>= go where
  go a = do
    req <- actionRequest a
    res <- analyzeResponse req
    case res of
      Left a' -> go a'
      Right at -> return at

