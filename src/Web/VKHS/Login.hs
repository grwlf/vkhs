{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
module Web.VKHS.Login where

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Text.HTML.TagSoup.Parsec as Tagsoup
import qualified Network.Shpider.Forms as Shpider
import qualified Data.Set as Set

import Data.List
import Data.Maybe
import Data.Time
import Data.Set(Set)
import Data.Either
import Control.Category ((>>>))
import Control.Applicative
import Data.Map (Map)
import Data.ByteString.Char8 (ByteString)
import Data.Text(Text)
import Debug.Trace
import System.IO
import System.Directory (doesFileExist)

import Web.VKHS.Types
import Web.VKHS.Client (ToClientState(..), ClientState(..), MonadClient,
                        urlCreate, URL_Protocol(..), URL_Host(..), URL_Port(..), URL_Path(..),
                        buildQuery, requestCreateGet, cookiesCreate, responseBody, defaultClientState,
                        ClientRequest(..), ClientResponse(..), Cookies(..), requestCreatePost,
                        responseBodyS, responseRedirect, urlFragments)
import Web.VKHS.Imports

-- | Alias for `LoginRoutine`
type L t a = LoginRoutine t a

data LoginRoutine t a =
    LoginOK a
  | LoginFailed LoginError
  | LoginAskInput [Tagsoup.Tag String] Form String (String -> t (L t a) (L t a))
  | LoginRequestExecute RobotAction ((ClientResponse,Cookies) -> t (L t a) (L t a))
  | LoginMessage Verbosity Text (() -> t (L t a) (L t a))

data LoginState = LoginState {
    ls_rights :: [AccessRight]
  -- ^ Access rights to be requested
  , ls_appid :: AppID
  -- ^ Application ID provided by vk.com
  , ls_formdata :: [(String,String)]
  -- ^ Dictionary containig inputID/value map for filling forms
  , ls_touched_inputs :: Set String -- [[String]]
  -- ^ Input fields that was once set explicitly
  , ls_cookies :: Cookies
  -- ^ Cookies of the session
  }

defaultLoginState :: GenericOptions -> LoginState
defaultLoginState go@GenericOptions{..} =
  LoginState {
    ls_rights = allAccess
  , ls_appid = l_appid
  , ls_formdata = (if not (null l_username) then [("email", l_username)] else [])
               ++ (if not (null l_password) then [("pass", l_password)] else [])
  , ls_touched_inputs = Set.fromList []
  , ls_cookies = Cookies mempty
  }

class (ToGenericOptions s) => ToLoginState s where
  toLoginState :: s -> LoginState
  modifyLoginState :: (LoginState -> LoginState) -> (s -> s)

-- class (MonadIO m, MonadClient m s, ToLoginState s, MonadVK m r) => MonadLogin m r s | m -> s
-- | Class of monads able to run VK API calls. @m@ - the monad itself, @x@ -
-- type of early error, @s@ - type of state (see alse @ToAPIState@)
class (MonadIO (m (L m x)), ToLoginState s, MonadVK (m (L m x)) (L m x) s) =>
  MonadLogin m x s | m -> s

-- | Login robot action
data RobotAction = DoGET URL Cookies | DoPOST FilledForm Cookies
  deriving(Show)

printAction :: String -> RobotAction -> Text
printAction prefix (DoGET url jar) = tpack $ prefix ++ " GET " ++ (show url)
printAction prefix (DoPOST FilledForm{..} jar) = printForm prefix fform

type Login m x a = m (LoginRoutine m x) a


debug :: (MonadLogin m x s) => Text -> Login m x ()
debug text = raiseVK (LoginMessage Debug text)

ensureClient :: (MonadLogin m x s) => Login m x (Either ClientError a) -> Login m x a
ensureClient m = m >>= \case
  Left e -> terminate $ LoginFailed $ LoginClientError e
  Right a -> return a

initialAction :: (MonadLogin m x s) => Login m x RobotAction
initialAction = do
  LoginState{..} <- toLoginState <$> getVKState
  GenericOptions{..} <- toGenericOptions <$> getVKState
  let
    protocol = (case o_use_https of
                  True -> "https"
                  False -> "http")
  u <- pure $
        urlCreate
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
            ])
  cookies <- if null l_cookies_file
             then pure $ Cookies mempty
             else liftIO $ do
               c <- doesFileExist l_cookies_file
               if c then read <$> readFile l_cookies_file
                    else pure $ Cookies mempty
  modifyVKState (modifyLoginState (\s -> s{ls_cookies = cookies}))
  return (DoGET u cookies)

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

fillForm :: (MonadLogin m x s) => [Tagsoup.Tag String] -> Form -> Login m x FilledForm
fillForm tags f@(Form{..}) = do
    LoginState{..} <- toLoginState <$> getVKState
    GenericOptions{..} <- toGenericOptions <$> getVKState
    let empty_inputs = Set.fromList $ Shpider.emptyInputs form
    let cleared_inputs = empty_inputs `Set.intersection` ls_touched_inputs
    case Set.null cleared_inputs  of
      True -> do
        modifyVKState $ modifyLoginState (\s -> s{ls_touched_inputs = empty_inputs`Set.union`ls_touched_inputs})
      False -> do
        terminate (LoginFailed $ LoginInvalidInputs f cleared_inputs)
    fis <-
      forM (Map.toList (Shpider.inputs form)) $ \(input,value) -> do
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
                value' <- raiseVK (LoginAskInput tags f input)
                return (input, value')
    -- Replace HTTPS with HTTP if not using TLS
    let action' = (if o_use_https == False && isPrefixOf "https" (Shpider.action form) then
                     "http" ++ (fromJust $ stripPrefix "https" (Shpider.action form))
                   else
                     Shpider.action form)
    return $ FilledForm form_title form{Shpider.inputs = Map.fromList fis, Shpider.action = action'}

actionRequest :: (MonadLogin m x s) => RobotAction -> Login m x (ClientResponse, Cookies)
actionRequest ra = do
  (res, jar') <- raiseVK (LoginRequestExecute ra)
  modifyVKState (modifyLoginState (\s -> s{ls_cookies = jar'}))
  return (res, jar')

analyzeResponse :: (MonadLogin m x s) => (ClientResponse, Cookies) -> Login m x (Either RobotAction AccessToken)
analyzeResponse (res, jar) = do
  LoginState{..} <- toLoginState <$> getVKState
  let tags = Tagsoup.parseTags (responseBodyS res)
      title = Shpider.gatherTitle tags

  forms <- pure $ map (Form title) (Shpider.gatherForms tags)
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
          terminate $ LoginFailed $ LoginNoAction
        (f:[]) -> do
           debug $ printForm "< 0 " $ form f
           ff <- fillForm tags f
           return $ Left (DoPOST ff jar)
        fs -> do
          forM_ (fs`zip`[(0::Integer)..]) $ \(f,n) -> do
            ff <- fillForm tags f
            debug $ printForm ("< " ++ (show n) ++ " ") $ fform ff
          terminate $ LoginFailed $ LoginNoAction

loginRoutine :: (MonadLogin m x s) => Login m x AccessToken
loginRoutine = (initialAction >>= go) <* saveCookies where
  go a = do
    req <- actionRequest a
    res <- analyzeResponse req
    case res of
      Left a' -> go a'
      Right at -> return at
  saveCookies = do
    cookies_file <- l_cookies_file <$> toGenericOptions <$> getVKState
    cookies <- ls_cookies <$> toLoginState <$> getVKState
    when (not $ null cookies_file) $
      liftIO $ writeFile cookies_file $ show cookies
