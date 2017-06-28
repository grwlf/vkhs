{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
module Web.VKHS.Types where

import Data.List
import Data.Char
import Data.Data
import Data.Typeable

import Data.Text(Text)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import qualified Network.Shpider.Forms as Shpider

import Control.Monad.State (MonadState(..), gets)
import Web.VKHS.Imports
import System.IO (stderr)


-- | AccessToken is a authentication data, required by all VK API
-- functions. It is a tuple of access_token, user_id, expires_in fields,
-- returned by login procedure.
--
-- See http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений
-- (in Russian) for more details
--
-- See also 'modifyAccessToken' and 'readInitialAccessToken'
data AccessToken = AccessToken {
    at_access_token :: String
  , at_user_id :: String
  , at_expires_in :: String
  } deriving(Read, Show, Eq, Ord)

-- | Access rigth to request from VK.
-- See API docs http://vk.com/developers.php?oid=-1&p=Права_доступа_приложений (in
-- Russian) for details
data AccessRight
  = Notify  -- Пользователь разрешил отправлять ему уведомления.
  | Friends -- Доступ к друзьям.
  | Photos  -- Доступ к фотографиям.
  | Audio   -- Доступ к аудиозаписям.
  | Video   -- Доступ к видеозаписям.
  | Docs    -- Доступ к документам.
  | Notes   -- Доступ заметкам пользователя.
  | Pages   -- Доступ к wiki-страницам.
  | Status  -- Доступ к статусу пользователя.
  | Offers  -- Доступ к предложениям (устаревшие методы).
  | Questions   -- Доступ к вопросам (устаревшие методы).
  | Wall    -- Доступ к обычным и расширенным методам работы со стеной.
            -- Внимание, данное право доступа недоступно для сайтов (игнорируется при попытке авторизации).
  | Groups  -- Доступ к группам пользователя.
  | Messages    -- (для Standalone-приложений) Доступ к расширенным методам работы с сообщениями.
  | Notifications   -- Доступ к оповещениям об ответах пользователю.
  | Stats   -- Доступ к статистике групп и приложений пользователя, администратором которых он является.
  | Ads     -- Доступ к расширенным методам работы с рекламным API.
  | Offline -- Доступ к API в любое время со стороннего сервера.
  deriving(Show, Eq, Ord, Enum)

toUrlArg :: [AccessRight] -> String
toUrlArg = intercalate "," . map (map toLower . show)


allAccess :: [AccessRight]
allAccess =
  [
  --   Notify
    Friends
  , Photos
  , Audio
  , Video
  , Docs
  , Notes
  -- , Pages
  , Status
  , Offers
  , Questions
  , Wall
  , Groups
  , Messages
  , Notifications
  , Stats
  -- , Ads
  -- , Offline
  ]

newtype AppID = AppID { aid_string :: String }
  deriving(Show, Eq, Ord)


data JSON = JSON { js_aeson :: Aeson.Value }
  deriving(Show, Data, Typeable)

parseJSON :: (Aeson.FromJSON a) => JSON -> Either String a
parseJSON j = Aeson.parseEither Aeson.parseJSON (js_aeson j)

data Form = Form {
    form_title :: String
  , form :: Shpider.Form
  } deriving(Show)

data FilledForm = FilledForm {
    fform_title :: String
  , fform :: Shpider.Form
  } deriving(Show)


-- | Generic parameters of the VK execution. For accessing from VK runtime, use
-- `getGenericOptions` function
data GenericOptions = GenericOptions {
    o_login_host :: String
  , o_api_host :: String
  , o_port :: Int
  , o_verbose :: Bool
  , o_use_https :: Bool
  , o_max_request_rate_per_sec :: Rational
  -- ^ How many requests per second is allowed
  , o_allow_interactive :: Bool

  , l_appid :: AppID
  , l_username :: String
  -- ^ VK user name, (typically, an email). Empty string means no value is given
  , l_password :: String
  -- ^ VK password. Empty string means no value is given
  -- * FIXME Hide plain-text passwords
  , l_access_token :: String
  -- ^ Initial access token, empty means 'not set'. Has higher precedence than
  -- l_access_token_file
  , l_access_token_file :: FilePath
  -- ^ Filename to store actual access token, should be used to pass its value
  -- between sessions
  } deriving(Show)

defaultOptions = GenericOptions {
    o_login_host = "oauth.vk.com"
  , o_api_host = "api.vk.com"
  , o_port = 443
  , o_verbose = False
  , o_use_https = True
  , o_max_request_rate_per_sec = 2
  , o_allow_interactive = True

  , l_appid  = AppID "3128877"
  , l_username = ""
  , l_password = ""
  , l_access_token = ""
  , l_access_token_file = ".vkhs-access-token"
  }

class ToGenericOptions s where
  toGenericOptions :: s -> GenericOptions

data Verbosity = Normal | Trace | Debug
  deriving(Enum,Eq,Ord,Show)

debug :: (ToGenericOptions s, MonadState s m, MonadIO m) => Text -> m ()
debug str = do
  GenericOptions{..} <- gets toGenericOptions
  when o_verbose $ do
    liftIO $ Text.hPutStrLn stderr str

alert :: (ToGenericOptions s, MonadState s m, MonadIO m) => Text -> m ()
alert str = do
    liftIO $ Text.hPutStrLn stderr str


data MusicOptions = MusicOptions {
    m_list_music :: Bool
  , m_search_string :: String
  , m_name_format :: String
  , m_output_format :: String
  , m_out_dir :: Maybe String
  , m_records_id :: [String]
  , m_skip_existing :: Bool
  } deriving(Show)

data UserOptions = UserOptions {
    u_queryString :: String
  } deriving(Show)

data WallOptions = WallOptions {
    w_woid :: String
  } deriving(Show)

data GroupOptions = GroupOptions {
    g_search_string :: String
  , g_output_format :: String
  } deriving(Show)

type MethodName = String
type MethodArgs = [(String, Text)]

