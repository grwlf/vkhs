module Web.VKHS.Types where

import Data.List
import Data.Char

import qualified Network.Shpider.Forms as Shpider

-- | AccessToken is a authentication data, required by all VK API
-- functions. It is a tuple of access_token, user_id, expires_in fields,
-- returned by login procedure.
--
-- See http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений
-- (in Russian) for more details
data AccessToken = AccessToken {
    at_access_token :: String
  , at_user_id :: String
  , at_expires_in :: String
  } deriving(Show, Eq, Ord)

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

data GenericOptions = GenericOptions {
    o_host :: String
  , o_port :: Int
  , o_verbose :: Bool
  , o_use_https :: Bool
  , o_max_request_rate_per_sec :: Rational
  -- ^ How many requests per second is allowed
  } deriving(Show)

defaultOptions = GenericOptions {
    o_host = "oauth.vk.com"
  , o_port = 443
  , o_verbose = False
  , o_use_https = True
  , o_max_request_rate_per_sec = 3
  }

-- defaultOptions = Options {
--     o_host = "oauth.vk.com"
--   , o_port = 80
--   , o_verbose = False
--   , o_use_https = False
--   }


data Form = Form {
    form_title :: String
  , form :: Shpider.Form
  } deriving(Show)

data FilledForm = FilledForm {
    fform_title :: String
  , fform :: Shpider.Form
  } deriving(Show)


data Verbosity = Normal | Trace | Debug
  deriving(Enum,Eq,Ord,Show)

data LoginOptions = LoginOptions {
    l_appid :: AppID
  , l_username :: String
  , l_password :: String
  } deriving(Show)

data CallOptions = CallOptions {
    c_accessToken :: String
  , c_parse :: Bool
  , c_method :: String
  , c_args :: String
  } deriving(Show)

data MusicOptions = MusicOptions {
    m_accessToken :: String
  , m_list_music :: Bool
  , m_search_string :: String
  , m_name_format :: String
  , m_output_format :: String
  , m_out_dir :: String
  , m_records_id :: [String]
  , m_skip_existing :: Bool
  } deriving(Show)

data UserOptions = UserOptions {
    u_accessToken :: String
  , u_queryString :: String
  } deriving(Show)

data WallOptions = WallOptions {
    w_accessToken :: String
  , w_woid :: String
  } deriving(Show)

