module Web.VKHS2.Types where

import Data.List
import Data.Char

-- | AccessToken is a authentication data, required by all VK API
-- functions. It is a tuple of access_token, user_id, expires_in fields,
-- returned by login procedure.
--
-- See http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений
-- (in Russian) for more details
newtype AccessToken = AccessToken (String,String,String)
  deriving(Show, Eq, Ord)

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

newtype ClientID = ClientID { cid_string :: String }
  deriving(Show, Eq, Ord)

