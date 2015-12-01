{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.VKHS.Client where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Catch
import Data.Default.Class

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Network.HTTP.Client ()
import Network.HTTP.Client.Internal (setUri)
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.Internal as Client
import qualified Network.URI as Client

import qualified Network.Shpider.Forms as Shpider

data Error = Error String
  deriving(Show, Ord, Eq)

newtype From = Form Shpider.Form
  deriving(Show)

newtype FilledForm = FilledForm Shpider.Form
  deriving(Show)


{-
 _   _ ____  _
| | | |  _ \| |
| | | | |_) | |
| |_| |  _ <| |___
 \___/|_| \_\_____|
-}

newtype URL_Protocol = URL_Protocol { urlproto :: String }
  deriving(Show)
newtype URL_Query = URL_Query { urlq :: String }
  deriving(Show)
newtype URL_Host = URL_Host { urlh :: String }
  deriving(Show)
newtype URL_Port = URL_Port { urlp :: String }
  deriving(Show)
newtype URL_Path = URL_Path { urlpath :: String }
  deriving(Show)
newtype URL = URL { uri :: Client.URI }
  deriving(Show, Eq)

buildQuery :: [(String,String)] -> URL_Query
buildQuery qis = URL_Query ("?" ++ intercalate "&" (map (\(a,b) -> (esc a) ++ "=" ++ (esc b)) qis)) where
  esc x = Client.escapeURIString Client.isAllowedInURI x

urlCreate :: URL_Protocol -> URL_Host -> Maybe URL_Port -> URL_Path -> URL_Query -> Either Error URL
urlCreate URL_Protocol{..} URL_Host{..} port  URL_Path{..} URL_Query{..} =
  pure $ URL $ Client.URI (urlproto ++ ":") (Just (Client.URIAuth "" urlh (maybe "" urlp port))) urlpath urlq []

{-
  ____            _    _
 / ___|___   ___ | | _(_) ___
| |   / _ \ / _ \| |/ / |/ _ \
| |__| (_) | (_) |   <| |  __/
 \____\___/ \___/|_|\_\_|\___|
-}

data Cookies = Cookies { jar :: Client.CookieJar }
  deriving(Show,Eq)

cookiesCreate :: () -> Cookies
cookiesCreate () = Cookies (Client.createCookieJar [])

{-
 _   _ _____ _____ ____
| | | |_   _|_   _|  _ \
| |_| | | |   | | | |_) |
|  _  | | |   | | |  __/
|_| |_| |_|   |_| |_|
-}

newtype Request = Request { req :: Client.Request }

requestCreate :: URL -> Cookies -> IO (Either Error Request)
requestCreate URL{..} Cookies{..} = do
  now <- liftIO $ getCurrentTime
  case setUri def uri of
    Left exc -> return (Left $ Error (show exc))
    Right r -> do
      (r,_) <- pure $ Client.insertCookiesIntoRequest r jar now
      return (Right $ Request r)


