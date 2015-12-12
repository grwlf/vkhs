{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Web.VKHS.Client where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Cont
import Data.Default.Class
import qualified Data.CaseInsensitive as CI

import System.IO.Unsafe

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Network.HTTP.Client ()
import Network.HTTP.Client.Internal (setUri)
import qualified Network.HTTP.Types as Client
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.Internal as Client
import qualified Network.URI as Client

import Pipes.Prelude as PP  (foldM)
import Pipes as Pipes (Producer(..), for, runEffect, (>->))
import Pipes.HTTP as Pipes hiding (Request, Response)

import qualified Network.Shpider.Forms as Shpider

data Error = Error String
  deriving(Show, Ord, Eq)

newtype From = Form Shpider.Form
  deriving(Show)

newtype FilledForm = FilledForm Shpider.Form
  deriving(Show)


{-
 __  __                       _
|  \/  | ___  _ __   __ _  __| |
| |\/| |/ _ \| '_ \ / _` |/ _` |
| |  | | (_) | | | | (_| | (_| |
|_|  |_|\___/|_| |_|\__,_|\__,_|
-}

data ClientState = ClientState {
    cl_man :: Client.Manager
  }

class ToClientState s where
  toClientState :: s -> ClientState

class (MonadIO m, MonadState s m, ToClientState s) => MonadClient s m  | m -> s

-- newtype ClientT m a = ClientT { unClient :: StateT ClientState m a }
--   deriving(Functor, Applicative, Monad, MonadState ClientState, MonadIO)

-- runClient :: (MonadIO m) => ClientT m a -> m a
-- runClient l = do
--   cl_man <- liftIO $ newManager defaultManagerSettings
--   evalStateT (unClient l) ClientState{..}

-- liftClient :: (Monad m) => m a -> ClientT m a
-- liftClient = ClientT . lift

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

requestCreate :: URL -> Cookies -> Either Error Request
requestCreate URL{..} Cookies{..} = do
  case setUri def uri of
    Left exc -> Left $ Error (show exc)
    Right r ->
      let
        now = unsafePerformIO getCurrentTime
        (r,_) = Client.insertCookiesIntoRequest r jar now
      in
      return (Request r)

data Response = Response {
    resp :: Client.Response (Pipes.Producer ByteString IO ())
  , resp_body :: ByteString
  }

responseBody :: Response -> String
responseBody Response{..} = BS.unpack $ resp_body

responseCookies :: Response -> Cookies
responseCookies Response{..} = Cookies (responseCookieJar resp)

responseHeaders :: Response -> [(String,String)]
responseHeaders Response{..} =
  map (\(o,h) -> (BS.unpack (CI.original o), BS.unpack h)) $ Client.responseHeaders resp

responseCode :: Response -> Int
responseCode Response{..} = Client.statusCode $ Client.responseStatus resp

responseCodeMessage :: Response -> String
responseCodeMessage Response{..} = BS.unpack $ Client.statusMessage $ Client.responseStatus resp

responseOK :: Response -> Bool
responseOK r = c == 200 where
  c = responseCode r

requestExecute :: (MonadClient s m) => Request -> m Response
requestExecute Request{..} = do
  ClientState{..} <- toClientState <$> get
  liftIO $ withHTTP req cl_man $ \resp -> do
    resp_body <- PP.foldM (\a b -> return $ BS.append a b) (return BS.empty) return (Client.responseBody resp)
    return Response{..}

