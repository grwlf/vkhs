{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.VKHS.Client where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Applicative
import Control.Arrow ((&&&),(***))
import Control.Monad
import Control.Monad.State
import Control.Monad.Cont
import Data.Default.Class
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split
import Data.Text(Text)

import Control.Concurrent (threadDelay)

import System.IO as IO
import System.IO.Unsafe as IO
import System.Clock as Clock

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Network.HTTP.Client ()
import Network.HTTP.Client.Internal (setUri)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as Client
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.Internal as Client
import qualified Network.URI as Client
import qualified Network.Shpider.Forms as Shpider

import Pipes.Prelude as PP (foldM)
import qualified Pipes as Pipes (Producer(..), for, runEffect, (>->))
import qualified Pipes.HTTP as Pipes hiding (Request, Response)

import qualified Text.Parsec as Parsec

import Debug.Trace

import Web.VKHS.Types

data Error = ErrorParseURL { euri :: String, emsg :: String }
           | ErrorSetURL { eurl :: URL, emsg :: String }
  deriving(Show, Eq)

{-
 __  __                       _
|  \/  | ___  _ __   __ _  __| |
| |\/| |/ _ \| '_ \ / _` |/ _` |
| |  | | (_) | | | | (_| | (_| |
|_|  |_|\___/|_| |_|\__,_|\__,_|
-}

data ClientState = ClientState {
    cl_man :: Client.Manager
  , cl_last_execute :: TimeSpec
  , cl_minimum_interval_ns :: Integer
  }

defaultState :: GenericOptions -> IO ClientState
defaultState GenericOptions{..} = do
  cl_man <- Client.newManager
              (Client.managerSetProxy (Client.proxyEnvironment Nothing)
                (case o_use_https of
                  True -> tlsManagerSettings
                  False -> Client.defaultManagerSettings))
  cl_last_execute <- pure (TimeSpec 0 0)
  cl_minimum_interval_ns <- pure (round (10^9  / o_max_request_rate_per_sec))
  return ClientState{..}

class ToClientState s where
  toClientState :: s -> ClientState
  modifyClientState :: (ClientState -> ClientState) -> (s -> s)

class (MonadIO m, MonadState s m, ToClientState s) => MonadClient m s | m -> s

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

-- FIXME: Pack Text to ByteStrings, not to String
buildQuery :: [(String,String)] -> URL_Query
buildQuery qis = URL_Query ("?" ++ intercalate "&" (map (\(a,b) -> (esc a) ++ "=" ++ (esc b)) qis)) where
  esc x = Client.escapeURIString Client.isAllowedInURI x

urlCreate :: URL_Protocol -> URL_Host -> Maybe URL_Port -> URL_Path -> URL_Query -> Either Error URL
urlCreate URL_Protocol{..} URL_Host{..} port  URL_Path{..} URL_Query{..} =
  pure $ URL $ Client.URI (urlproto ++ ":") (Just (Client.URIAuth "" urlh (maybe "" ((":"++).urlp) port))) urlpath urlq []

urlFromString :: String -> Either Error URL
urlFromString s =
  case Client.parseURI s of
    Nothing -> Left (ErrorParseURL s "Client.parseURI failed")
    Just u -> Right (URL u)

-- | FIXME: Convert to BS
splitFragments :: String -> String -> String -> [(String,String)]
splitFragments sep eqs =
    filter (\(a, b) -> not (null a))
  . map (f . splitOn eqs)
  . concat
  . map (splitOn sep)
  . lines
  where f []     = ("", "")
        f [x]    = (trim x, "")
        f (x:xs) = (trim x, trim $ intercalate eqs xs)

        trim = rev (dropWhile (`elem` (" \t\n\r" :: String)))
          where rev f = reverse . f . reverse . f

-- | FIXME: Convert to BS
urlFragments :: URL -> [(String,String)]
urlFragments URL{..} = splitFragments "&" "=" $  unsharp $ Client.uriFragment uri where
  unsharp ('#':x) = x
  unsharp y = y

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

data Request = Request {
    req :: Client.Request
  , req_jar :: Client.CookieJar
  }

requestCreateGet :: (MonadClient m s) => URL -> Cookies -> m (Either Error Request)
requestCreateGet URL{..} Cookies{..} = do
  case setUri def uri of
    Left exc -> do
      return $ Left $ ErrorSetURL (URL uri) (show exc)
    Right r -> do
      now <- liftIO getCurrentTime
      (r',_) <- pure $ Client.insertCookiesIntoRequest r jar now
      return $ Right $ Request {
          req = r'{
              Client.redirectCount = 0
            , Client.checkStatus = \_ _ _ -> Nothing
            },
          req_jar = jar
      }

requestCreatePost :: (MonadClient m s) => FilledForm -> Cookies -> m (Either Error Request)
requestCreatePost (FilledForm tit Shpider.Form{..}) c = do
  case Client.parseURI (Client.escapeURIString Client.isAllowedInURI action) of
    Nothing -> return (Left (ErrorParseURL action "parseURI failed"))
    Just action_uri -> do
      r <- requestCreateGet (URL action_uri) c
      case r of
        Left err -> do
          return $ Left err
        Right Request{..} -> do
          return $ Right $ Request (Client.urlEncodedBody (map (BS.pack *** BS.pack) $ Map.toList inputs) req) req_jar

data Response = Response {
    resp :: Client.Response (Pipes.Producer ByteString IO ())
  , resp_body :: ByteString
  }

responseBodyS :: Response -> String
responseBodyS Response{..} = BS.unpack $ resp_body

responseBody :: Response -> ByteString
responseBody Response{..} = resp_body

dumpResponseBody :: (MonadClient m s) => FilePath -> Response -> m ()
dumpResponseBody f Response{..} = liftIO $ BS.writeFile f resp_body

responseCookies :: Response -> Cookies
responseCookies Response{..} = Cookies (Client.responseCookieJar resp)

responseHeaders :: Response -> [(String,String)]
responseHeaders Response{..} =
  map (\(o,h) -> (BS.unpack (CI.original o), BS.unpack h)) $ Client.responseHeaders resp

responseCode :: Response -> Int
responseCode Response{..} = Client.statusCode $ Client.responseStatus resp

responseCodeMessage :: Response -> String
responseCodeMessage Response{..} = BS.unpack $ Client.statusMessage $ Client.responseStatus resp

responseRedirect :: Response -> Maybe URL
responseRedirect r =
  case lookup "Location" (responseHeaders r) of
    Just loc -> URL <$> Client.parseURI loc
    Nothing -> Nothing

responseOK :: Response -> Bool
responseOK r = c == 200 where
  c = responseCode r

requestExecute :: (MonadClient m s) => Request -> m (Response, Cookies)
requestExecute Request{..} = do
  jar <- pure req_jar
  ClientState{..} <- toClientState <$> get
  clk <- liftIO $ do
    clk <- Clock.getTime Clock.Realtime
    let interval_ns = toNanoSecs (clk `diffTimeSpec` cl_last_execute)
    when (interval_ns < cl_minimum_interval_ns) $ do
      threadDelay (fromInteger $ (cl_minimum_interval_ns - interval_ns) `div` 1000); -- convert ns to us
    return clk

  modify (modifyClientState (\s -> s{cl_last_execute = clk}))

  liftIO $ do
    Pipes.withHTTP req cl_man $ \resp -> do
      resp_body <- PP.foldM (\a b -> return $ BS.append a b) (return BS.empty) return (Client.responseBody resp)
      now <- getCurrentTime
      let (jar', resp') = Client.updateCookieJar resp req now jar
      return (Response resp resp_body, Cookies jar')

downloadFileWith :: (MonadClient m s) => URL -> (ByteString -> IO ()) -> m ()
downloadFileWith url h = do
  (ClientState{..}) <- toClientState <$> get
  (Right Request{..}) <- requestCreateGet url (cookiesCreate ())
  liftIO $ Pipes.withHTTP req cl_man $ \resp -> do
      PP.foldM (\() a -> h a) (return ()) (const (return ())) (Client.responseBody resp)

