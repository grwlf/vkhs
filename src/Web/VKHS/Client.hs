-- | This module mainly contains HTTP wrappers required to operate VK monad
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.VKHS.Client where

import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Types as Client
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.Internal as Client
import qualified Network.URI as Client
import qualified Network.Shpider.Forms as Shpider
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import qualified Pipes as Pipes (Producer, for, runEffect, (>->))
import qualified Pipes.HTTP as Pipes hiding (Request, Response)
import qualified Text.Parsec as Parsec

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Data.Monoid((<>))
import Control.Applicative
import Control.Arrow ((&&&),(***))
import Control.Monad
import Control.Monad.State
import Control.Monad.Cont
import Data.Default.Class
import Data.Map (Map)
import Data.List.Split
import Data.Text(Text)
import Control.Concurrent (threadDelay)
import System.IO as IO
import System.IO.Unsafe as IO
import System.Clock as Clock
import Data.ByteString.Char8 (ByteString)
import Network.HTTP.Client ()
import Network.HTTP.Client.Internal (setUri)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Pipes.Prelude as PP (foldM)
import Debug.Trace

import Web.VKHS.Types

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
  , cl_verbose :: Bool
  }

defaultClientState :: GenericOptions -> IO ClientState
defaultClientState GenericOptions{..} = do
  cl_man <- Client.newManager
              (Client.managerSetProxy (Client.proxyEnvironment Nothing)
                (case o_use_https of
                  True -> tlsManagerSettings
                  False -> Client.defaultManagerSettings))
  cl_last_execute <- pure (TimeSpec 0 0)
  cl_minimum_interval_ns <- pure (round ((((10::Rational)^(9::Integer)))  / o_max_request_rate_per_sec))
  cl_verbose <- pure (o_verbosity == Debug)
  return ClientState{..}

class ToClientState s where
  toClientState :: s -> ClientState
  modifyClientState :: (ClientState -> ClientState) -> (s -> s)

class (MonadIO m, MonadState s m, ToClientState s) => MonadClient m s | m -> s

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

--    * FIXME Pack Text to ByteStrings, not to String
buildQuery :: [(String,String)] -> URL_Query
buildQuery qis = URL_Query ("?" ++ intercalate "&" (map (\(a,b) -> (esc a) ++ "=" ++ (esc b)) qis)) where
  esc x = Client.escapeURIString (\c -> Client.isAllowedInURI c && (not (Client.isReserved c))) x

urlCreate :: URL_Protocol -> URL_Host -> Maybe URL_Port -> URL_Path -> URL_Query -> URL
urlCreate URL_Protocol{..} URL_Host{..} port  URL_Path{..} URL_Query{..} =
  URL $ Client.URI (urlproto ++ ":") (Just (Client.URIAuth "" urlh (maybe "" ((":"++).urlp) port))) urlpath urlq []

urlFromString :: String -> Either ClientError URL
urlFromString s =
  case Client.parseURI s of
    Nothing -> Left (ErrorParseURL (Text.pack s) "Client.parseURI failed")
    Just u -> Right (URL u)

-- |    * FIXME Convert to ByteString /  Text
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
          where rev g = reverse . g . reverse . g

-- |    * FIXME Convert to ByteString / Text
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

data ClientRequest = ClientRequest {
    req :: Client.Request
  , req_jar :: Client.CookieJar
  }

-- | Create HTTP(S) GET request
requestCreateGet :: (MonadClient m s) => URL -> Cookies -> m (Either ClientError ClientRequest)
requestCreateGet URL{..} Cookies{..} = do
  case setUri Client.defaultRequest uri of
    Left exc -> do
      return $ Left $ ErrorSetURL (URL uri) (show exc)

    Right r -> do
      now <- liftIO getCurrentTime
      (r',_) <- pure $ Client.insertCookiesIntoRequest r jar now
      return $ Right $ ClientRequest {
          req = r'{
              Client.redirectCount = 0
            },
          req_jar = jar
      }

-- | Create HTTP(S) POST request
requestCreatePost :: (MonadClient m s) => FilledForm -> Cookies -> m (Either ClientError ClientRequest)
requestCreatePost (FilledForm tit Shpider.Form{..}) c = do
  case Client.parseURI (Client.escapeURIString Client.isAllowedInURI action) of
    Nothing -> return (Left (ErrorParseURL (Text.pack action) "parseURI failed"))
    Just action_uri -> do
      r <- requestCreateGet (URL action_uri) c
      case r of
        Left err -> do
          return $ Left err
        Right ClientRequest{..} -> do
          return $ Right $ ClientRequest (Client.urlEncodedBody (map (BS.pack *** BS.pack) $ Map.toList inputs) req) req_jar

-- | Upload the bytestring data @bs@ to the server @text_url@
--
--     * FIXME Use 'URL' rather than Text. Think about
--       https://github.com/blamario/network-uri
--
requestUploadPhoto :: (MonadClient m s) => HRef -> String -> m (Either ClientError ClientRequest)
requestUploadPhoto HRef{..} bs = do
  case Client.parseURI (Text.unpack href) of
    Nothing -> return (Left (ErrorParseURL href "parseURI failed"))
    Just uri -> do
      r <- requestCreateGet (URL uri) (cookiesCreate ())
      case r of
        Left err -> do
          return $ Left err
        Right ClientRequest{..} -> do
          req' <- Multipart.formDataBody [Multipart.partFile "photo" bs] req
          return $ Right $ ClientRequest req' req_jar

data ClientResponse = ClientResponse {
    resp :: Client.Response (Pipes.Producer ByteString IO ())
  , resp_body :: ByteString
  }

responseBodyS :: ClientResponse -> String
responseBodyS ClientResponse{..} = BS.unpack $ resp_body

responseBody :: ClientResponse -> ByteString
responseBody ClientResponse{..} = resp_body

dumpResponseBody :: (MonadClient m s) => FilePath -> ClientResponse -> m ()
dumpResponseBody f ClientResponse{..} = liftIO $ BS.writeFile f resp_body

responseCookies :: ClientResponse -> Cookies
responseCookies ClientResponse{..} = Cookies (Client.responseCookieJar resp)

responseHeaders :: ClientResponse -> [(String,String)]
responseHeaders ClientResponse{..} =
  map (\(o,h) -> (BS.unpack (CI.original o), BS.unpack h)) $ Client.responseHeaders resp

responseCode :: ClientResponse -> Int
responseCode ClientResponse{..} = Client.statusCode $ Client.responseStatus resp

responseCodeMessage :: ClientResponse -> String
responseCodeMessage ClientResponse{..} = BS.unpack $ Client.statusMessage $ Client.responseStatus resp

responseRedirect :: ClientResponse -> Maybe URL
responseRedirect r =
  case lookup "Location" (responseHeaders r) of
    Just loc -> URL <$> Client.parseURI loc
    Nothing -> Nothing

responseOK :: ClientResponse -> Bool
responseOK r = c == 200 where
  c = responseCode r

-- | Execute the 'Request' created by 'requestCreatePost' or 'requestCreateGet'
requestExecute :: (MonadClient m s) => ClientRequest -> m (ClientResponse, Cookies)
requestExecute ClientRequest{..} = do
  jar <- pure req_jar
  ClientState{..} <- toClientState <$> get
  clk <- liftIO $ do
    clk <- Clock.getTime Clock.Realtime
    let interval_ns = toNanoSecs (clk `diffTimeSpec` cl_last_execute)
    when (interval_ns < cl_minimum_interval_ns) $ do
      when cl_verbose $ do
        hPutStrLn stderr $  "Delaying execution to match the request threshold limit of "
                         <> show cl_minimum_interval_ns <> " ns"
      threadDelay (fromInteger $ (cl_minimum_interval_ns - interval_ns) `div` 1000); -- convert ns to us
    return clk

  modify (modifyClientState (\s -> s{cl_last_execute = clk}))

  liftIO $ do
    Pipes.withHTTP req cl_man $ \resp -> do
      resp_body <- PP.foldM (\a b -> return $ BS.append a b) (return BS.empty) return (Client.responseBody resp)
      now <- getCurrentTime
      let (jar', _) = Client.updateCookieJar resp req now jar
      return (ClientResponse resp resp_body, Cookies jar')

-- | Download helper
downloadFileWith :: (MonadClient m s) => URL -> (ByteString -> IO ()) -> m ()
downloadFileWith url h = do
  (ClientState{..}) <- toClientState <$> get
  (Right ClientRequest{..}) <- requestCreateGet url (cookiesCreate ())
  liftIO $ Pipes.withHTTP req cl_man $ \resp -> do
      PP.foldM (\() a -> h a) (return ()) (const (return ())) (Client.responseBody resp)
