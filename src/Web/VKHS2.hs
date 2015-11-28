{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.VKHS2 where

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

-- import System.IO.Streams (InputStream, OutputStream, stdout)
-- import qualified System.IO.Streams as Streams
import Network.HTTP.Client hiding (get)
import Network.HTTP.Client.Internal (setUri)
import Network.HTTP.Client.TLS
import Network.URI

-- import URI.ByteString (URI, RelativeRef, parseURI, strictURIParserOptions)
-- import URI.ByteString
-- import qualified URI.ByteString as URI

-- import qualified Blaze.ByteString.Builder           as BB
-- import qualified Blaze.ByteString.Builder.Char.Utf8 as BB

import Pipes.Prelude as PP  (foldM)
import Pipes (for, runEffect, (>->))
import Pipes.HTTP

-- import OpenSSL (withOpenSSL)
-- import qualified OpenSSL as SSL
-- import qualified OpenSSL.Session as SSL

-- import Web.Cookie
import Web.VKHS2.Types

data LoginState = LoginState {
    ls_rights :: [AccessRight]
  -- ^ Access rights to be requested
  , ls_appid :: AppID
  -- ^ Application ID provided by vk.com
  , ls_formdata :: [(String,String)]
  -- ^ Dictionary containig inputID/value map for filling forms
  } deriving (Show)

defaultState = LoginState allAccess (AppID "3128877") []


newtype LoginT m a = LoginT { unLogin :: StateT LoginState m a }
  deriving(Functor, Applicative, Monad, MonadState LoginState, MonadIO)

-- FIXME: protect MonadIO instance
-- runLogin :: (Monad m) => LoginT m a -> m a
-- runLogin l = withOpenSSL (runStateT (unLogin l) defaultState)
runLogin l = (runStateT (unLogin l) defaultState)

liftLogin :: (Monad m) => m a -> LoginT m a
liftLogin = LoginT . lift


newtype Url = Url String
  deriving(Show,Eq,Ord)


data RobotAction = DoGET URI CookieJar | DoPOST
  deriving(Show,Eq)

buildQuery :: [(String,String)] -> String
buildQuery qis = "?" ++ intercalate "&" (map (\(a,b) -> (esc a) ++ "=" ++ (esc b)) qis) where
  esc x = escapeURIString isAllowedInURI x

initialAction :: (Monad m) => LoginT m RobotAction
initialAction = do
  LoginState{..} <- get
  return $ DoGET (
    URI ("https:") (Just (URIAuth "" "oauth.vk.com" "443")) "/authorize" (buildQuery [
        ("client_id", aid_string ls_appid)
      , ("scope", toUrlArg ls_rights)
      , ("redirect_url", "https://oauth.vk.com/blank.html")
      , ("display", "wap")
      , ("response_type", "token")
      , ("foo&foo","bar =bar")
      ]) []) (createCookieJar [])

-- renderCookiesText' :: CookiesText -> ByteString
-- renderCookiesText' = BB.toByteString . renderCookiesText

actionRequest :: (MonadIO m) => RobotAction -> LoginT m Request
actionRequest (DoGET url cookiejar) = do
  LoginState{..} <- get
  now <- liftIO $ getCurrentTime
  r <- either (error.show) return $ setUri def url
  (r,_) <- pure $ insertCookiesIntoRequest r cookiejar now
  return r


newtype VKT_ r m a = VKT_ { unVK :: ContT r (LoginT m) a }
  deriving(Functor, Applicative, Monad, MonadCont, MonadState LoginState)

liftCont :: ((a -> LoginT m r) -> LoginT m r) -> VKT_ r m a
liftCont f = VKT_ (ContT f)

data Error = ETimeout
  deriving(Show, Eq, Ord)

data VKResult m a =
    VKFine a
  | VKUnexpected Error (a -> VKT m a)

type VKT m a = VKT_ (VKResult m a) m a

raiseError :: (Monad m) => Error -> VKT m a
raiseError e = liftCont (\cont -> do
  return (VKUnexpected e (\x -> liftCont (\cont' -> do
    res <- cont x
    case res of
      VKFine a -> cont' a
      x -> return x))))

-- do_login :: VKT IO ()
-- do_login = do
--   undefined


{- Test -}

test_loop :: (Monad m) => (Error -> LoginT m a) -> VKT m a -> LoginT m a
test_loop fixer m = do
  res <- runContT (unVK m) (return . VKFine)
  case res of
    VKFine x -> return x
    VKUnexpected err cont -> fixer err >>= test_loop fixer . cont


-- test_login :: (MonadIO m) => LoginT m ()
-- test_login = do
--   a <- initialAction
--   r <- actionRequest a
--   liftIO $ do
--     ctx <- baselineContextSSL
--     c <- openConnectionSSL ctx "oauth.vk.com" 443
--     sendRequest c r emptyBody
--     receiveResponse c (\h b -> do
--       -- putStrLn (show h)
--       Streams.connect b stdout)
--     closeConnection c

test_login :: (MonadIO m) => LoginT m ()
test_login = do
  act <- initialAction
  req <- actionRequest act
  liftIO $ do
    putStrLn (show req)
    return ()
    -- m <- newManager tlsManagerSettings
    -- withHTTP req m $ \res -> do
    --   b <- PP.foldM (\a b -> return $ BS.append a b) (return BS.empty) return (responseBody res)
    --   BS.putStrLn b

-- test = serializeRelativeRef' $ RelativeRef {
--     rrAuthority = Nothing,
--     rrPath = "/foo",
--     rrQuery = Query {queryPairs = [("bar","baz")]},
--     rrFragment = Just "quux"
--     }
