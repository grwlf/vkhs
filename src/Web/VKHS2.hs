{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.VKHS2 where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Cont

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import Network.Http.Client hiding (get)

import URI.ByteString (URI, RelativeRef, parseURI, strictURIParserOptions)
import URI.ByteString
import qualified URI.ByteString as URI

import qualified Blaze.ByteString.Builder           as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BB

import OpenSSL (withOpenSSL)
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL

import Web.Cookie
import Web.VKHS2.Types

data LoginState = LoginState {
    ls_rights :: [AccessRight]
  -- ^ Access rights to be requested
  , ls_clientId :: ClientID
  -- ^ Application ID provided by vk.com
  , ls_formdata :: [(String,String)]
  -- ^ Dictionary containig inputID/value map for filling forms
  } deriving (Show)

defaultState = LoginState allAccess (ClientID "0") []


newtype LoginT m a = LoginT { unLogin :: StateT LoginState m a }
  deriving(Functor, Applicative, Monad, MonadState LoginState, MonadIO)

liftLogin :: (Monad m) => m a -> LoginT m a
liftLogin = LoginT . lift


-- FIXME: protect MonadIO instance
runLogin s l = withOpenSSL (runStateT (unLogin l) s)

newtype Url = Url String
  deriving(Show,Eq,Ord)


data RobotAction = DoGET ByteString CookiesText | DoPOST
  deriving(Show,Eq,Ord)


initialAction :: (Monad m) => LoginT m RobotAction
initialAction = do
  LoginState{..} <- get
  return $ DoGET (serializeRelativeRef' (
    RelativeRef Nothing "/authorize" (Query [
        ("client_id", BS.pack (cid_string ls_clientId))
      , ("scope", BS.pack (toUrlArg ls_rights))
      , ("redirect_url", "https://oauth.vk.com/blank.html")
      , ("display", "wap")
      , ("response_type", "token")
      ]) Nothing)) []

renderCookiesText' :: CookiesText -> ByteString
renderCookiesText' = BB.toByteString . renderCookiesText

actionRequest :: (MonadIO m) => RobotAction -> LoginT m Request
actionRequest (DoGET url cookies) = do
  LoginState{..} <- get
  return $ buildRequest1 $ do
    http GET url
    when (not (null cookies)) $ do
      setHeader "Cookie" (renderCookiesText' cookies)


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


test_login :: (MonadIO m) => LoginT m ()
test_login = do
  a <- initialAction
  r <- actionRequest a
  liftIO $ do
    putStrLn "1"
    ctx <- baselineContextSSL
    c <- openConnectionSSL ctx "oauth.vk.com" 443
    sendRequest c r emptyBody
    receiveResponse c (\p i -> do
      Streams.connect i stdout)
    closeConnection c

test = serializeRelativeRef' $ RelativeRef {
    rrAuthority = Nothing,
    rrPath = "/foo",
    rrQuery = Query {queryPairs = [("bar","baz")]},
    rrFragment = Just "quux"
    }
