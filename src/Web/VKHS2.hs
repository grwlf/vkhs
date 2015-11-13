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

data VKResult m =
    VKFine
  | VKUnexpected (VKT m (VKResult m))

type VKT m a = VKT_ (VKResult m) m a

raiseError :: (Monad m) => VKT m (VKResult m)
raiseError = liftCont $ \next -> do
  return (VKUnexpected (liftCont $ \next' -> next VKFine >>= next' ))


do_login :: VKT IO ()
do_login = do
  undefined


{- Test -}

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
