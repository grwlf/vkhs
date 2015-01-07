{-# LANGUAGE ScopedTypeVariables #-}

module Web.VKHS.Curl
  ( vk_curl
  , vk_curl_file
  , vk_curl_payload
  , pack
  , unpack
  ) where

import Data.IORef (newIORef, readIORef, atomicModifyIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)

import Control.Applicative
import Control.Exception (catch,bracket)
import Control.Concurrent (threadDelay)
import Control.Monad.Writer

import Network.CURL730

import Prelude hiding (catch)

import System.IO

import Web.VKHS.Types


pack = BS.pack . map BS.c2w
unpack = map BS.w2c . BS.unpack

-- | Generic request sender. Returns whole HTTP answer as a ByteString
vk_curl :: Env a -> Writer [CURLoption] () -> IO (Either String BS.ByteString)
vk_curl e w = do
    let askE x = return (x e)
    v <- askE verbose
    a <- askE useragent
    d <- askE delay_ms
    do
        buff <- newIORef BS.empty
        do {
          bracket (curl_easy_init) (curl_easy_cleanup) $ \curl ->
              let 
                  memwrite n = atomicModifyIORef buff
                      (\o -> (BS.append o n, CURL_WRITEFUNC_OK))
              in do
              curl_easy_setopt curl $
                  [ CURLOPT_HEADER         True
                  , CURLOPT_WRITEFUNCTION (Just $ memwrite)
                  , CURLOPT_SSL_VERIFYPEER False
                  , CURLOPT_USERAGENT a
                  , CURLOPT_VERBOSE (v == Debug )
                  ] ++ (execWriter w);
              curl_easy_perform curl;
              threadDelay (1000 * d); -- convert ms to us
              b <- readIORef buff ;
              return (Right b) ;
        } `catch`
            (\(e::CURLE) -> return $ Left ("CURL error: " ++ (show e)))

scanPattern pat s =
  let (_,o,x,n) = BS.foldl' check (False, BS.empty, BS.empty, BS.empty) s
  in (BS.reverse o, x, BS.reverse n)
  where 
    check (False, old, st, new) b
      | BS.length st < BS.length pat = (False, old, st`BS.snoc`b, new)
      | st == pat                    = (True, old, st, b`BS.cons`new) 
      | otherwise                    = (False, (BS.head st)`BS.cons`old, (BS.tail st)`BS.snoc`b, new)
    check (True, old, st, new) b     = (True, old, st, b`BS.cons`new)

cutheader :: BS.ByteString -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
cutheader buf new =
  let
  buf' = BS.append buf new
  overfill = BS.length buf' >= 1024
  (_,p,t) = scanPattern (BS.pack $ map BS.c2w "\r\n\r\n") buf'
  in case (overfill, BS.null t) of
    (True, True) -> Nothing
    (False, True) -> Just (buf', t)
    (_, False) -> Just (buf', t)

data State = Pending BS.ByteString | Working BS.ByteString | FailNoHeader

process :: State -> BS.ByteString -> State
process s@(Pending b) bs =
  case cutheader b bs of
    Nothing -> FailNoHeader
    Just (b', t)
      | BS.null t -> (Pending b')
      | otherwise -> (Working t)
process s@(Working _) bs = (Working bs)
process s _ = s

-- | Downloads the file and saves it on disk
vk_curl_file
  :: Env a
  -- ^ User environment
  -> String
  -- ^ URL
  -> (BS.ByteString -> IO ())
  -- ^ File write handler
  -> IO (Either String ())
vk_curl_file e url cb = do
    let askE x = return (x e)
    v <- askE verbose
    a <- askE useragent
    d <- askE delay_ms
    do
        sr <- newIORef =<< (Pending <$> pure BS.empty)

        bracket (curl_easy_init) (curl_easy_cleanup) $ \curl -> do {
            let 
            filewrite bs = do
              s' <- atomicModifyIORef sr (\s -> let s' = process s bs in (s',s'))
              case s' of
                FailNoHeader -> return CURL_WRITEFUNC_FAIL
                Pending b -> return CURL_WRITEFUNC_OK
                Working t -> do
                  cb t
                  return CURL_WRITEFUNC_OK 
            in
              curl_easy_setopt curl $
                [ CURLOPT_HEADER         True
                , CURLOPT_WRITEFUNCTION (Just $ filewrite)
                , CURLOPT_SSL_VERIFYPEER False
                , CURLOPT_USERAGENT a
                , CURLOPT_VERBOSE (v == Debug)
                , CURLOPT_URL url
                ]; 

            curl_easy_perform curl;
            threadDelay (1000 * d); -- convert ms to us
            s <- readIORef sr;
            case s of
              Working _ -> return $ Right ()
              _         -> return $ Left "HTTP header detection failure"
        } `catch`
            (\(e::CURLE) -> return $ Left ("CURL error: " ++ (show e)))


-- | Return HTTP payload, ignore headers
vk_curl_payload :: Env a -> Writer [CURLoption] () -> IO (Either String BS.ByteString)
vk_curl_payload e w = do
    let askE x = return (x e)
    v <- askE verbose
    a <- askE useragent
    d <- askE delay_ms
    do
        sr <- newIORef (BS.empty, Pending BS.empty)

        bracket (curl_easy_init) (curl_easy_cleanup) $ \curl -> do {
            let 
            writer bs = do
              atomicModifyIORef sr (\(buff,s) -> let s' = process s bs ; paired x =(x,x) in 
                case s' of
                  FailNoHeader -> paired (buff,s')
                  Pending b -> paired (buff,s')
                  Working t -> paired (BS.append buff t,s'))
              return CURL_WRITEFUNC_OK

            in
              curl_easy_setopt curl $
                [ CURLOPT_HEADER         True
                , CURLOPT_WRITEFUNCTION (Just $ writer)
                , CURLOPT_SSL_VERIFYPEER False
                , CURLOPT_USERAGENT a
                , CURLOPT_VERBOSE (v == Debug)
                ] ++ (execWriter w); 
            curl_easy_perform curl;
            threadDelay (1000 * d); -- convert ms to us
            (buff,s) <- readIORef sr;
            case s of
              Working _ -> return $ Right buff
              _         -> return $ Left "HTTP header detection failure"
        } `catch`
            (\(e::CURLE) -> return $ Left ("CURL error: " ++ (show e)))

