{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.VKHS where

import Data.List
import Data.Maybe
import Data.Time
import Data.Either
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Cont

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Web.VKHS.Types
import Web.VKHS.Monad

{- Test -}

test_loop :: (Monad m) => VKT a m a -> ClientT m (ResultDescription a)
test_loop m = do
  res <- runVK m
  case res of
    UnexpectedInt e k -> test_loop (k 0)
    _ -> return (describeResult res)


-- test_loop :: (Monad m) => (Error -> ClientT m a) -> VKT m a -> ClientT m a
-- test_loop fixer m = do
--   res <- runVK m
--   case res of
--     Fine x -> return x
--     Unexpected err cont -> fixer err >>= test_loop fixer . cont

-- test_login :: (MonadIO m) => ClientT m ()
-- test_login = do
--   act <- initialAction
--   req <- actionRequest act
--   liftIO $ do
--     putStrLn (show req)
--     return ()

