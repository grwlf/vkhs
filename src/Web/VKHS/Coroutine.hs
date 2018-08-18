{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | TODO: Rename to Coroutine.hs
module Web.VKHS.Coroutine where

import qualified Web.VKHS.Client as Client
import qualified Text.HTML.TagSoup.Parsec as Tagsoup

import Web.VKHS.Types
import Web.VKHS.Imports

-- | Alias for 'Result'
type R t a = APIRoutine t a

-- | Result of 'VK' monad execution. @t@ represents the continuation monad, which
-- needs to track two types: the early break @t@ and the current result @a@.
-- In order to be runnable (e.g. by 'runVK') both types are need to be the same.
--
-- FIXME * Re-implement the concept using `Monad.Free` library
-- FIMXE * Clean out of test/unused constructors
data APIRoutine t a =
    Fine a
  | APIFailed APIError
  | ExecuteAPI (MethodName, MethodArgs) (JSON -> t (R t a) (R t a))
  | UploadFile (HRef,FilePath) (UploadRecord -> t (R t a) (R t a))
  | APILogin (AccessToken -> t (R t a) (R t a))
  | APIMessage Verbosity Text (() -> t (R t a) (R t a))


-- | Alias for `LoginRoutine`
type L t a = LoginRoutine t a

data LoginRoutine t a =
    LoginOK a
  | LoginFailed LoginError
  | LoginAskInput [Tagsoup.Tag String] Form String (String -> t (L t a) (L t a))
  | LoginMessage Verbosity Text (() -> t (L t a) (L t a))

