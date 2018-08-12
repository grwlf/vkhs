{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | TODO: Rename to Coroutine.hs
module Web.VKHS.Error where

import qualified Web.VKHS.Client as Client
import qualified Text.HTML.TagSoup.Parsec as Tagsoup

import Web.VKHS.Types
import Web.VKHS.Imports

-- data Error = ETimeout | EClient Client.Error
--   deriving(Show, Eq)

-- | Message type used by the Superwiser to comminicatre with 'VK' coroutine.
--
-- See 'apiR' for usage example.
data CallRecovery =
    ReExec MethodName MethodArgs
  -- ^ VK program is to re-execute the method with the given parameters
  | ReParse JSON
  -- ^ VK program is to re-parse the JSON as if it was the result of API call in
  -- question
  deriving(Show)

-- | Alias for 'Result'
type R t a = Result t a

-- | Result of 'VK' monad execution. @t@ represents the continuation monad, which
-- needs to track two types: the early break @t@ and the current result @a@.
-- In order to be runnable (e.g. by 'runVK') both types are need to be the same.
--
--    * FIXME re-implement the concept using `Monad.Free` library
--    * FIMXE clean out of test/unused constructors
data Result t a =
    Fine a
  | APIFailed APIError
  | ExecuteAPI (MethodName, MethodArgs) (JSON -> t (R t a) (R t a))
  | UploadFile (HRef,FilePath) (UploadRecord -> t (R t a) (R t a))
  | APILogin (AccessToken -> t (R t a) (R t a))
  | APIMessage Verbosity Text (() -> t (R t a) (R t a))


type L t a = LoginRoutine t a

data LoginRoutine t a =
    LoginOK a
  | LoginFailed LoginError
  | LoginAskInput [Tagsoup.Tag String] Form String (String -> t (L t a) (L t a))
  | LoginMessage Verbosity Text (() -> t (L t a) (L t a))

