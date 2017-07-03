{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.VKHS.Error where

import Web.VKHS.Types
import Web.VKHS.Client (Response, Request, URL)
import qualified Web.VKHS.Client as Client
import Data.ByteString.Char8 (ByteString, unpack)

import Web.VKHS.Imports

data Error = ETimeout | EClient Client.Error
  deriving(Show, Eq)

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
  -- ^ The normal exit of a computation
  | UnexpectedInt Error (Int -> t (R t a) (R t a))
  -- ^ Invalid integer value. It is possible for client to set a correct URL and
  -- continue
  | UnexpectedBool Error (Bool -> t (R t a) (R t a))
  -- ^ Invalid boolean value. It is possible for client to set a correct URL and
  -- continue
  | UnexpectedURL Client.Error (URL -> t (R t a) (R t a))
  -- ^ Invalid URL. It is possible for client to set a correct URL and continue
  | UnexpectedRequest Client.Error (Request -> t (R t a) (R t a))
  | UnexpectedResponse Client.Error (Response -> t (R t a) (R t a))
  | UnexpectedFormField Form String (String -> t (R t a) (R t a))
  | LoginActionsExhausted
  | RepeatedForm Form (() -> t (R t a) (R t a))
  | JSONParseFailure ByteString (JSON -> t (R t a) (R t a))
  | JSONParseFailure' JSON String
  | JSONCovertionFailure (JSON, Text) (JSON -> t (R t a) (R t a))
  -- ^ Failed to convert JSON into Haskell object, Text describes an error.
  -- Superwiser may wish to replace the JSON with the correct one
  | LogError Text (() -> t (R t a) (R t a))
  | CallFailure (MethodName, MethodArgs, JSON, String) (CallRecovery -> t (R t a) (R t a))

data ResultDescription a =
    DescFine a
  | DescError String
  deriving(Show)

-- | A partial @Show@ for 'Result' class. Continuation parameters prevent it from be
-- instance of standard Show.
describeResult :: (Show a) => Result t a -> Text
describeResult (Fine a) = "Fine " <> tshow a
describeResult (UnexpectedInt e k) = "UnexpectedInt " <> (tshow e)
describeResult (UnexpectedBool e k) = "UnexpectedBool " <>  (tshow e)
describeResult (UnexpectedURL e k) = "UnexpectedURL " <> (tshow e)
describeResult (UnexpectedRequest e k) = "UnexpectedRequest " <> (tshow e)
describeResult LoginActionsExhausted = "LoginActionsExhausted"
describeResult (RepeatedForm f k) = "RepeatedForm"
describeResult (JSONParseFailure bs _) = "JSONParseFailure " <> (tshow bs)
describeResult (JSONParseFailure' JSON{..} s) = "JSONParseFailure' " <> (tshow s) <> " JSON: " <> (tpack $ take 1000 $ show js_aeson)
describeResult (LogError t k) = "LogError " <> (tshow t)
describeResult (JSONCovertionFailure j k) = "JSONConvertionFailure " <> (tshow j)
describeResult (CallFailure (n,args,j,err) k) = "CallFailure " <> tshow n <> " " <> tshow args

